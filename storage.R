# =============================================================================
#  storage.R  -  where the register data is saved
# =============================================================================
#
#  Both app.R and app_bills.R load this file with source("storage.R").
#  It can save the data in one of two places:
#
#   * A Google Sheet  - use this on shinyapps.io. The app there is restarted
#                       often and loses its own files, but the sheet stays.
#   * CSV files       - in the "data" folder. Fine for running the app on your
#                       own computer or on a server that keeps its files.
#
#  If no sheet ID is set in .Renviron (see SHEET_ID below), the CSV files are used.
#
#  Each table (payouts, counts, ...) is one tab in the sheet, or one CSV file.
#  The apps only use the four storage_ functions at the bottom of this file,
#  so they don't need to know which of the two is in use.
#
#  Setting up the Google Sheet: see GOOGLE_SETUP.md in this folder.
# =============================================================================


# ---- Settings ----------------------------------------------------------------

# The ID of your Google Sheet: the long part of its web address between
# "/d/" and "/edit". It is kept in the .Renviron file in this folder (not in
# the code), as a line like:  REGISTER_SHEET_ID=1AbC...xyz
# If that line is missing, CSV files are used instead.
SHEET_ID <- Sys.getenv("REGISTER_SHEET_ID")

# The key file for the Google service account (see GOOGLE_SETUP.md).
KEY_FILE <- "google-key.json"

DATA_DIR <- "data"   # where the CSV files go when no sheet is set


# ---- Setup -------------------------------------------------------------------

USE_SHEETS <- nzchar(SHEET_ID)

# How often (in milliseconds) each open app re-reads the data to pick up
# other people's entries. Google limits how often a sheet can be read, so
# with a sheet we check every 15 seconds; CSV files can be read every second.
REFRESH_MS <- if (USE_SHEETS) 15000 else 1000

if (USE_SHEETS) {
  library(googlesheets4)
  # Log in to Google as the service account. On Hugging Face the key file's
  # contents come from the REGISTER_GOOGLE_KEY secret instead of the file.
  key_json <- Sys.getenv("REGISTER_GOOGLE_KEY")
  gs4_auth(path = if (nzchar(key_json)) key_json else KEY_FILE)
} else {
  dir.create(DATA_DIR, showWarnings = FALSE)
}

csv_path <- function(name) file.path(DATA_DIR, paste0(name, ".csv"))

# Run a Google call; if it fails (e.g. a network hiccup), wait 2 s and try once more.
try_twice <- function(expr_fn) {
  tryCatch(expr_fn(), error = function(e) {
    Sys.sleep(2)
    expr_fn()
  })
}


# ---- The four functions the apps use -----------------------------------------

# The names of the sheet's tabs, looked up once (saves a Google request per table at startup)
existing_tabs <- NULL

# Make the table with just its column names, if it doesn't exist yet.
storage_create <- function(name, columns) {
  if (USE_SHEETS) {
    if (is.null(existing_tabs)) existing_tabs <<- sheet_names(SHEET_ID)
    if (!(name %in% existing_tabs)) {
      sheet_add(SHEET_ID, sheet = name)
      header <- as.data.frame(t(columns))          # one row: the column names
      range_write(SHEET_ID, header, sheet = name, col_names = FALSE)
      existing_tabs <<- c(existing_tabs, name)
    }
  } else if (!file.exists(csv_path(name))) {
    writeLines(paste0('"', columns, '"', collapse = ","), csv_path(name))
  }
}

# Read several whole tables at once (with a Google Sheet: in ONE request,
# which is much faster than one table at a time). Every column comes back as
# text. Returns a list of data frames, named like the tables.
storage_read_many <- function(names) {
  if (USE_SHEETS) {
    tables <- try_twice(function() read_tabs_at_once(names))
  } else {
    tables <- lapply(names, function(name)
      read.csv(csv_path(name), stringsAsFactors = FALSE, colClasses = "character"))
  }
  # Skip rows that are completely empty. They appear when someone clears rows in
  # the Google Sheet instead of deleting them, and would otherwise break the app.
  tables <- lapply(tables, function(d) d[rowSums(!is.na(d) & d != "") > 0, , drop = FALSE])
  setNames(tables, names)
}

# Read one whole table
storage_read <- function(name) storage_read_many(name)[[1]]

# Ask Google for several tabs in a single request ("values:batchGet") and turn
# each into a data frame: the first row holds the column names, empty cells
# become NA, and everything is text (as it's shown in the sheet).
read_tabs_at_once <- function(names) {
  ranges <- setNames(as.list(paste0("'", names, "'")), rep("ranges", length(names)))
  url <- paste0("https://sheets.googleapis.com/v4/spreadsheets/", SHEET_ID, "/values:batchGet")
  response <- httr::GET(url, query = ranges, gs4_token(), httr::timeout(20))
  answer <- gargle::response_process(response)
  lapply(answer$valueRanges, function(range) {
    rows <- lapply(range$values, function(r) vapply(r, as.character, character(1)))
    header <- rows[[1]]
    body <- rows[-1]
    cells <- lapply(body, function(r) { r <- c(r, rep(NA, length(header) - length(r)))[seq_along(header)]; r[r == ""] <- NA; r })
    d <- as.data.frame(matrix(unlist(cells), ncol = length(header), byrow = TRUE,
                              dimnames = list(NULL, header)), stringsAsFactors = FALSE)
    if (length(body) == 0) d <- as.data.frame(setNames(replicate(length(header), character(), simplify = FALSE), header))
    d
  })
}

# Add one row (a one-row data frame) to the end of a table.
# Returns TRUE if it worked, FALSE (and shows a message) if it didn't.
storage_append <- function(name, row) {
  saved <- tryCatch({
    if (USE_SHEETS) {
      try_twice(function() sheet_append(SHEET_ID, row, sheet = name))
    } else {
      write.table(row, csv_path(name), sep = ",", append = TRUE,
                  col.names = FALSE, row.names = FALSE, qmethod = "double")
    }
    TRUE
  }, error = function(e) FALSE)

  if (!saved) {
    showNotification("Couldn't save. Check the internet connection and try again.",
                     type = "error")
  }
  storage_forget(name)          # the shared copy is out of date now
  saved
}

# Replace a whole table with a data frame (used when deleting a row).
# Returns TRUE if it worked, FALSE (and shows a message) if it didn't.
storage_replace <- function(name, data) {
  saved <- tryCatch({
    if (USE_SHEETS) {
      # Empty the tab, write the column names, then add the rows back
      try_twice(function() range_clear(SHEET_ID, sheet = name))
      header <- as.data.frame(t(names(data)))
      try_twice(function() range_write(SHEET_ID, header, sheet = name, col_names = FALSE))
      if (nrow(data) > 0) try_twice(function() sheet_append(SHEET_ID, data, sheet = name))
    } else {
      write.csv(data, csv_path(name), row.names = FALSE)
    }
    TRUE
  }, error = function(e) FALSE)

  if (!saved) {
    showNotification("Couldn't save. Check the internet connection and try again.",
                     type = "error")
  }
  storage_forget(name)          # the shared copy is out of date now
  saved
}


# ---- One shared copy of the data for everyone --------------------------------
#
# Google allows only a limited number of reads per minute. If every open
# browser tab read the sheet itself, a few open tabs would hit that limit.
# So the app keeps ONE copy of each table in memory, shared by everyone who
# has the app open, and re-reads it from the sheet at most once every
# REFRESH_MS. If Google can't be reached, everyone keeps seeing the last copy.
#
# Use storage_read_shared() for showing data, and storage_read() (always
# fresh) right before saving something.

shared_copy <- new.env()

# Tables that are always refreshed together, in one request (set by the app
# with storage_share(), e.g. the payouts and the recounts)
shared_tables <- character()
storage_share <- function(names) shared_tables <<- union(shared_tables, names)

storage_read_shared <- function(name) {
  copy <- shared_copy[[name]]
  age_ms <- if (is.null(copy)) Inf else
    as.numeric(difftime(Sys.time(), copy$time, units = "secs")) * 1000
  if (age_ms < REFRESH_MS - 500) return(copy$data)     # still fresh enough

  names <- union(name, shared_tables)
  data <- tryCatch(storage_read_many(names), error = function(e) NULL)
  if (is.null(data)) {
    if (!is.null(copy)) return(copy$data)            # Google failed: keep the last copy
    stop("The data couldn't be loaded. Please reload the page in a minute.")
  }
  now <- Sys.time()
  for (n in names) shared_copy[[n]] <- list(data = data[[n]], time = now)
  data[[name]]
}

# Throw away the shared copy of a table, so the next read gets it fresh
storage_forget <- function(name) {
  if (exists(name, envir = shared_copy, inherits = FALSE)) rm(list = name, envir = shared_copy)
}


# ---- Button clicks that can't crash the page ---------------------------------
#
# If reading or saving fails in the middle of a button click (e.g. Google is
# slow or over its limit), Shiny would normally close that person's page.
# Wrapping the click's code in try_or_tell() shows a message instead.
#   what - what the click was doing, e.g. "save the payout"
try_or_tell <- function(what, expr) {
  tryCatch(expr, error = function(e) {
    warning("Couldn't ", what, ": ", conditionMessage(e))
    showNotification(paste0("Couldn't ", what, ", because the data couldn't be reached. ",
                            "Nothing was changed. Please try again in a minute."),
                     type = "error", duration = 10)
    invisible(NULL)
  })
}
