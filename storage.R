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
  gs4_auth(path = KEY_FILE)        # log in to Google as the service account
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

# Make the table with just its column names, if it doesn't exist yet.
storage_create <- function(name, columns) {
  if (USE_SHEETS) {
    if (!(name %in% sheet_names(SHEET_ID))) {
      sheet_add(SHEET_ID, sheet = name)
      header <- as.data.frame(t(columns))          # one row: the column names
      range_write(SHEET_ID, header, sheet = name, col_names = FALSE)
    }
  } else if (!file.exists(csv_path(name))) {
    writeLines(paste0('"', columns, '"', collapse = ","), csv_path(name))
  }
}

# Read a whole table. Every column comes back as text.
storage_read <- function(name) {
  if (USE_SHEETS) {
    d <- try_twice(function() read_sheet(SHEET_ID, sheet = name, col_types = "c"))
    as.data.frame(d)
  } else {
    read.csv(csv_path(name), stringsAsFactors = FALSE, colClasses = "character")
  }
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
  saved
}
