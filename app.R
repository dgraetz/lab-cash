# =============================================================================
#  Lab Petty Cash Register  -  a small Shiny app
# =============================================================================
#
#  What it does
#  ------------
#  * Research assistants (RAs) record money they took out of the register.
#  * Everyone sees how much should be left in the register right now.
#  * The lab manager can "recount" the register: they type in how much cash
#    is actually in it, and that becomes the new starting amount.
#
#  How the numbers work
#  --------------------
#  Every recount starts a new "period" (1, 2, 3, ...).
#     balance = amount counted at the last recount
#               - sum of all payouts entered since that recount
#
#  Where the data lives
#  --------------------
#  Two tables, either as tabs in a Google Sheet or as CSV files in the "data"
#  folder (see storage.R):
#     payouts  - one row per payout
#     counts   - one row per recount by the lab manager
#
#  How to run it
#  -------------
#  In R:   shiny::runApp("path/to/register_tracker")
#
# =============================================================================

library(shiny)
library(bslib)


# ---- 1. Settings you may want to change -------------------------------------

# Password the lab manager types to unlock the recount panel.
# It is kept in the .Renviron file in this folder (not in the code), so it
# doesn't end up on GitHub. That file has one line:  REGISTER_MANAGER_PIN=1234
MANAGER_PIN <- Sys.getenv("REGISTER_MANAGER_PIN")

CURRENCY  <- "$"    # shown in front of every amount, e.g. "€" or "$"

# All dates and times are shown in this time zone. (shinyapps.io servers run on
# UTC, so without this the times would be 7-8 hours ahead.)
TIME_ZONE <- "America/Los_Angeles"   # Pacific time

# The current time / date in TIME_ZONE, e.g. "2026-11-15 17:45" / 2026-11-15
now_text <- function() format(Sys.time(), "%Y-%m-%d %H:%M", tz = TIME_ZONE)
today    <- function() as.Date(format(Sys.time(), "%Y-%m-%d", tz = TIME_ZONE))
LOW_SHARE <- 0.2    # warn when less than 20 % of the counted amount is left

# Where the data is saved (Google Sheet or CSV files) is set in storage.R.
source("storage.R")
PAYOUTS <- "payouts"   # name of the payouts table (sheet tab / CSV file)
COUNTS  <- "counts"    # name of the recounts table


# ---- 2. Helper functions for reading and writing the CSV files ---------------

# Create the two tables (with just their column names) the first time the app runs.
setup_tables <- function() {
  storage_create(PAYOUTS, c("id", "entered_at", "date", "amount", "period"))
  storage_create(COUNTS,  c("period", "counted_at", "amount", "expected", "note"))
}

# Read a table. Everything is read as text first, then the number columns are
# turned into numbers. (This keeps an empty table from breaking the sums.)
read_table <- function(name) {
  d <- storage_read(name)
  for (col in intersect(names(d), c("id", "amount", "expected", "period"))) {
    d[[col]] <- as.numeric(d[[col]])
  }
  d
}

# Format a number as money, e.g. 12.5 -> "$12.50"
money <- function(x) {
  sign <- ifelse(x < 0, "-", "")
  paste0(sign, CURRENCY, formatC(abs(x), format = "f", digits = 2, big.mark = ","))
}

# Work out the current state of the register from the two tables.
register_status <- function(payouts, counts) {
  if (nrow(counts) == 0) {
    return(list(set_up = FALSE))
  }
  last_count <- counts[nrow(counts), ]                  # most recent recount
  this_period <- payouts[payouts$period == last_count$period, ]
  spent <- sum(this_period$amount)

  list(
    set_up     = TRUE,
    period     = last_count$period,
    counted    = last_count$amount,
    counted_at = last_count$counted_at,
    spent      = spent,
    balance    = last_count$amount - spent,
    payouts    = this_period
  )
}

setup_tables()


# ---- 3. The user interface ---------------------------------------------------

ui <- page_fluid(
  theme = bs_theme(version = 5, primary = "#2f6b55",
                   base_font = font_google("IBM Plex Sans"),
                   code_font = font_google("IBM Plex Mono")),
  tags$head(tags$style("
    .balance   { font-family: 'IBM Plex Mono', monospace; font-size: 3rem;
                 font-weight: 600; line-height: 1.1; }
    .low       { color: #b4462b; }
    .muted     { color: #6b7570; }
    .container-fluid { max-width: 1000px; padding-block: 24px; }
    .app-header { display: flex; align-items: center; gap: 16px; margin-bottom: 16px; }
    .app-header h2, .app-header p { margin: 0; }
    .app-logo   { width: 96px; height: 96px; flex-shrink: 0; }
  ")),

  # Header: lab logo (from the www folder) next to the title
  div(class = "app-header",
    img(src = "logo.svg", alt = "Cognitive Dynamics Lab logo", class = "app-logo"),
    div(
      h2("Lab Cash"),
      p(class = "muted", "Record every payout from the register so the balance stays right.")
    )
  ),

  layout_columns(
    col_widths = c(5, 7),

    # Left: the balance
    card(
      card_header("In the register now"),
      uiOutput("balance_box")
    ),

    # Right: the form RAs use
    card(
      card_header("Record a payout"),
      numericInput("amount", paste0("Amount (", CURRENCY, ")"),
                   value = NA, min = 0, step = 0.01),
      dateInput("date", "Date paid out"),
      actionButton("add", "Record payout", class = "btn-primary")
    )
  ),

  card(
    card_header("Payouts since the last recount"),
    tableOutput("payout_table")
  ),

  # The lab manager panel, hidden behind a PIN
  accordion(
    open = FALSE,
    accordion_panel(
      "Lab manager: recount the register",
      uiOutput("manager_panel")
    ),
    accordion_panel(
      "Recount history",
      tableOutput("count_table")
    )
  )
)


# ---- 4. The server: what happens when people click things --------------------

server <- function(input, output, session) {

  # Re-read the data every few seconds (REFRESH_MS, set in storage.R). This
  # is what makes a payout entered on one computer show up on everyone
  # else's screen. refresh() lets us re-read right away after saving.
  refresh <- reactiveVal(0)
  payouts <- reactive({ refresh(); invalidateLater(REFRESH_MS); read_table(PAYOUTS) })
  counts  <- reactive({ refresh(); invalidateLater(REFRESH_MS); read_table(COUNTS) })

  status <- reactive(register_status(payouts(), counts()))

  # Start the date box on today's date (Pacific time) for each visitor
  updateDateInput(session, "date", value = today())

  # Has this browser tab entered the manager PIN?
  is_manager <- reactiveVal(FALSE)


  # -- 4a. Balance card -------------------------------------------------------
  output$balance_box <- renderUI({
    s <- status()
    if (!s$set_up) {
      return(p("The register has not been counted yet. The lab manager sets",
               "the starting amount in the panel below."))
    }
    is_low <- s$balance < LOW_SHARE * s$counted
    tagList(
      div(class = paste("balance", if (is_low) "low"), money(s$balance)),
      p(class = "muted",
        paste0("Counted ", money(s$counted), " on ", s$counted_at,
               ", ", money(s$spent), " paid out since.")),
      if (is_low) p(class = "low", strong("Running low. Tell the lab manager."))
    )
  })


  # -- 4b. Recording a payout -------------------------------------------------
  observeEvent(input$add, {
    # Read the files fresh (not the 1-second cached copy) so two people saving
    # at the same moment don't get the same payout number.
    all_payouts <- read_table(PAYOUTS)
    s <- register_status(all_payouts, read_table(COUNTS))

    # Check the form before saving anything
    problem <- NULL
    if (!s$set_up)                              problem <- "The register has not been counted yet."
    else if (is.na(input$amount) || input$amount <= 0)
                                                problem <- "Enter an amount above zero."

    if (!is.null(problem)) {
      showNotification(problem, type = "error")
      return()
    }

    new_row <- data.frame(
      id         = if (nrow(all_payouts) == 0) 1 else max(all_payouts$id) + 1,
      entered_at = now_text(),
      date       = format(input$date),
      amount     = round(input$amount, 2),
      period     = s$period
    )
    if (!storage_append(PAYOUTS, new_row)) return()
    refresh(refresh() + 1)

    showNotification(paste("Recorded", money(new_row$amount)), type = "message")
    updateNumericInput(session, "amount", value = NA)
  })


  # -- 4c. Table of payouts in the current period -----------------------------
  output$payout_table <- renderTable({
    s <- status()
    if (!s$set_up || nrow(s$payouts) == 0) {
      return(data.frame(Note = "No payouts since the last recount."))
    }
    p <- s$payouts[order(s$payouts$id, decreasing = TRUE), ]   # newest first
    data.frame(
      `#`      = as.integer(p$id),
      Date     = p$date,
      Amount   = money(p$amount),
      check.names = FALSE
    )
  }, striped = TRUE, width = "100%")


  # -- 4d. Lab manager panel --------------------------------------------------
  output$manager_panel <- renderUI({
    if (!is_manager()) {
      return(tagList(
        passwordInput("pin", "Manager PIN"),
        actionButton("unlock", "Unlock")
      ))
    }
    s <- status()
    tagList(
      if (s$set_up) p("The app expects ", strong(money(s$balance)),
                      " in the register. Count the cash and enter what is really there."),
      numericInput("counted", paste0("Cash in the register (", CURRENCY, ")"),
                   value = NA, min = 0, step = 0.01),
      textInput("count_note", "Note (optional)",
                placeholder = "e.g. topped up with 200 from lab budget"),
      actionButton("recount", "Save recount", class = "btn-warning"),
      hr(),
      h6("Fix a mistake"),
      numericInput("delete_id", "Delete payout number #", value = NA, min = 1),
      actionButton("delete", "Delete payout", class = "btn-outline-danger")
    )
  })

  observeEvent(input$unlock, {
    if (MANAGER_PIN == "") {
      showNotification("No manager PIN is set. Add REGISTER_MANAGER_PIN to the .Renviron file and restart R.",
                       type = "error", duration = 10)
    } else if (identical(input$pin, MANAGER_PIN)) {
      is_manager(TRUE)
    } else {
      showNotification("Wrong PIN.", type = "error")
    }
  })

  # Saving a recount starts a new period with the counted amount
  observeEvent(input$recount, {
    req(is_manager())
    if (is.na(input$counted) || input$counted < 0) {
      showNotification("Enter the amount you counted (0 or more).", type = "error")
      return()
    }
    s <- register_status(read_table(PAYOUTS), read_table(COUNTS))
    new_row <- data.frame(
      period     = if (s$set_up) s$period + 1 else 1,
      counted_at = now_text(),
      amount     = round(input$counted, 2),
      expected   = if (s$set_up) s$balance else NA,
      note       = trimws(input$count_note)
    )
    if (!storage_append(COUNTS, new_row)) return()
    refresh(refresh() + 1)
    showNotification(paste("Register set to", money(new_row$amount)), type = "message")
    updateNumericInput(session, "counted", value = NA)
    updateTextInput(session, "count_note", value = "")
  })

  # Deleting rewrites the payouts table without that one row
  observeEvent(input$delete, {
    req(is_manager())
    all_payouts <- read_table(PAYOUTS)
    if (is.na(input$delete_id) || !(input$delete_id %in% all_payouts$id)) {
      showNotification("No payout with that number.", type = "error")
      return()
    }
    kept <- all_payouts[all_payouts$id != input$delete_id, ]
    if (!storage_replace(PAYOUTS, kept)) return()
    refresh(refresh() + 1)
    showNotification(paste0("Deleted payout #", input$delete_id), type = "message")
    updateNumericInput(session, "delete_id", value = NA)
  })


  # -- 4e. History of recounts ------------------------------------------------
  output$count_table <- renderTable({
    c <- counts()
    if (nrow(c) == 0) return(data.frame(Note = "No recounts yet."))
    c <- c[nrow(c):1, ]                                        # newest first
    data.frame(
      When       = c$counted_at,
      Counted    = money(c$amount),
      Expected   = ifelse(is.na(c$expected), "-", money(c$expected)),
      Difference = ifelse(is.na(c$expected), "-", money(c$amount - c$expected)),
      Note       = ifelse(is.na(c$note), "", c$note)
    )
  }, striped = TRUE, width = "100%")
}


shinyApp(ui, server)
