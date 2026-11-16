# =============================================================================
#  Lab Cash Register (bills version)  -  a small Shiny app
# =============================================================================
#
#  What it does
#  ------------
#  * Research assistants (RAs) click + / - for each bill they took out of the
#    register ($20, $10, $5, $1). The app adds up the total.
#  * Everyone sees how many of each bill should be left, and the total.
#  * The lab manager "recounts" the register: they enter how many of each bill
#    are actually in it, and that becomes the new starting point.
#
#  How the numbers work
#  --------------------
#  Every recount starts a new "period" (1, 2, 3, ...).
#  For each bill type:
#     bills left = bills counted at the last recount
#                  - bills paid out since that recount
#  The balance is the bills left multiplied by their value, added up.
#
#  Where the data lives
#  --------------------
#  Two tables, either as tabs in a Google Sheet or as CSV files in the "data"
#  folder (see storage.R). Each bill type has its own column: n20 = number of
#  $20 bills, n10 = number of $10 bills, and so on. (They have their own names
#  so they don't mix with the tables of the simpler app.R.)
#     bills_payouts  - one row per payout
#     bills_counts   - one row per recount by the lab manager
#
#  How to run it
#  -------------
#  Open register_tracker.Rproj in RStudio, open app_bills.R, click "Run App".
#  Or in the R console:   shiny::runApp("app_bills.R")
#
# =============================================================================

library(shiny)
library(bslib)


# ---- 1. Settings you may want to change -------------------------------------

# Password the lab manager types to unlock the recount panel.
# It is kept in the .Renviron file in this folder (not in the code), so it
# doesn't end up on GitHub. That file has one line:  REGISTER_MANAGER_PIN=1234
MANAGER_PIN <- Sys.getenv("REGISTER_MANAGER_PIN")

CURRENCY  <- "$"              # shown in front of every amount

# All dates and times are shown in this time zone. (shinyapps.io servers run on
# UTC, so without this the times would be 7-8 hours ahead.)
TIME_ZONE <- "America/Los_Angeles"   # Pacific time

# The current time / date in TIME_ZONE, e.g. "2026-11-15 17:45" / 2026-11-15
now_text <- function() format(Sys.time(), "%Y-%m-%d %H:%M", tz = TIME_ZONE)
today    <- function() as.Date(format(Sys.time(), "%Y-%m-%d", tz = TIME_ZONE))
BILLS     <- c(20, 10, 5, 1)  # the bill types in the register, largest first
LOW_SHARE <- 0.2              # warn when less than 20 % of the counted total is left

# Where the data is saved (Google Sheet or CSV files) is set in storage.R.
source("storage.R")
PAYOUTS <- "bills_payouts"   # name of the payouts table (sheet tab / CSV file)
COUNTS  <- "bills_counts"    # name of the recounts table

# Column names for the bill counts: "n20", "n10", "n5", "n1"
BILL_COLS <- paste0("n", BILLS)


# ---- 2. Helper functions -----------------------------------------------------

# Create the two tables (with just their column names) the first time the app runs.
setup_tables <- function() {
  storage_create(PAYOUTS, c("id", "entered_at", "date", BILL_COLS, "amount", "period"))
  storage_create(COUNTS,  c("period", "counted_at", BILL_COLS, "amount", "expected", "note"))
}

# Read a table. Everything is read as text first, then the number columns are
# turned into numbers. (This keeps an empty table from breaking the sums.)
read_table <- function(name) {
  d <- storage_read(name)
  number_cols <- c("id", "amount", "expected", "period", BILL_COLS)
  for (col in intersect(names(d), number_cols)) {
    d[[col]] <- as.numeric(d[[col]])
  }
  d
}

# Format a number as money, e.g. 35 -> "$35"
money <- function(x) {
  sign <- ifelse(x < 0, "-", "")
  paste0(sign, CURRENCY, formatC(abs(x), format = "d", big.mark = ","))
}

# Total value of a set of bill counts, e.g. c(1, 0, 1, 2) -> 20 + 5 + 2 = 27
bills_value <- function(counts) {
  sum(counts * BILLS)
}

# Short text for a set of bill counts, e.g. "1 × $20, 2 × $1"
bills_text <- function(counts) {
  used <- counts > 0
  if (!any(used)) return("-")
  paste0(counts[used], " × ", CURRENCY, BILLS[used], collapse = ", ")
}

# Work out the current state of the register from the two tables.
register_status <- function(payouts, counts) {
  if (nrow(counts) == 0) {
    return(list(set_up = FALSE))
  }
  last_count  <- counts[nrow(counts), ]                  # most recent recount
  this_period <- payouts[payouts$period == last_count$period, ]

  counted  <- unlist(last_count[BILL_COLS])              # bills at the recount
  paid_out <- colSums(this_period[BILL_COLS])            # bills taken since
  left     <- counted - paid_out                         # bills that should be there

  list(
    set_up     = TRUE,
    period     = last_count$period,
    counted_at = last_count$counted_at,
    counted    = counted,
    paid_out   = paid_out,
    left       = left,
    total      = bills_value(counted),
    spent      = bills_value(paid_out),
    balance    = bills_value(left),
    payouts    = this_period
  )
}

setup_tables()


# ---- 3. The user interface ---------------------------------------------------

# One row of the payout form: "$20   [-]  0  [+]"
bill_stepper <- function(bill) {
  div(class = "stepper",
    span(class = "bill-label", paste0(CURRENCY, bill)),
    actionButton(paste0("minus_", bill), "−", class = "btn-outline-secondary btn-step"),
    span(class = "bill-count", textOutput(paste0("count_", bill), inline = TRUE)),
    actionButton(paste0("plus_", bill), "+", class = "btn-outline-secondary btn-step")
  )
}

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
    .stepper   { display: flex; align-items: center; gap: 12px; margin-bottom: 8px; }
    .bill-label{ font-family: 'IBM Plex Mono', monospace; font-weight: 600;
                 width: 3.5rem; }
    .bill-count{ font-family: 'IBM Plex Mono', monospace; font-size: 1.25rem;
                 width: 2.5rem; text-align: center; }
    .btn-step  { width: 2.5rem; font-size: 1.1rem; padding: 2px 0; }
    .payout-total { font-family: 'IBM Plex Mono', monospace; font-size: 1.5rem;
                    font-weight: 600; margin-block: 8px; }
    .bill-table td, .bill-table th { padding: 2px 12px 2px 0;
                 font-family: 'IBM Plex Mono', monospace; }
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
      p(class = "muted", "Click + for each bill you took out."),
      lapply(BILLS, bill_stepper),
      div(class = "payout-total", textOutput("payout_total", inline = TRUE)),
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
      "Lab manager: Reset the register",
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

  # The bills currently picked in the payout form, e.g. c(1, 0, 1, 0)
  picked <- reactiveVal(setNames(rep(0, length(BILLS)), BILL_COLS))


  # -- 4a. Balance card -------------------------------------------------------
  output$balance_box <- renderUI({
    s <- status()
    if (!s$set_up) {
      return(p("The register has not been counted yet. The lab manager enters",
               "the bills in the panel below."))
    }
    is_low <- s$balance < LOW_SHARE * s$total

    # Small table: one row per bill type. A negative count means more of that
    # bill was paid out than the register should have had; it's shown in red.
    rows <- lapply(seq_along(BILLS), function(i) {
      negative <- s$left[i] < 0
      tags$tr(class = if (negative) "low",
        tags$td(paste0(CURRENCY, BILLS[i])),
        tags$td(paste0("× ", s$left[i])),
        tags$td(class = if (!negative) "muted", money(BILLS[i] * s$left[i]))
      )
    })
    any_negative <- any(s$left < 0)

    tagList(
      div(class = paste("balance", if (is_low) "low"), money(s$balance)),
      tags$table(class = "bill-table", tags$tbody(rows)),
      p(class = "muted",
        paste0("Counted ", money(s$total), " on ", s$counted_at,
               ", ", money(s$spent), " paid out since.")),
      if (is_low) p(class = "low", strong("Running low. Tell the lab manager.")),
      if (any_negative) p(class = "low",
        "Red rows: more bills were paid out than were counted. The register needs a recount.")
    )
  })


  # -- 4b. The + / - buttons --------------------------------------------------
  # For each bill type, make one observer for "+" and one for "-".
  lapply(seq_along(BILLS), function(i) {
    bill <- BILLS[i]

    observeEvent(input[[paste0("plus_", bill)]], {
      p <- picked()
      p[i] <- p[i] + 1
      picked(p)
    })

    observeEvent(input[[paste0("minus_", bill)]], {
      p <- picked()
      p[i] <- max(0, p[i] - 1)        # never go below zero
      picked(p)
    })

    output[[paste0("count_", bill)]] <- renderText(picked()[i])
  })

  output$payout_total <- renderText(paste("Total:", money(bills_value(picked()))))


  # -- 4c. Recording a payout -------------------------------------------------
  observeEvent(input$add, {
    # Read the files fresh (not the 1-second cached copy) so two people saving
    # at the same moment don't get the same payout number.
    all_payouts <- read_table(PAYOUTS)
    s <- register_status(all_payouts, read_table(COUNTS))
    p <- picked()

    # Check before saving anything
    problem <- NULL
    if (!s$set_up) {
      problem <- "The register has not been counted yet."
    } else if (sum(p) == 0) {
      problem <- "Click + for the bills you took out."
    }

    if (!is.null(problem)) {
      showNotification(problem, type = "error")
      return()
    }

    # Taking more of a bill than the register should have is allowed (the
    # records may be off), but we warn so the lab manager knows to recount.
    if (any(p > s$left)) {
      short <- which(p > s$left)[1]
      showNotification(
        paste0("Heads up: the register should only have had ", max(s$left[short], 0),
               " × ", CURRENCY, BILLS[short],
               ". The payout is saved, but please tell the lab manager to recount."),
        type = "warning", duration = 10)
    }

    new_row <- data.frame(
      id         = if (nrow(all_payouts) == 0) 1 else max(all_payouts$id) + 1,
      entered_at = now_text(),
      date       = format(input$date),
      as.list(p),                       # the n20, n10, n5, n1 columns
      amount     = bills_value(p),
      period     = s$period
    )
    if (!storage_append(PAYOUTS, new_row)) return()
    refresh(refresh() + 1)

    showNotification(paste("Recorded", money(new_row$amount)), type = "message")
    picked(setNames(rep(0, length(BILLS)), BILL_COLS))   # clear the form
  })


  # -- 4d. Table of payouts in the current period -----------------------------
  output$payout_table <- renderTable({
    s <- status()
    if (!s$set_up || nrow(s$payouts) == 0) {
      return(data.frame(Note = "No payouts since the last recount."))
    }
    p <- s$payouts[order(s$payouts$id, decreasing = TRUE), ]   # newest first
    data.frame(
      `#`    = as.integer(p$id),
      Date   = p$date,
      Bills  = apply(p[BILL_COLS], 1, bills_text),
      Amount = money(p$amount),
      check.names = FALSE
    )
  }, striped = TRUE, width = "100%")


  # -- 4e. Lab manager panel --------------------------------------------------
  output$manager_panel <- renderUI({
    if (!is_manager()) {
      return(tagList(
        passwordInput("pin", "Manager PIN"),
        actionButton("unlock", "Unlock")
      ))
    }
    # One number box per bill type, side by side
    count_boxes <- lapply(BILLS, function(bill) {
      numericInput(paste0("counted_", bill), paste0(CURRENCY, bill, " bills"),
                   value = NA, min = 0, step = 1)
    })
    tagList(
      uiOutput("expected_text"),
      do.call(layout_columns, c(list(col_widths = rep(3, length(BILLS))), count_boxes)),
      div(class = "payout-total", textOutput("counted_total", inline = TRUE)),
      textInput("count_note", "Note (optional)",
                placeholder = "e.g. topped up with 200 from lab budget"),
      actionButton("recount", "Save recount", class = "btn-warning"),
      hr(),
      h6("Fix a mistake"),
      numericInput("delete_id", "Delete payout number #", value = NA, min = 1),
      actionButton("delete", "Delete payout", class = "btn-outline-danger")
    )
  })

  output$expected_text <- renderUI({
    s <- status()
    if (!s$set_up) return(p("Count each type of bill and enter how many there are."))
  })

  # The bill counts the manager typed in. Empty boxes count as 0.
  counted_bills <- reactive({
    n <- sapply(BILLS, function(bill) {
      v <- input[[paste0("counted_", bill)]]
      if (is.null(v) || is.na(v)) 0 else v
    })
    setNames(n, BILL_COLS)
  })

  output$counted_total <- renderText(paste("Total:", money(bills_value(counted_bills()))))

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

  # Saving a recount starts a new period with the counted bills
  observeEvent(input$recount, {
    req(is_manager())
    n <- counted_bills()
    if (any(n < 0) || any(n != round(n))) {
      showNotification("Bill counts must be whole numbers, 0 or more.", type = "error")
      return()
    }
    s <- register_status(read_table(PAYOUTS), read_table(COUNTS))
    new_row <- data.frame(
      period     = if (s$set_up) s$period + 1 else 1,
      counted_at = now_text(),
      as.list(n),                       # the n20, n10, n5, n1 columns
      amount     = bills_value(n),
      expected   = if (s$set_up) s$balance else NA,
      note       = trimws(input$count_note)
    )
    if (!storage_append(COUNTS, new_row)) return()
    refresh(refresh() + 1)
    showNotification(paste("Register set to", money(new_row$amount)), type = "message")
    for (bill in BILLS) updateNumericInput(session, paste0("counted_", bill), value = NA)
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


  # -- 4f. History of recounts ------------------------------------------------
  output$count_table <- renderTable({
    c <- counts()
    if (nrow(c) == 0) return(data.frame(Note = "No recounts yet."))
    c <- c[nrow(c):1, ]                                        # newest first
    data.frame(
      When       = c$counted_at,
      Bills      = apply(c[BILL_COLS], 1, bills_text),
      Counted    = money(c$amount),
      Expected   = ifelse(is.na(c$expected), "-", money(c$expected)),
      Difference = ifelse(is.na(c$expected), "-", money(c$amount - c$expected)),
      Note       = ifelse(is.na(c$note), "", c$note)
    )
  }, striped = TRUE, width = "100%")
}


shinyApp(ui, server)
