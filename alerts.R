# =============================================================================
#  alerts.R  -  optional email to the lab manager when the register runs low
# =============================================================================
#
#  Both app.R and app_bills.R load this file with source("alerts.R").
#
#  How it works
#  ------------
#  * In the manager panel, the lab manager can turn on low-balance emails,
#    enter who gets them, and set limits: for the total ("email me when it
#    drops below $50") and, in the bills version, for each bill type ("email
#    me when 20 or fewer $1 bills are left"). Every limit is optional.
#  * When a payout breaks one of the limits, the app sends ONE email. Each
#    limit is emailed about only once until the next recount, so nobody gets
#    an email for every payout while the register is low.
#  * The settings are saved like the rest of the data (Google Sheet or CSV),
#    so they survive restarts.
#
#  Sending email
#  -------------
#  The app doesn't log in to an email account. Instead, a small Google Apps
#  Script on the lab's Google account sends the email for it (the script is in
#  mail_script.gs, setup in EMAIL_SETUP.md). The app sends the script the
#  address, subject and text, plus a secret code so nobody else can use it.
#  The script's web address and the secret go in .Renviron (never in the code):
#     REGISTER_MAIL_URL=https://script.google.com/macros/s/.../exec
#     REGISTER_MAIL_SECRET=...
#  Without these two lines everything else still works; the manager panel just
#  says that email isn't set up.
# =============================================================================


# ---- Where to send the email request (read from .Renviron) -------------------

MAIL_URL    <- Sys.getenv("REGISTER_MAIL_URL")
MAIL_SECRET <- Sys.getenv("REGISTER_MAIL_SECRET")

EMAIL_READY <- nzchar(MAIL_URL) && nzchar(MAIL_SECRET)

# The columns of the alert-settings table (it only ever has one row).
#   total_on       - TRUE: email when the total drops below `below`
#   bills_on       - TRUE: email when a bill type is at or below its limit
#                    (only in the bills version)
#   below_n20, ... - the limit for each bill type (empty = no limit for that bill)
#   sent_period    - the period in which emails were last sent
#   sent           - which alerts were already sent in that period, e.g. "balance,n1"
alert_columns <- function(bill_cols = character()) {
  c("email", "total_on", "below", "bills_on", paste0("below_", bill_cols), "sent_period", "sent")
}


# ---- Reading and saving the alert settings -----------------------------------

read_alert_settings <- function(table, bill_cols = character()) {
  d <- storage_read(table)
  # One value from the settings row, or NA if it's missing
  get <- function(col) if (nrow(d) > 0 && col %in% names(d)) d[[col]][1] else NA
  num <- function(col) suppressWarnings(as.numeric(get(col)))

  below      <- num("below")
  bill_below <- vapply(bill_cols, function(b) num(paste0("below_", b)), numeric(1))
  email      <- get("email")

  # Settings saved by the older version had one "enabled" box for everything
  old_enabled <- identical(get("enabled"), "TRUE")
  total_on <- if ("total_on" %in% names(d)) identical(get("total_on"), "TRUE") else old_enabled && !is.na(below)
  bills_on <- if ("bills_on" %in% names(d)) identical(get("bills_on"), "TRUE") else old_enabled && any(!is.na(bill_below))

  sent <- get("sent")
  list(
    email       = if (is.na(email)) "" else email,
    total_on    = total_on,
    below       = below,
    bills_on    = length(bill_cols) > 0 && bills_on,
    bill_below  = bill_below,
    sent_period = num("sent_period"),
    sent        = if (is.na(sent) || sent == "") character() else strsplit(sent, ",")[[1]]
  )
}

save_alert_settings <- function(table, settings) {
  row <- data.frame(email    = settings$email,
                    total_on = as.character(settings$total_on),
                    below    = settings$below,
                    bills_on = as.character(settings$bills_on))
  for (b in names(settings$bill_below)) row[[paste0("below_", b)]] <- settings$bill_below[[b]]
  row$sent_period <- settings$sent_period
  row$sent        <- paste(settings$sent, collapse = ",")
  storage_replace(table, row)
}

# Split "a@x.org, b@y.org" into c("a@x.org", "b@y.org")
split_emails <- function(text) {
  e <- trimws(strsplit(text, ",")[[1]])
  e[nzchar(e)]
}

looks_like_email <- function(x) grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", x)


# ---- Sending -----------------------------------------------------------------

# Ask the Google Apps Script to send one plain-text email.
# Returns TRUE if it was sent, FALSE if not.
# With dry_run = TRUE the script only checks the secret and sends nothing.
send_email <- function(to, subject, body, dry_run = FALSE) {
  if (!EMAIL_READY) return(FALSE)
  tryCatch({
    response <- httr::POST(
      MAIL_URL,
      body = list(secret  = MAIL_SECRET,
                  to      = paste(to, collapse = ","),
                  subject = subject,
                  body    = body,
                  dry_run = dry_run),
      encode = "json",
      httr::timeout(20)
    )
    answer <- httr::content(response, as = "parsed", type = "application/json")
    if (!isTRUE(answer$ok)) stop(answer$error)
    TRUE
  }, error = function(e) {
    warning("Email not sent: ", conditionMessage(e))
    FALSE
  })
}


# ---- Checking the alert rules after a payout ----------------------------------

# Call this right after a payout is saved.
#   table    - name of the alert-settings table
#   balance  - the balance after the payout
#   period   - the current period (changes with every recount)
#   left     - bills version only: how many of each bill are left after the
#              payout, named like c(n20 = 3, n10 = 2, n5 = 0, n1 = 14)
#   details  - extra lines for the email body
check_alerts <- function(table, balance, period, left = NULL, details = character()) {
  s <- read_alert_settings(table, names(left))
  if (!s$total_on && !s$bills_on) return(invisible())   # both boxes unticked

  # Alerts already emailed since the last recount aren't sent again
  already <- if (identical(s$sent_period, period)) s$sent else character()

  # Collect every ticked rule that is now broken and hasn't been emailed yet
  triggered <- list()
  if (s$total_on && !is.na(s$below) && balance < s$below && !("balance" %in% already)) {
    triggered$balance <- paste0("The total is down to ", money(balance),
                                " (your limit: below ", money(s$below), ").")
  }
  for (b in names(s$bill_below)) {
    limit <- s$bill_below[[b]]
    if (s$bills_on && !is.na(limit) && left[[b]] <= limit && !(b %in% already)) {
      triggered[[b]] <- paste0("Only ", left[[b]], " × ", CURRENCY, sub("^n", "", b),
                               if (left[[b]] == 1) " bill" else " bills",
                               " left (your limit: ", limit, " or fewer).")
    }
  }
  if (length(triggered) == 0) return(invisible())

  body <- c(
    "The lab cash register is running low:",
    "",
    paste("-", unlist(triggered)),
    "",
    paste0("Total in the register: ", money(balance), "."),
    details,
    "",
    "Each of these alerts is sent once, and again only after the register has been recounted.",
    "To change or turn off these emails, use the Lab manager panel in the app."
  )
  sent <- send_email(split_emails(s$email),
                     paste0("Lab Cash: register running low (", money(balance), " left)"),
                     paste(body, collapse = "\n"))

  if (sent) {
    s$sent_period <- period
    s$sent        <- union(already, names(triggered))
    save_alert_settings(table, s)
  } else {
    showNotification("Saved. The low-balance email to the lab manager couldn't be sent.",
                     type = "warning")
  }
}


# ---- The manager-panel section and its buttons --------------------------------

# Put uiOutput("alert_panel") inside the manager panel, and call this once in
# the app's server function.
#   table      - name of the alert-settings table
#   is_manager - the reactiveVal that says whether the PIN was entered
#   bills      - bills version only: the bill types, e.g. c(20, 10, 5, 1)
alert_server <- function(input, output, session, table, is_manager, bills = NULL) {

  bill_cols <- if (length(bills) > 0) paste0("n", bills) else character()
  bill_id   <- function(b) paste0("alert_", b)          # input id, e.g. "alert_n20"

  settings <- reactiveVal(NULL)

  # Load the saved settings when the manager unlocks the panel
  observeEvent(is_manager(), {
    if (is_manager()) settings(read_alert_settings(table, bill_cols))
  })

  output$alert_panel <- renderUI({
    s <- settings()
    if (is.null(s)) return(NULL)

    # One number box per bill type, side by side
    bill_boxes <- lapply(seq_along(bill_cols), function(i) {
      numericInput(bill_id(bill_cols[i]), paste0(CURRENCY, bills[i], " bills"),
                   value = unname(s$bill_below[bill_cols[i]]), min = 0, step = 1)
    })

    tagList(
      hr(),
      h6("Low-balance emails (optional)"),
      if (!EMAIL_READY) p(class = "low",
        "Email sending isn't set up yet, so no emails will go out.",
        "See EMAIL_SETUP.md in the app folder."),
      textInput("alert_email", "Send it to (separate several addresses with commas)",
                value = s$email, width = "100%"),
      p(class = "muted", "Tick what should trigger an email. Leave both unticked for no emails."),
      checkboxInput("alert_total_on", paste0("Email when the total drops below (", CURRENCY, ")"),
                    value = s$total_on, width = "100%"),
      numericInput("alert_below", NULL, value = s$below, min = 0, step = 1),
      if (length(bill_cols) > 0) tagList(
        checkboxInput("alert_bills_on", "Email when this many bills or fewer are left:",
                      value = s$bills_on, width = "100%"),
        p(class = "muted", style = "margin-bottom: 4px;",
          "Leave a bill empty if it shouldn't trigger an email."),
        do.call(layout_columns, c(list(col_widths = rep(3, length(bill_cols))), bill_boxes))
      ),
      div(style = "display: flex; gap: 8px; flex-wrap: wrap;",
        actionButton("alert_save", "Save email settings", class = "btn-primary"),
        actionButton("alert_test", "Send a test email", class = "btn-outline-secondary")
      )
    )
  })

  # The bill limits typed into the form (NA = empty)
  form_bill_limits <- function() {
    vapply(bill_cols, function(b) {
      v <- input[[bill_id(b)]]
      if (is.null(v)) NA_real_ else as.numeric(v)
    }, numeric(1))
  }

  # Check the form; returns an error message, or NULL if everything is fine
  form_problem <- function() {
    emails <- split_emails(input$alert_email)
    limits <- form_bill_limits()
    total_on <- isTRUE(input$alert_total_on)
    bills_on <- isTRUE(input$alert_bills_on)
    if (length(emails) > 0 && !all(looks_like_email(emails)))
      return("One of the email addresses doesn't look right.")
    if (total_on && (is.na(input$alert_below) || input$alert_below <= 0))
      return("Enter the total (above zero) that should trigger an email, or untick that box.")
    if (any(!is.na(limits) & (limits < 0 | limits != round(limits))))
      return("Bill limits have to be whole numbers, 0 or more (or leave them empty).")
    if (bills_on && all(is.na(limits)))
      return("Fill in a limit for at least one bill, or untick that box.")
    if ((total_on || bills_on) && length(emails) == 0)
      return("Enter an email address.")
    NULL
  }

  observeEvent(input$alert_save, {
    req(is_manager())
    problem <- form_problem()
    if (!is.null(problem)) return(showNotification(problem, type = "error"))

    s <- settings()
    s$email      <- paste(split_emails(input$alert_email), collapse = ", ")
    s$total_on   <- isTRUE(input$alert_total_on)
    s$below      <- input$alert_below
    s$bills_on   <- isTRUE(input$alert_bills_on)
    s$bill_below <- form_bill_limits()
    if (save_alert_settings(table, s)) {
      settings(s)
      showNotification(if (s$total_on || s$bills_on) "Email settings saved." else "Low-balance emails turned off.",
                       type = "message")
    }
  })

  observeEvent(input$alert_test, {
    req(is_manager())
    emails <- split_emails(input$alert_email)
    if (length(emails) == 0 || !all(looks_like_email(emails)))
      return(showNotification("Enter a valid email address first.", type = "error"))
    if (!EMAIL_READY)
      return(showNotification("Email sending isn't set up yet (see EMAIL_SETUP.md).", type = "error"))

    sent <- send_email(emails, "Lab Cash: test email",
                       "This is a test from the Lab Cash app. Low-balance emails will come from this address.")
    if (sent) showNotification(paste("Test email sent to", paste(emails, collapse = ", ")), type = "message")
    else      showNotification("The test email couldn't be sent. Check REGISTER_MAIL_URL and REGISTER_MAIL_SECRET in .Renviron.", type = "error")
  })
}
