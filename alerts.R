# =============================================================================
#  alerts.R  -  optional email to the lab manager when the register runs low
# =============================================================================
#
#  Both app.R and app_bills.R load this file with source("alerts.R").
#
#  How it works
#  ------------
#  * In the manager panel, the lab manager can turn on a low-balance email,
#    enter who gets it, and set the amount ("email me when the register
#    drops below $50").
#  * When a payout takes the balance below that amount, the app sends ONE
#    email. It sends the next one only after the next recount, so nobody gets
#    an email for every payout while the register is low.
#  * The settings are saved like the rest of the data (Google Sheet or CSV),
#    so they survive restarts.
#
#  Sending email
#  -------------
#  The app sends through an email account. Its login goes in .Renviron (never
#  in the code):
#     REGISTER_SMTP_USER=the.lab.account@gmail.com
#     REGISTER_SMTP_PASSWORD=the app password (see EMAIL_SETUP.md)
#  Without these two lines everything else still works; the manager panel just
#  says that email isn't set up.
# =============================================================================


# ---- Settings for the email account (read from .Renviron) --------------------

SMTP_USER     <- Sys.getenv("REGISTER_SMTP_USER")
SMTP_PASSWORD <- Sys.getenv("REGISTER_SMTP_PASSWORD")
SMTP_HOST     <- Sys.getenv("REGISTER_SMTP_HOST", unset = "smtp.gmail.com")
SMTP_PORT     <- as.integer(Sys.getenv("REGISTER_SMTP_PORT", unset = "465"))

EMAIL_READY <- nzchar(SMTP_USER) && nzchar(SMTP_PASSWORD)

# The columns of the alert-settings table (it only ever has one row)
ALERT_COLUMNS <- c("enabled", "email", "below", "sent_period")


# ---- Reading and saving the alert settings -----------------------------------

read_alert_settings <- function(table) {
  d <- storage_read(table)
  if (nrow(d) == 0) {
    return(list(enabled = FALSE, email = "", below = NA, sent_period = NA))
  }
  list(
    enabled     = identical(d$enabled[1], "TRUE"),
    email       = ifelse(is.na(d$email[1]), "", d$email[1]),
    below       = suppressWarnings(as.numeric(d$below[1])),
    sent_period = suppressWarnings(as.numeric(d$sent_period[1]))
  )
}

save_alert_settings <- function(table, settings) {
  storage_replace(table, data.frame(
    enabled     = as.character(settings$enabled),
    email       = settings$email,
    below       = settings$below,
    sent_period = settings$sent_period
  ))
}

# Split "a@x.org, b@y.org" into c("a@x.org", "b@y.org")
split_emails <- function(text) {
  e <- trimws(strsplit(text, ",")[[1]])
  e[nzchar(e)]
}

looks_like_email <- function(x) grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", x)


# ---- Sending -----------------------------------------------------------------

# Send one plain-text email. Returns TRUE if it was sent, FALSE if not.
send_email <- function(to, subject, body) {
  if (!EMAIL_READY) return(FALSE)
  tryCatch({
    library(emayili)
    message <- envelope() |>
      from(SMTP_USER) |>
      to(to) |>
      subject(subject) |>
      text(body)
    smtp <- server(host = SMTP_HOST, port = SMTP_PORT,
                   username = SMTP_USER, password = SMTP_PASSWORD)
    smtp(message)
    TRUE
  }, error = function(e) {
    warning("Email not sent: ", conditionMessage(e))
    FALSE
  })
}


# ---- Checking the balance after a payout --------------------------------------

# Call this right after a payout is saved.
#   table        - name of the alert-settings table
#   balance      - the balance after the payout
#   period       - the current period (changes with every recount)
#   details      - extra lines for the email body (e.g. which bills are left)
check_low_balance <- function(table, balance, period, details = character()) {
  s <- read_alert_settings(table)

  if (!s$enabled || is.na(s$below)) return(invisible())   # alerts are off
  if (balance >= s$below)            return(invisible())   # not low yet
  if (identical(s$sent_period, period)) return(invisible()) # already sent since the last recount

  body <- c(
    paste0("The lab cash register is running low: ", money(balance), " left."),
    paste0("You asked to be emailed when it drops below ", money(s$below), "."),
    "",
    details,
    "",
    "You'll get the next email after the register has been recounted and drops low again.",
    "To change or turn off this email, use the Lab manager panel in the app."
  )
  sent <- send_email(split_emails(s$email),
                     paste("Lab Cash: register is low,", money(balance), "left"),
                     paste(body, collapse = "\n"))

  if (sent) {
    s$sent_period <- period
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
alert_server <- function(input, output, session, table, is_manager) {

  settings <- reactiveVal(NULL)

  # Load the saved settings when the manager unlocks the panel
  observeEvent(is_manager(), {
    if (is_manager()) settings(read_alert_settings(table))
  })

  output$alert_panel <- renderUI({
    s <- settings()
    if (is.null(s)) return(NULL)
    tagList(
      hr(),
      h6("Low-balance email (optional)"),
      if (!EMAIL_READY) p(class = "low",
        "Email sending isn't set up yet, so no emails will go out.",
        "See EMAIL_SETUP.md in the app folder."),
      checkboxInput("alert_on", "Email me when the register runs low", value = s$enabled),
      textInput("alert_email", "Send it to (separate several addresses with commas)",
                value = s$email, placeholder = "lab.manager@uoregon.edu"),
      numericInput("alert_below", paste0("When the balance drops below (", CURRENCY, ")"),
                   value = s$below, min = 0, step = 1),
      div(style = "display: flex; gap: 8px; flex-wrap: wrap;",
        actionButton("alert_save", "Save email settings", class = "btn-primary"),
        actionButton("alert_test", "Send a test email", class = "btn-outline-secondary")
      ),
      if (s$enabled && !is.na(s$sent_period))
        p(class = "muted", style = "margin-top: 8px;",
          "An email has already gone out since the last recount.")
    )
  })

  # Check the form; returns an error message, or NULL if everything is fine
  form_problem <- function() {
    emails <- split_emails(input$alert_email)
    if (input$alert_on && length(emails) == 0) return("Enter an email address.")
    if (length(emails) > 0 && !all(looks_like_email(emails)))
      return("One of the email addresses doesn't look right.")
    if (input$alert_on && (is.na(input$alert_below) || input$alert_below <= 0))
      return("Enter the amount (above zero) that should trigger the email.")
    NULL
  }

  observeEvent(input$alert_save, {
    req(is_manager())
    problem <- form_problem()
    if (!is.null(problem)) return(showNotification(problem, type = "error"))

    s <- settings()
    s$enabled <- input$alert_on
    s$email   <- paste(split_emails(input$alert_email), collapse = ", ")
    s$below   <- input$alert_below
    if (save_alert_settings(table, s)) {
      settings(s)
      showNotification(if (s$enabled) "Email settings saved." else "Low-balance email turned off.",
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
                       "This is a test from the Lab Cash app. Low-balance emails will look like this and come from this address.")
    if (sent) showNotification(paste("Test email sent to", paste(emails, collapse = ", ")), type = "message")
    else      showNotification("The test email couldn't be sent. Check the email login in .Renviron.", type = "error")
  })
}
