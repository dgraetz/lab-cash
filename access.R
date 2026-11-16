# =============================================================================
#  access.R  -  only open the app through the secret link
# =============================================================================
#
#  If REGISTER_ACCESS_KEY is set (in .Renviron, or as a secret on Hugging
#  Face), the app only opens when its address ends in ?key=<that value>:
#     https://.../?key=k7f2q9...
#  Anyone else gets an empty page that says nothing about what the app is.
#
#  If REGISTER_ACCESS_KEY isn't set, the app opens for everyone as before -
#  except on Hugging Face, where it stays closed until the key is set, so it
#  is never open by accident.
#
#  Both app.R and app_bills.R load this file with source("access.R") and end
#  with shinyApp(locked_ui(ui), locked_server(server)).
# =============================================================================

ACCESS_KEY    <- Sys.getenv("REGISTER_ACCESS_KEY")
ON_HUGGINGFACE <- nzchar(Sys.getenv("SPACE_ID"))   # Hugging Face sets SPACE_ID

# Does this address (its "?key=..." part) open the app?
has_access <- function(query_string) {
  if (!nzchar(ACCESS_KEY)) return(!ON_HUGGINGFACE)
  key <- parseQueryString(if (is.null(query_string)) "" else query_string)$key
  identical(key, ACCESS_KEY)
}

# What everyone without the right link sees
closed_page <- tags$div(
  style = "font-family: system-ui, sans-serif; color: #666; padding: 48px 24px; text-align: center;",
  "This page isn't available."
)

# The page: the real app with the right link, otherwise the empty page
locked_ui <- function(ui) {
  function(req) if (has_access(req$QUERY_STRING)) ui else closed_page
}

# The server: does nothing at all (reads no data) without the right link
locked_server <- function(server) {
  function(input, output, session) {
    if (has_access(isolate(session$clientData$url_search))) server(input, output, session)
  }
}
