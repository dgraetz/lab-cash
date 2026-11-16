# =============================================================================
#  idle.R  -  pause the app for visitors who stop using it
# =============================================================================
#
#  shinyapps.io counts the app's hours for as long as anyone has it open, even
#  if nobody touches it (the free plan has 25 hours a month). So if nobody
#  moves the mouse, scrolls, taps or types for a few minutes, the page is
#  paused: it disconnects from the app and shows a "Continue" button that
#  reloads it. Once nobody is connected, shinyapps.io lets the app sleep.
#
#  Both app.R and app_bills.R load this file with source("idle.R").
#  Put idle_ui() in the page, and call idle_server() in the server function.
# =============================================================================

IDLE_MINUTES         <- 2    # RAs only need the app to enter a payout
IDLE_MINUTES_MANAGER <- 10   # once the manager panel is unlocked (counting cash takes a while)


# The script and styles for the page
idle_ui <- function() {
  tags$head(
    tags$style(HTML("
      #idle-overlay { position: fixed; inset: 0; z-index: 2147483000;
                      background: rgba(20, 35, 28, .55); display: grid; place-items: center;
                      padding: 16px; }
      #idle-overlay .idle-box { background: #fff; border-radius: 8px; padding: 24px 28px;
                      max-width: 340px; text-align: center; box-shadow: 0 10px 30px rgba(0,0,0,.25); }
      #idle-overlay .idle-box p { margin-bottom: 12px; }
    ")),
    tags$script(HTML(sprintf("
      window.addEventListener('DOMContentLoaded', function() {
        var limit = %s * 60000;      // milliseconds without activity before pausing
        var timer = null, paused = false;

        function pause() {
          if (paused) return;
          paused = true;
          var o = document.createElement('div');
          o.id = 'idle-overlay';
          o.innerHTML = '<div class=\"idle-box\">' +
            '<p><strong>Paused</strong></p>' +
            '<p>The app pauses after a few minutes without use. Anything you hadn\\'t saved yet is cleared.</p>' +
            '<button class=\"btn btn-primary\" onclick=\"location.reload()\">Continue</button></div>';
          document.body.appendChild(o);
          if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('idle_pause', Date.now());
        }
        function restart() {
          if (paused) return;
          clearTimeout(timer);
          timer = setTimeout(pause, limit);
        }

        ['mousemove', 'mousedown', 'keydown', 'touchstart', 'scroll', 'wheel'].forEach(function(e) {
          document.addEventListener(e, restart, { passive: true });
        });

        // The app tells the page when the limit changes (manager panel unlocked)
        $(document).on('shiny:connected', function() {
          Shiny.addCustomMessageHandler('idle-limit', function(m) {
            limit = m.minutes * 60000;
            restart();
          });
        });

        restart();
      });
    ", as.numeric(IDLE_MINUTES))))
  )
}


# The server side: disconnect a paused page, and use the longer limit for the manager
idle_server <- function(input, session, is_manager) {
  observeEvent(input$idle_pause, session$close())

  observeEvent(is_manager(), {
    minutes <- if (is_manager()) IDLE_MINUTES_MANAGER else IDLE_MINUTES
    session$sendCustomMessage("idle-limit", list(minutes = minutes))
  }, ignoreInit = TRUE)
}
