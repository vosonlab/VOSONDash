#' Launch the VOSON Dashboard Shiny Application
#' 
#' @return None
#' 
#' @export
launchVOSONDash <- function() {
  app_dir <- system.file("", "vosondash", package = "VOSONDash")
  if (app_dir == "") {
    stop("Could not find application directory. Try re-installing the VOSONDash package.", call. = FALSE)
  }
  
  shiny::runApp(app_dir, display.mode = "normal", launch.browser = TRUE)
}

# runApp(appDir = "./inst/vosondash")
