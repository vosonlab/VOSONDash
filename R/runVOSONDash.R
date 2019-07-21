#' @title Run the VOSON Dashboard Shiny Application
#' 
#' @description This function launches the \pkg{VOSONDash} Shiny app in the default web browser. 
#' 
#' @return None
#' 
#' @export
runVOSONDash <- function() {
  app_dir <- system.file("", "vosondash", package = "VOSONDash")
  if (app_dir == "") {
    stop("Could not find the app try re-installing the VOSONDash package.", call. = FALSE)
  }
  
  shiny::runApp(app_dir, display.mode = "normal", launch.browser = TRUE)
}
