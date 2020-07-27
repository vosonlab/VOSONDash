# voson dashboard shiny app server

#### shiny server ----------------------------------------------------------------------------------------------------- #
shinyServer(function(input, output, session) {
  
  # api keys
  u_api_keys_path <- "NA"
  u_api_tokens_path <- "NA"
  
  if (isLocal) {
    u_api_keys_path <- paste0(Sys.getenv("HOME"), "/vosondash_keys.rds", sep = "")
    u_api_tokens_path <- paste0(Sys.getenv("HOME"), "/vosondash_tokens.rds", sep = "")
  }
  
  #### network graphs ####
  source("server/networkGraphsServer.R", local = TRUE)
  
  #### text analysis ####
  source("server/textAnalysisServer.R", local = TRUE)
  
  #### network metrics ####
  source("server/networkMetricsServer.R", local = TRUE)  
  
  #### assortativity ####
  source("server/assortativityServer.R", local = TRUE)
  
  #### twitter ####
  source("server/twitterServer.R", local = TRUE)
  
  #### youtube ####
  source("server/youtubeServer.R", local = TRUE)
  
  #### reddit ####
  source("server/redditServer.R", local = TRUE)
  
  #### api keys ####
  source("server/apiKeysServer.R", local = TRUE)
  
  source("server/consoleServer.R", local = TRUE)
  
  if (is2910) { shinyjs::enable("twitter_semantic_assoc") }
  
  if (VOSONDash::isMac()) { shinyjs::enable("macos_font_check") }
  
  # reset collect consoles on startup
  observeEvent(input$sidebar_menu, {
    resetConsole("twitter_console", FALSE)
    resetConsole("youtube_console", FALSE)
    resetConsole("reddit_console", FALSE)
  }, once = TRUE, ignoreInit = FALSE)
  
  # stop app when browser closes
  session$onSessionEnded(function() {
    if (isLocal) {
      message("Session ended or browser closed. Exiting.\n")
      stopApp()
    }
  })
}) #### end shinyServer
