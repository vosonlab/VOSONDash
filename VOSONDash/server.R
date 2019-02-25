# voson dashboard shiny app server

# server libraries
suppressMessages(library(igraph))
library(SnowballC)
library(tm)
library(lattice)
library(wordcloud)
library(vosonSML)
suppressMessages(library(dplyr))
library(httr)

# server helper files
source("utils.R", local = TRUE)
source("sml.R", local = TRUE)

# increase maximum file upload size to 128MB
# decrease for server deployment
options(shiny.maxRequestSize = 128*1024^2)

#### shiny server ----------------------------------------------------------------------------------------------------- #
shinyServer(function(input, output, session) {
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
  
  #### vosonSML version ####
  vosonsml_version <- getVosonSMLVersion()
  if (!is.null(vosonsml_version)) {
    vosonsml_version <- paste0("vosonSML v", vosonsml_version)  
  } else {
    vosonsml_version <- ""
  }
  
  output$vosonSML_version_field <- renderMenu({
    sidebarMenu(
      menuItem(vosonsml_version)
    )
  })
}) #### end shinyServer
