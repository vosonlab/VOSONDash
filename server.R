# voson dashboard shiny app server

# server libraries
library(igraph)
library(SnowballC)
library(tm)
library(lattice)
library(wordcloud)
library(vosonSML)
library(dplyr)

# server helper files
source("utils.R", local = TRUE)
source("sml.R", local = TRUE)

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
  
  #### api keys ####
  source("server/apiKeysServer.R", local = TRUE)
  
}) #### end shinyServer