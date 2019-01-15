# voson dashboard shiny app ui

#### shiny ui --------------------------------------------------------------------------------------------------------- #
dashboardPage(
  skin = "blue",
  title = paste0("VOSON Dashboard ", app_version, sep = ""),
  
  dashboardHeader(title = span(icon("share-alt"), "VOSON Dashboard", 
                               span("(", app_date, span(app_version, class = 'social_green'), ")", class = "version")), 
                  titleWidth = 400),
  
  # sidebar menu items
  dashboardSidebar(useShinyjs(), width = 180,
                   sidebarMenu(id = "sidebar_menu",
                               menuItem("Home", tabName = "home_tab", icon = icon("dashboard")),
                               h4("Analysis", style = "padding-left:20px;"),
                               menuItem("Network Graphs", tabName = "network_graphs_tab", icon = icon("share-alt"), 
                                        selected = TRUE),
                               menuItem("Network Metrics", tabName = "network_metrics_tab", icon = icon("bar-chart")),
                               menuItem("Text Analysis", tabName = "text_analysis_tab", icon = icon("bar-chart")),
                               menuItem("Assortativity", tabName = "assortativity_tab", icon = icon("bar-chart")),
                               h4("Collection", style = "padding-left:20px;"),
                               sidebarMenuOutput("vosonSML_version_field"),
                               menuItem("Twitter", tabName = "twitter_collection_tab", icon = icon("twitter")),
                               menuItem("Youtube", tabName = "youtube_collection_tab", icon = icon("youtube")),
                               menuItem("Reddit", tabName = "reddit_collection_tab", icon = icon("reddit")),
                               menuItem("API Keys", tabName = "keys_tab", icon = icon("key"))
                   )
  ),
  
  # page body
  dashboardBody(
    # custom ui stylesheet
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="custom.css")
    ),
    
    # additional js features
    useShinyjs(),
    
    tabItems(
      
      #### home tab
      tabItem(tabName="home_tab",
              fluidRow(
                box(width=12, includeHTML("www/home.html"))
              )
      ),
      
      #### network graphs tab
      source("ui/networkGraphsUI.R")$value,
      
      #### network metric tab
      source("ui/networkMetricsUI.R")$value,
      
      #### text analysis tab
      source("ui/textAnalysisUI.R")$value,
      
      #### assortativity tab
      source("ui/assortativityUI.R")$value,
      
      #### twitter collection tab
      source("ui/twitterUI.R")$value,
      
      #### youtube collection tab
      source("ui/youtubeUI.R")$value,
      
      #### reddit collection tab
      source("ui/redditUI.R")$value,
      
      #### api keys tab
      source("ui/apiKeysUI.R")$value
      
    ) ## end tabItems
  ) ## end dashboardBody
) #### end dashboardPage
