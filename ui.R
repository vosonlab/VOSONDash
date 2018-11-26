# voson dashboard shiny app ui

networkGraphsUI <- source("ui/networkGraphsUI.R")
networkMetricsUI <- source("ui/networkMetricsUI.R")
textAnalysisUI <- source("ui/textAnalysisUI.R")
assortativitysUI <- source("ui/assortativityUI.R")
twitterUI <- source("ui/twitterUI.R")
youtubeUI <- source("ui/youtubeUI.R")
apiKeysUI <- source("ui/apiKeysUI.R")

#### shiny ui --------------------------------------------------------------------------------------------------------- #
dashboardPage(
  skin = "blue",
  title = paste0("VOSON Dashboard ", app_version, sep = ""),
  
  dashboardHeader(title = span(icon("share-alt"), "VOSON Dashboard", span("(", app_date, span(app_version, class = 'social_green'), ")", class = "version")), titleWidth = 400),
  
  # sidebar menu items
  dashboardSidebar(useShinyjs(), width = 180,
                   sidebarMenu(id = "sidebar_menu",
                               menuItem("Home", tabName = "home_tab", icon = icon("dashboard")),
                               menuItem("Network Graphs", tabName = "network_graphs_tab", icon = icon("share-alt"), selected = TRUE),
                               menuItem("Network Metrics", tabName = "network_metrics_tab", icon = icon("bar-chart")),
                               menuItem("Text Analysis", tabName = "text_analysis_tab", icon = icon("bar-chart")),
                               menuItem("Assortativity", tabName = "assortativity_tab", icon = icon("bar-chart")),
                               menuItem("Twitter", tabName = "twitter_collection_tab", icon = icon("twitter")),
                               menuItem("Youtube", tabName = "youtube_collection_tab", icon = icon("youtube")),
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
      networkGraphsUI$value,
      
      #### network metric tab
      networkMetricsUI$value,
      
      #### text analysis tab
      textAnalysisUI$value,
      
      #### assortativity tab
      assortativitysUI$value,
      
      #### twitter collection tab
      twitterUI$value,
      
      #### youtube collection tab
      youtubeUI$value,
      
      #### api keys tab
      apiKeysUI$value
      
    ) ## end tabItems
  ) ## end dashboardBody
) #### end dashboardPage