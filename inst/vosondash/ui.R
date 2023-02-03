# voson dashboard shiny app ui

#### shiny ui --------------------------------------------------------------------------------------------------------- #
dashboardPage(
  skin = "blue",
  title = paste0("VOSON Dashboard ", app_version, sep = ""),
  
  dashboardHeader(
    tags$li(class = "dropdown",
            tags$style(".main-header {min-height: 40px; line-height: 40px}"),
            tags$style(".main-header .logo {height: 40px; line-height: 40px}"),
            tags$style(".sidebar-toggle {height: 40px; padding-top: 1px !important;}"),
            tags$style(".navbar {min-height:40px !important}")),
            
            title = span(icon("diagram-project"), "VOSON Dashboard", 
                    span(app_version, class = "version")),
    
            titleWidth = 300
    ),
  
  # sidebar menu items
  dashboardSidebar(useShinyjs(), width = 180,
                   tags$style(".left-side, .main-sidebar {padding-top: 40px}"),
                   sidebarMenu(id = "sidebar_menu",
                               h4("Analysis", style = "padding-left:20px; font-size:16px;"),
                               menuItem("Network Graphs", tabName = "network_graphs_tab", icon = icon("share-nodes"), 
                                        selected = TRUE),
                               menuItem("Network Metrics", tabName = "network_metrics_tab", icon = icon("ruler-combined")),
                               menuItem("Text Analysis", tabName = "text_analysis_tab", icon = icon("arrow-up-wide-short")),
                               menuItem("Assortativity", tabName = "assortativity_tab", icon = icon("braille")),
                               h4("Collection", style = "padding-left:20px; font-size:16px;"),
                               menuItem("Twitter", tabName = "twitter_collection_tab", icon = icon("twitter")),
                               menuItem("Youtube", tabName = "youtube_collection_tab", icon = icon("youtube")),
                               menuItem("Reddit", tabName = "reddit_collection_tab", icon = icon("reddit")),
                               menuItem("Hyperlink", tabName = "hyperlink_collection_tab", icon = icon("globe")),
                               menuItem("API Keys", tabName = "keys_tab", icon = icon("key"))
                   )
  ),
    
  # page body
  dashboardBody(
    # additional js features
    useShinyjs(),
    
    extendShinyjs(text = disable_tab_jscode, functions = c("disableTab")),
    inlineCSS(disable_tab_css),
    
    extendShinyjs(text = gbl_scroll_console, functions = c("scroll_console")),
    # custom ui stylesheet
    tags$head(
      tags$script(src = "popper.js"),
      tags$script(HTML("$(function () {
        $('[data-toggle = \"popover\"]').popover()
      })
      $('.popover-dismiss').popover({
        trigger: 'focus'
      })")),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "favicon/favicon-16x16.png"),
      tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon/favicon-32x32.png"),
      tags$link(rel = "apple-touch-icon", type = "image/png", sizes = "180x180",
                href = "favicon/apple-touch-icon.png"),
      tags$link(rel = "apple-touch-icon", type = "image/png", sizes = "120x120",
                href = "favicon/apple-touch-icon-120x120.png"),
      tags$link(rel = "apple-touch-icon", type = "image/png", sizes = "76x76",
                href = "favicon/apple-touch-icon-76x76.png"),
      tags$link(rel = "apple-touch-icon", type = "image/png", sizes = "60x60",
                href = "favicon/apple-touch-icon-60x60.png")
    ),    
    
    tabItems(
      
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
      
      #### hyperlink collection tab
      source("ui/webUI.R")$value,
      
      #### api keys tab
      source("ui/apiKeysUI.R")$value
      
    ) ## end tabItems
  ) ## end dashboardBody
) #### end dashboardPage
