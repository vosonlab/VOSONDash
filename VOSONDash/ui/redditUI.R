#### reddit collection tab ------------------------------------------------------------------------------------------- #
tabItem(tabName = "reddit_collection_tab",
        fluidRow(
          column(width = 3, offset = 0,
                 fluidRow(
                   sidebarPanel(width = 12, class = "custom_well_for_controls_collect",
                                # youtube video ids input
                                textAreaInput("reddit_url_input", label = "Add Reddit URL", value = "",
                                              width = NULL, height = NULL,
                                              cols = NULL, rows = 2, placeholder = NULL, resize = "vertical"),
                                actionButton("reddit_add_url_button", label = "Add"),
                                selectInput("reddit_url_list_output", "", c(), multiple = TRUE, selectize = FALSE, 
                                            size = 3),
                                actionButton("reddit_remove_url_button", label = "Remove"),
                                p(""),
                                disabled(actionButton("reddit_collect_button", label = "Collect Threads", 
                                                      icon = icon("cloud-download")))
                   )
                 )
          ),
          
          column(width = 9, offset = 0,
                 fluidRow(
                   tabBox(width = 12,
                          title = div(
                            span(actionButton("clear_reddit_console", label = icon("erase", lib = "glyphicon"), 
                                              style = "padding: 2px 8px;", title = "Clear Console"), 
                                 style = "padding-right: 10px;"),
                            span(icon("reddit", class = "reddit_red"), "Reddit Network Collection")
                          ),                          
                          tabPanel("Console", width = 12,
                                   verbatimTextOutput("reddit_arguments_output"),
                                   
                                   # youtube collect console
                                   pre(id = "reddit_console", style = "height: 300px; overflow-y: scroll")
                          )
                   ),
                   
                   sidebarPanel(width = 12, class = "custom_well_for_buttons",
                                fluidRow(collectDataButtonsUI("reddit"),
                                         collectGraphButtonsUI("reddit"),
                                         collectViewGraphButtonsUI("reddit"))
                   )
                 )
          )
        ),
        
        fluidRow(
          # youtube collection data table
          tabBox(width = 12, title = "Reddit Data",
                 tabPanel("Results", 
                          fluidRow(
                            div(checkboxInput('expand_show_reddit_cols', 'Column filters', FALSE),
                                style = "margin-left:12px; margin-right:5px;", class = "div_inline"),                            
                            div(checkboxInput("dt_reddit_truncate_text_check", "Truncate text", TRUE), 
                                class = "div_inline")
                          ),
                          uiOutput("reddit_data_cols_ui"),                          
                          DT::dataTableOutput("dt_reddit_data"))
          )
        )
)
#### end reddit_collection_tab