#### youtube collection tab ------------------------------------------------------------------------------------------- #
tabItem(tabName = "youtube_collection_tab",
        fluidRow(
          column(width = 3, offset = 0,
                 fluidRow(
                   sidebarPanel(width = 12, class = "custom_well_for_controls",
                                checkboxInput('expand_youtube_keys_panel_check', 'Show API Key', FALSE),
                                conditionalPanel(condition = 'input.expand_youtube_keys_panel_check',
                                                 # youtube api keys input
                                                 textInput("youtube_api_key_input", label = "Data API Key", value = "")
                                )
                   ),
                   
                   sidebarPanel(width = 12, class = "custom_well_for_controls_collect",
                                # youtube video ids input
                                textInput("youtube_video_id_input", label = "Add Video ID", value = ""),
                                actionButton("youtube_add_video_id_button", label = "Add"),
                                selectInput("youtube_video_id_list_output", "", c(), multiple = TRUE, selectize = FALSE, size = 3),
                                actionButton("youtube_remove_video_id_button", label = "Remove"),
                                p(""),
                                disabled(actionButton("youtube_collect_button", label = "Collect Comments", icon = icon("cloud-download")))
                   )
                 )
          ),
          
          column(width = 9, offset = 0,
                 fluidRow(
                   tabBox(width = 12, title = span(icon("youtube", class = "youtube_red"), "Youtube Network Collection"),
                          tabPanel("Console", width = 12,
                                   verbatimTextOutput("youtube_arguments_output"),
                                   
                                   # youtube collect console
                                   pre(id = "youtube_console", style = "height: 300px; overflow-y: scroll")
                          )
                   ),
                   
                   sidebarPanel(width = 12, class = "custom_well_for_buttons",
                                fluidRow(
                                  disabled(downloadButton("download_youtube_data_button", label = "Download Data")),
                                  disabled(downloadButton("download_youtube_graph_button", label = "Download Graphml")),
                                  disabled(downloadButton("download_youtube_graphWT_button", 
                                                          label = "Download Graphml (+text)")),                    
                                  disabled(actionButton("view_youtube_graph_button", label = "View Graph", icon("eye"))),
                                  disabled(actionButton("view_youtube_graphWT_button", label = "View Graph (+text)", 
                                                        icon("eye")))
                                )
                   )
                 )
          )
        ),
        
        fluidRow(
          # youtube collection data table
          tabBox(width = 12, title = "Youtube Data",
                 tabPanel("Results", 
                          fluidRow(
                            div(checkboxInput("dt_youtube_truncate_text_check", "Truncate text", TRUE), style = "margin-left:12px; margin-right:5px;", class = "div_inline")
                          ),
                          DT::dataTableOutput("dt_youtube_data"))
          )
        )
)
#### end youtube_collection_tab