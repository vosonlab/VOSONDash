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
                                textAreaInput("youtube_video_id_input", label = "Add Youtube URL", value = "",
                                              width = NULL, height = NULL,
                                              cols = NULL, rows = 2, placeholder = NULL, resize = "vertical"),
                                actionButton("youtube_add_video_id_button", label = "Add"),
                                selectInput("youtube_video_id_list_output", "", c(), multiple = TRUE, selectize = FALSE, size = 3),
                                actionButton("youtube_remove_video_id_button", label = "Remove"),
                                div(div("Max Comments", class = "div_inline", style = "padding-bottom:10px;padding-right:10px;"),
                                    div(numericInput("youtube_max_comments_input", label = NULL, value = gbl_def_youtube_count, min = 1, width = "90px"), class = "div_inline")),
                                p(""),
                                disabled(actionButton("youtube_collect_button", label = "Collect Comments", icon = icon("cloud-download")))
                   )
                 )
          ),
          
          column(width = 9, offset = 0,
                 fluidRow(
                   tabBox(width = 12,
                          title = div(
                            span(actionButton("clear_youtube_console", label = icon("erase", lib = "glyphicon"), 
                                              style = "padding: 2px 8px;", title = "Clear Console"), 
                                 style = "padding-right: 10px;"),
                            span(icon("youtube", class = "youtube_red"), "Youtube Network Collection")
                          ),
                          tabPanel("Console", width = 12,
                                   verbatimTextOutput("youtube_arguments_output"),
                                   
                                   # youtube collect console
                                   pre(id = "youtube_console", style = "height: 300px; overflow-y: scroll")
                          )
                   ),
                   
                   sidebarPanel(width = 12, class = "custom_well_for_buttons",
                                fluidRow(collectDataButtonsUI("youtube"),
                                         collectGraphButtonsUI("youtube"),
                                         collectViewGraphButtonsUI("youtube"))
                   )
                 )
          )
        ),
        
        fluidRow(
          # youtube collection data table
          tabBox(width = 12, title = "Youtube Data",
                 tabPanel("Results", 
                          fluidRow(
                            div(checkboxInput('expand_show_youtube_cols', 'Column filters', FALSE),
                                style = "margin-left:12px; margin-right:5px;", class = "div_inline"),                            
                            div(checkboxInput("dt_youtube_truncate_text_check", "Truncate text", TRUE), 
                                class = "div_inline")
                          ),
                          uiOutput("youtube_data_cols_ui"),                          
                          DT::dataTableOutput("dt_youtube_data"))
          )
        )
)
#### end youtube_collection_tab