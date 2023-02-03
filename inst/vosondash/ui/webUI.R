#### hyperlink collection tab ------------------------------------------------------------------------------------------- #
tabItem(tabName = "hyperlink_collection_tab",
        fluidRow(
          column(width = 4, offset = 0,
                 fluidRow(
                   tabBox(title = NULL,
                          id = "hyperlink_control_tabset",
                          width = 12,
                          tabPanel("Collect Data",
                                   
                                   div(tags$b("Seed Page URL")),
                                   textInput("hyperlink_url_input", label = NULL, value = ""),
                                   div(
                                     div(tags$b("Follow hrefs"), class = "div_inline", style = "padding-bottom:10px;padding-right:10px;"),
                                     div(selectInput("hyperlink_crawl_type_select", label = NULL,
                                                     choices = c("external" = "ext",
                                                                 "internal" = "int", 
                                                                 "all" = "all"),
                                                     multiple = FALSE, selected = "external", width = "100px"), class = "div_inline")
                                   ),
                                   div(
                                     div(tags$b("Max depth"), class = "div_inline", style = "padding-bottom:10px;padding-right:10px;"),
                                     div(textInput("hyperlink_max_depth_input", label = NULL, value = "", width = "50px"), class = "div_inline")
                                   ),
                                   div(
                                     div(tags$b("Request delay"), class = "div_inline", style = "padding-bottom:10px;padding-right:10px;"),
                                     div(
                                       checkboxInput("hyperlink_request_delay_robots_checkbox", label = "Use robots.txt", value = TRUE, width = NULL),
                                       class = "div_inline"
                                     )
                                   ),
                                   conditionalPanel(
                                     condition = "!input.hyperlink_request_delay_robots_checkbox",
                                     div(
                                       div(tags$b("Custom delay (secs)"), class = "div_inline", style = "padding-bottom:10px;padding-right:10px;"),
                                       div(textInput("hyperlink_delay_input", label = NULL, value = 2, width = "50px"), class = "div_inline")
                                     )
                                   ),
                                   
                                   actionButton("hyperlink_add_url_button", label = "Add"),
                                   conditionalPanel(
                                     condition = "output.seed_table_toggle",
                                     DTOutput("hyperlink_seed_urls_table"),
                                     div(actionButton("hyperlink_remove_url_button", label = "Remove"), style = "padding-top:10px;")
                                   ),
                                   p(""),
                                   disabled(actionButton("hyperlink_collect_button", label = "Collect Hyperlinks", 
                                                         icon = icon("cloud-arrow-down")))
                                   
                          ), # end tabPanel
                          tabPanel("Create Network",
                                   div(tags$b("Network")),
                                   selectInput("hyperlink_network_type_select", label = NULL, choices = c("activity", "actor"), multiple = FALSE),
                                   p(""),
                                   disabled(actionButton("hyperlink_create_button", label = "Create Network", icon = icon("share-nodes")))
                                   
                          ) # end tabPanel
                   ) # end tabBox
                 ) # end fluidRow
          ), # end column
          
          column(width = 8, offset = 0,
                 fluidRow(
                   tabBox(width = 12,
                          title = div(
                            span(actionButton("clear_hyperlink_console", label = icon("erase", lib = "glyphicon"), 
                                              style = "padding: 2px 8px;", title = "Clear Console"), 
                                 style = "padding-right: 10px;"),
                            span(icon("globe", class = "hyperlink_green"), "Hyperlink Network Collection")
                          ),                          
                          tabPanel("Console", width = 12,
                                   verbatimTextOutput("hyperlink_arguments_output"),
                                   tags$head(
                                     tags$style("#hyperlink_arguments_output{overflow-y:scroll; max-height: 80px;}")
                                   ),
                                   
                                   # hyperlink collect console
                                   pre(id = "hyperlink_console", style = "height: 300px; overflow-y: scroll")
                          )
                   ),
                   
                   sidebarPanel(width = 12, class = "custom_well_for_buttons",
                                fluidRow(collectDataButtonsUI("hyperlink"),
                                         collectNetworkButtonsUI("hyperlink"),
                                         collectGraphButtonsUI("hyperlink"),
                                         collectViewGraphButtonsUI("hyperlink"))
                   )
                 )
          )
        ),
        
        fluidRow(
          # hyperlink collection data table
          tabBox(width = 12, title = "Hyperlink Data",
                 tabPanel("Results", 
                          fluidRow(
                            div(checkboxInput('expand_show_hyperlink_cols', 'Column filters', FALSE),
                                style = "margin-left:12px; margin-right:5px;", class = "div_inline"),                            
                            div(checkboxInput("dt_hyperlink_truncate_text_check", "Truncate text", TRUE), 
                                class = "div_inline")
                          ),
                          uiOutput("hyperlink_data_cols_ui"),                          
                          DT::dataTableOutput("dt_hyperlink_data"))
          )
        )
)
#### end hyperlink_collection_tab