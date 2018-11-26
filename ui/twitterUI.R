#### twitter collection tab ------------------------------------------------------------------------------------------- #
tabItem(tabName = "twitter_collection_tab",
        fluidRow(
          column(width = 3, offset = 0,
                 fluidRow(
                   sidebarPanel(width = 12, class = "custom_well_for_controls",
                                checkboxInput('expand_twitter_keys_panel_check', 'Show API Keys', FALSE),
                                conditionalPanel(condition = 'input.expand_twitter_keys_panel_check',
                                                 
                                                 # twitter api keys input
                                                 textInput("twitter_api_key_input", label = "API Key", value = ""),
                                                 textInput("twitter_api_secret_input", label = "API Secret", value = ""),
                                                 textInput("twitter_access_token_input", label = "Access Token", value = ""),
                                                 textInput("twitter_access_token_secret_input", label = "Access Token Secret", value = "")
                                )),
                   sidebarPanel(width = 12, class = "custom_well_for_controls_collect",
                                # twitter search term input
                                textAreaInput("twitter_search_term_input", label = "Search Term", value = "",
                                              width = NULL, height = NULL,
                                              cols = NULL, rows = 2, placeholder = NULL, resize = "vertical"),
                                
                                checkboxInput("twitter_retweets_check", "Filter out retweets", FALSE),
                                
                                div(div("Results", class = "div_inline", style = "padding-bottom:10px;padding-right:10px;"), 
                                    div(selectInput("twitter_search_type_select", label = NULL, choices = c("mixed", "recent", "popular"),
                                                    multiple = FALSE, width = "90px"), class = "div_inline")),
                                
                                div(div("Count", class = "div_inline", style = "padding-bottom:10px;padding-right:10px;margin-right:22px;"),
                                    div(numericInput("twitter_tweet_count_input", label = NULL, value = 100, min = 1, width = "90px"), class = "div_inline")),
                                
                                div(div("Language", class = "div_inline", style = "padding-bottom:10px;padding-right:10px;"),
                                    div(textInput("twitter_language_input", label = NULL, value = "", width = "45px"), class = "div_inline")),
                                
                                div("Date Range", class = "date_range_label"),
                                div(dateInput("twitter_date_since_input", label = NULL, value = "", min = NULL, max = NULL,
                                              format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                              language = "en", width = "90px", autoclose = TRUE), class = "div_inline"),
                                div(" to ", class = "div_inline"),
                                div(dateInput("twitter_date_until_input", label = NULL, value = "", min = NULL, max = NULL,
                                              format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                              language = "en", width = "90px", autoclose = TRUE), class = "div_inline"),
                                checkboxInput('expand_twitter_filters_check', 'Show Filters', FALSE),
                                conditionalPanel(condition = 'input.expand_twitter_filters_check',
                                                 div(div("From:", class = "div_inline", style = "padding-bottom:10px;padding-right:10px;"),
                                                     div(textInput("twitter_filter_from", label = NULL, value = "", width = "auto"), class = "div_inline")),
                                                 div(div("To:", class = "div_inline", style = "padding-bottom:10px;padding-right:10px;margin-right:19px;"),
                                                     div(textInput("twitter_filter_to", label = NULL, value = "", width = "auto"), class = "div_inline")),
                                                 div("Collect only tweets with:"),
                                                 checkboxInput("twitter_filter_safe", "Safe Content", FALSE),
                                                 checkboxInput("twitter_filter_media", "Images or Video", FALSE),
                                                 checkboxInput("twitter_filter_url", "URL's", FALSE),
                                                 checkboxInput("twitter_filter_negative", "Negative Attitude", FALSE),
                                                 checkboxInput("twitter_filter_positive", "Positive Attitude", FALSE)
                                ),
                                p(""),
                                disabled(actionButton("twitter_collect_button", label = "Collect Tweets", icon = icon("cloud-download")))
                   )
                 )
          ),
          
          column(width = 9, offset = 0,
                 fluidRow(
                   tabBox(width = 12, title = span(icon("twitter", class = "twitter_blue"), "Twitter Network Collection"),
                          tabPanel("Console", width = 12,
                                   verbatimTextOutput("twitter_arguments_output"),
                                   
                                   # twitter collect console
                                   pre(id = "twitter_console", style = "height: 300px; overflow-y: scroll")
                          )
                   ),
                   # download twitter data and graphml button
                   sidebarPanel(width = 12, class = "custom_well_for_buttons",
                                fluidRow(
                                  disabled(downloadButton("download_twitter_data_button", label = "Download Data")),
                                  disabled(downloadButton("download_twitter_graph_button", label = "Download Graphml")),
                                  disabled(downloadButton("download_twitter_graphWT_button", label = "Download Graphml (+text)")),
                                  disabled(actionButton("view_twitter_graph_button", label = "View Graph", icon("eye")))
                                )
                   )
                 )
          )
        ),
        
        fluidRow(
          # twitter collection data table
          tabBox(width = 12, title = "Twitter Data",
                 tabPanel("Results", 
                          fluidRow(
                            div(checkboxInput("dt_twitter_truncate_text_check", "Truncate text", TRUE), style = "margin-left:12px; margin-right:5px;", class = "div_inline")
                          ),
                          DT::dataTableOutput("dt_twitter_data"))
          )
        )
)
#### end twitter_collection_tab