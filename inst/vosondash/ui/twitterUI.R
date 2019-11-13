#### twitter collection tab ------------------------------------------------------------------------------------------- #
tabItem(tabName = "twitter_collection_tab",
        fluidRow(
          column(width = 3, offset = 0,
                 fluidRow(
                   sidebarPanel(width = 12, class = "custom_well_for_controls",
                                p(tags$b("Auth Token")),
                                verbatimTextOutput("twitter_collect_token_output", placeholder = TRUE)
                                ),
                                    
                   # sidebarPanel(width = 12, class = "custom_well_for_controls_collect",
                    
                   tabBox(title = NULL,
                          id = "twitter_control_tabset",
                          width = 12,
                          tabPanel("Collect Data",
                                                 
                                # twitter search term input
                                div(tags$b("Search Query"), 
                                    vpopover(po_twit_query()$title, po_twit_query()$content), 
                                    style = "margin-bottom:5px;"),
                                textAreaInput("twitter_search_term_input", label = NULL, value = "",
                                              width = NULL, height = NULL,
                                              cols = NULL, rows = 2, placeholder = NULL, resize = "vertical"),
                                
                                checkboxInput("twitter_retweets_check", "Include retweets", TRUE),
                                checkboxInput("twitter_retry_check", "Retry on rate limit", TRUE),
                                
                                div(div("Results", 
                                        vpopover(po_twit_results()$title, po_twit_results()$content),
                                        class = "div_inline", style = "padding-bottom:10px;padding-right:10px;"), 
                                    div(selectInput("twitter_search_type_select", label = NULL, choices = c("recent", "mixed", "popular"),
                                                    multiple = FALSE, width = "90px"), class = "div_inline")),
                                
                                div(div("Count", class = "div_inline", style = "padding-bottom:10px;padding-right:10px;margin-right:28px;"),
                                    div(numericInput("twitter_tweet_count_input", label = NULL, value = gbl_def_tweet_count, min = 1, width = "90px"), class = "div_inline")),
                                
                                div(div("Language", 
                                        vpopover(po_twit_lang()$title, po_twit_lang()$content),
                                        class = "div_inline", style = "padding-bottom:10px;padding-right:10px;"),
                                    div(textInput("twitter_language_input", label = NULL, value = "", width = "45px"), class = "div_inline")),
                                
                                div(div("Date Until", class = "div_inline", style = "padding-bottom:10px;padding-right:16px;"),
                                    div(suppressWarnings(dateInput("twitter_date_until_input", label = NULL, value = "", min = NULL, max = NULL,
                                                  format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                                  language = "en", width = "90px", autoclose = TRUE)), class = "div_inline")),
                                
                                checkboxInput('expand_twitter_id_filters_check', 
                                              div("Tweet ID Range", vpopover(po_twit_id_range()$title, po_twit_id_range()$content), class = "div_inline"), 
                                              FALSE),
                                conditionalPanel(condition = 'input.expand_twitter_id_filters_check',
                                                div(div("Since ID", class = "div_inline", style = "padding-bottom:10px;padding-right:10px;"),
                                                    div(textInput("twitter_since_id_input", label = NULL, value = "", width = "110px"), class = "div_inline")),
                                                div(div("Max ID", class = "div_inline", style = "padding-bottom:10px;padding-right:20px;"),
                                                    div(textInput("twitter_max_id_input", label = NULL, value = "", width = "110px"), class = "div_inline"))
                                ),
                                
                                checkboxInput('expand_twitter_filters_check', 'Additional Filters', FALSE),
                                conditionalPanel(condition = 'input.expand_twitter_filters_check',
                                                 div(div("From User:", class = "div_inline", style = "padding-bottom:10px;padding-right:10px;"),
                                                     div(textInput("twitter_filter_from", label = NULL, value = "", width = "auto"), class = "div_inline")),
                                                 div(div("To User:", class = "div_inline", style = "padding-bottom:10px;padding-right:10px;margin-right:19px;"),
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
                                
                          ), # end tabPanel
                          tabPanel("Create Network",
                                   selectInput("twitter_network_type_select", label = "Network", choices = c("activity", "actor", "twomode", "semantic"), multiple = FALSE),
                                   conditionalPanel(
                                           condition = "input.twitter_network_type_select == 'activity' || 
                                                        input.twitter_network_type_select == 'actor'",
                                           checkboxInput("twitter_network_text", "Add Text", FALSE)
                                   ),
                                   conditionalPanel(
                                           condition = "input.twitter_network_type_select == 'actor'",
                                           checkboxInput("twitter_network_user_data", "Lookup User Data", FALSE)
                                   ),
                                   conditionalPanel(
                                           condition = "input.twitter_network_type_select == 'twomode'",
                                           textAreaInput("twitter_twomode_remove", label = "Remove Terms", value = "",
                                                     width = NULL, height = NULL,
                                                     cols = NULL, rows = 2, placeholder = NULL, resize = "vertical")
                                   ),
                                   conditionalPanel(
                                           condition = "input.twitter_network_type_select == 'semantic'",
                                           checkboxInput("twitter_semantic_stopwords", "Remove English stopwords", TRUE),
                                           textAreaInput("twitter_semantic_remove", label = "Remove Terms", value = "",
                                                     width = NULL, height = NULL,
                                                     cols = NULL, rows = 2, placeholder = NULL, resize = "vertical"),
                                           sliderInput("twitter_term_freq", "% Most Frequent Words", 1, 100, value = 5, step = 1,
                                                       round = TRUE),
                                           sliderInput("twitter_hashtag_freq", "% Most Frequent Hashtags", 1, 100, value = 50, step = 1,
                                                       round = TRUE)
                                   ),
                                   p(""),
                                   disabled(actionButton("twitter_create_button", label = "Create Network", icon = icon("share-alt")))
                                   
                          ) # end tabPanel
                   ) # end tabBox
                                
                   #) # end sidebar
                 )
          ),
          
          column(width = 9, offset = 0,
                 fluidRow(
                   tabBox(width = 12, 
                          title = div(
                            span(actionButton("clear_twitter_console", label = icon("erase", lib = "glyphicon"), 
                                              style = "padding: 2px 8px;", title = "Clear Console"), 
                                 style = "padding-right: 10px;"),
                            span(icon("twitter", class = "twitter_blue"), "Twitter Network Collection")
                          ),
                          tabPanel("Console", width = 12,
                                   verbatimTextOutput("twitter_arguments_output"),

                                   # twitter collect console
                                   pre(id = "twitter_console", style = "height: 300px; overflow-y: scroll")
                          )
                   ),
                   # download twitter data and graphml button
                   sidebarPanel(width = 12, class = "custom_well_for_buttons",
                                fluidRow(collectDataButtonsUI("twitter"),
                                         collectNetworkButtonsUI("twitter"),
                                         collectGraphButtonsUI("twitter"),
                                         collectViewGraphButtonsUI("twitter"))
                   )
                 )
          )
        ),
        
        fluidRow(
          # twitter collection data table
          tabBox(width = 12, title = "Twitter Data",
                 tabPanel("Results",
                          fluidRow(
                            div(checkboxInput('expand_show_twitter_cols', 'Column filters', FALSE),
                                style = "margin-left:12px; margin-right:5px;", class = "div_inline"),
                            div(checkboxInput("dt_twitter_truncate_text_check", "Truncate text", TRUE), 
                                class = "div_inline")
                          ),
                          uiOutput("twitter_data_cols_ui"),
                          DT::dataTableOutput("dt_twitter_data"))
          )
        )
)
#### end twitter_collection_tab