#### text analysis tab ------------------------------------------------------------------------------------------------ #
tabItem(tabName="text_analysis_tab",
        fluidRow(
          column(width=3, offset=0,
                 fluidRow(
                   sidebarPanel(width = 12, class = "custom_well_for_controls",
                                div("Filters", style = "font-weight: bold;"),
                                disabled(checkboxInput("text_analysis_stopwords_check", "Remove standard stopwords", TRUE)),
                                disabled(textInput("text_analysis_user_stopwords_input", label = "User-defined stopwords", value = "")),
                                disabled(checkboxInput("text_analysis_user_stopwords_check", "Remove user-defined stopwords", FALSE)),
                                disabled(checkboxInput("text_analysis_twitter_hashtags_check", "Remove Twitter hashtags", TRUE)),
                                disabled(checkboxInput("text_analysis_twitter_usernames_check", "Remove Twitter usernames", TRUE)),
                                disabled(checkboxInput("text_analysis_stem_check", "Apply word stemming", FALSE))
                   ),
                   sidebarPanel(width = 12, class = "custom_well_for_controls",
                                div("Current Network Graph", style = "font-weight: bold;", style = "margin-bottom:5px;"),
                                div("These values summarise the current state of the network graph including any filtering by category and component controls.", style = "margin-bottom:5px;"),
                                verbatimTextOutput("text_analysis_details_output", placeholder = TRUE)
                   )
                 )
          ),
          
          column(width=9, offset=0,
                 fluidRow(
                   tabBox(width = 12, title = span(icon("bar-chart", class = "social_green"), "Text Analysis"), 
                          selected = "Word Frequency", id = "selected_text_analysis_tab",
                          
                          tabPanel("Word Frequency",
                                   fluidRow(
                                     column(width = 9, taPlotContainerUI("word_freqs")),
                                     column(width = 3,
                                            fluidRow(
                                              div(sidebarPanel(width = 12, class = "custom_well_for_controls",
                                                               sliderInput("text_analysis_wf_top_count", "Display Top:", min = 1, max = 50, value = 20),
                                                               sliderInput("text_analysis_wf_min_word_freq", "Minimum Frequency:", min = 1, max = 50, value = 1)
                                              ), style = "margin-right:10px;")
                                            )
                                     )
                                   )
                          ),
                          tabPanel("Word Cloud",
                                   fluidRow(
                                     column(width = 9, taPlotContainerUI("word_clouds")),
                                     column(width = 3,
                                            fluidRow(
                                              div(sidebarPanel(width = 12, class = "custom_well_for_controls",
                                                               sliderInput("text_analysis_wc_min_word_freq", "Minimum Frequency:", min = 1, max = 50, value = 1),
                                                               sliderInput("text_analysis_wc_max_word_count", "Maximum Words:", min = 1, max = 300, value = 50)
                                              ), style = "margin-right:10px;"))
                                     )
                                   )
                          ),
                          tabPanel("Comparison Cloud",
                                   fluidRow(
                                     column(width = 9, plotOutput("comparison_cloud_plot", height = "550px")),
                                     column(width = 3,
                                            fluidRow(
                                              div(sidebarPanel(width = 12, class = "custom_well_for_controls",
                                                               sliderInput("text_analysis_cc_max_word_count", "Maximum Words:", min = 1, max = 300, value = 50)
                                              ), style = "margin-right:10px;"))
                                     )
                                   )
                          ),
                          tabPanel("Sentiment",
                                   fluidRow(
                                     column(width = 12, div("Analysis uses the Syuzhet Package - NRC Emotion Lexicon (Saif Mohammad)", style = "margin-left:15px;"))
                                   ),
                                   fluidRow(
                                     column(width = 12, taPlotContainerUI("word_sentiments"))
                                   )
                          )                          
                   )
                 )
          )
        )
)
#### end text_analysis_tab