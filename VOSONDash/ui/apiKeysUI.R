#### api keys tab ----------------------------------------------------------------------------------------------------- #
tabItem(tabName = "keys_tab",
        fluidRow(
          column(width = 3, offset = 0,
                 fluidRow(
                   sidebarPanel(width = 12, class = "custom_well_for_controls", style = "padding-bottom: 10px",
                                h3("Social Media API Keys"),
                                checkboxInput('load_and_use_keys_check', 'Load and Use API Keys on app start', FALSE),
                                actionButton("keys_load_button", label = "Load Keys", style = "margin-right: 10px"),
                                disabled(actionButton("keys_save_button", label = "Save Keys"))
                   )
                 )
          ),
          
          column(width = 9, offset = 0,
                 
                 column(width = 6, offset = 0,
                        fluidRow(
                          sidebarPanel(width = 12, class = "custom_well_for_keys",
                                       h4(icon("twitter", class = "twitter_blue"), "Twitter"),
                                       textInput("keys_twitter_api_key_input", label = "API Key", value = ""),
                                       textInput("keys_twitter_api_secret_input", label = "API Secret", value = ""),
                                       textInput("keys_twitter_access_token_input", label = "Access Token", value = ""),
                                       textInput("keys_twitter_access_token_secret_input", label = "Access Token Secret", value = ""),
                                       actionButton("keys_twitter_populate_button", "Use Keys", icon("copy"))
                          )
                        )
                 ),
                 
                 column(width = 6, offset = 0,
                        fluidRow(
                          sidebarPanel(width = 12, class = "custom_well_for_keys",
                                       h4(icon("youtube", class = "youtube_red"), "Youtube"),
                                       textInput("keys_youtube_api_key_input", label = "Data API Key", value = ""),
                                       actionButton("keys_youtube_populate_button", "Use Keys", icon("copy"))
                          )
                        )
                 )
                 
          )
        ),
        
        fluidRow(
          column(width = 9, offset = 3,
                 verbatimTextOutput('keys_file_output', placeholder = FALSE)
          )
        )
)
#### end keys_tab