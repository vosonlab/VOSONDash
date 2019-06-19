#### api keys tab ----------------------------------------------------------------------------------------------------- #
tabItem(tabName = "keys_tab",
        fluidRow(
          column(width = 3, offset = 0,
                 fluidRow(
                   sidebarPanel(width = 12, class = "custom_well_for_controls", style = "padding-bottom: 10px",
                                h3("Social Media API Creds"), p("API credentials are stored in the users home directory.")
                   ),
                   sidebarPanel(width = 12, class = "custom_well_for_controls", style = "padding-bottom: 10px",
                                h4("Keys"),
                                p(g_api_keys_path),                                
                                checkboxInput('load_and_use_keys_check', 'Load and Use API Keys on app start', FALSE),
                                actionButton("keys_load_button", label = "Load Keys", style = "margin-right: 10px"),
                                disabled(actionButton("keys_save_button", label = "Save Keys"))
                   ),
                   sidebarPanel(width = 12, class = "custom_well_for_controls", style = "padding-bottom: 10px",
                                h4("Tokens"),
                                p(g_api_tokens_path),                                
                                # checkboxInput('load_and_use_tokens_check', 'Load and Use API Tokens on app start', FALSE),
                                actionButton("tokens_load_button", label = "Load Tokens", style = "margin-right: 10px"),
                                disabled(actionButton("tokens_save_button", label = "Save Tokens"))
                   )
                 )
          ),
          
          column(width = 9, offset = 0,
                 
                 column(width = 6, offset = 0,
                        fluidRow(
                                sidebarPanel(width = 12, class = "custom_well_for_keys", style = "padding-bottom: 10px",
                                             h4("Token Creation"),
                                             p("Web Auth Tokens require an App name, Consumer Key, Consumer Secret, a twitter account and 
                                               a web browser that allows new tabs to be opened. The user will be asked to log into 
                                               twitter and authorize the app before the token can be created."),
                                             p("Dev App Tokens require an App Name and all four API Keys as found in their twitter 
                                               developer app settings.")
                                ),                                
                          sidebarPanel(width = 12, class = "custom_well_for_keys",
                                       h4(icon("coins", class = "twitter_blue"), "Twitter Tokens"),
                                       textInput("keys_twitter_app_name_input", label = "App Name", value = ""),
                                       textInput("keys_twitter_api_key_input", label = "Consumer Key", value = ""),
                                       textInput("keys_twitter_api_secret_input", label = "Consumer Secret", value = ""),
                                       
                                       fluidRow(
                                       div(disabled(actionButton("create_web_auth_token", "Create Web Auth Token", icon("drafting-compass"))), style = "display:inline-block;float:right;margin-right:15px;margin-left:5px;"),
                                       div(checkboxInput("web_auth_check", "Experimental (aborting will end app session)", value = FALSE, width = NULL), style = "display:inline-block;float:right;"),
                                       style = "padding-bottom:0px; margin-bottom:0px"),
                                       
                                       textInput("keys_twitter_access_token_input", label = "Access Token", value = ""),
                                       textInput("keys_twitter_access_token_secret_input", label = "Access Token Secret", value = ""),
                                       fluidRow(div(actionButton("create_app_token", "Create Dev App Token", icon("drafting-compass"), style = "float:right;margin-right:15px;padding-bottom:2px")))
                          ),
                          sidebarPanel(width = 12, class = "custom_well_for_keys",
                                  h4("Created Token"),
                                  verbatimTextOutput("save_token_output"),
                                  disabled(actionButton("save_token", "Add Token to Select List"))
                          )                           
                        )
                 ),
                 
                 column(width = 6, offset = 0,
                        fluidRow(
                                sidebarPanel(width = 12, class = "custom_well_for_keys", style = "padding-bottom: 10px",
                                             h4("Select Authorization"), 
                                             p("Select the tokens and keys to use for data collection.")
                                ),                                  
                          sidebarPanel(width = 12, class = "custom_well_for_controls", style = "padding-bottom: 5px",
                                       h4(icon("twitter", class = "twitter_blue"), "Twitter Auth"),
                                       selectInput("twitter_token_select", "Select twitter token", c("None"), selected = NULL, width = 340, size = NULL),
                                       #verbatimTextOutput("twitter_set_token", placeholder = FALSE),
                                       textOutput("twitter_set_token", container = span, inline = FALSE),
                                       fluidRow(actionButton("use_selected_token", "Use Token", icon("copy"), style = "margin-left:15px;"),
                                                actionButton("delete_selected_token", "Delete", icon("delete"), style = "float:right;margin-right:15px;"))
                          ),                                
                          sidebarPanel(width = 12, class = "custom_well_for_keys",
                                       h4(icon("youtube", class = "youtube_red"), "Youtube Auth"),
                                       textInput("keys_youtube_api_key_input", label = "Data API Key", value = ""),
                                       actionButton("keys_youtube_populate_button", "Use Key", icon("copy"))
                          )
                        )
                 )
                 
          )
        ),
        
        fluidRow(
          column(width = 12, offset = 0,
                 h4("Keys & Token Log"),
                 verbatimTextOutput('api_keys_log_output', placeholder = TRUE)
          )
        )
)
#### end keys_tab