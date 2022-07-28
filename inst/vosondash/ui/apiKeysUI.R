#### api keys tab ----------------------------------------------------------------------------------------------------- #
tabItem(tabName = "keys_tab",
        fluidRow(
          column(width = 3, offset = 0,
                 fluidRow(
                   column(width = 12, offset = 0, h4("Social Media API Creds")),
                   sidebarPanel(width = 12, class = "custom_well_for_controls", style = "padding-bottom: 10px",
                                h4("Keys"),
                                #p("path: u_api_keys_path"),
                                textOutput("user_keys_path"),
                                checkboxInput('load_and_use_keys_check', 'Load and Use API Keys on app start', FALSE),
                                actionButton("keys_load_button", label = "Load Keys", style = "margin-right: 10px"),
                                disabled(actionButton("keys_save_button", label = "Save Keys"))
                   ),
                   sidebarPanel(width = 12, class = "custom_well_for_controls", style = "padding-bottom: 10px",
                                h4("Tokens"),
                                # p("paths: u_api_tokens_path"),
                                textOutput("user_tokens_path"),
                                # checkboxInput('load_and_use_tokens_check', 'Load and Use API Tokens on app start', FALSE),
                                actionButton("tokens_load_button", label = "Load Tokens", style = "margin-right: 10px"),
                                disabled(actionButton("tokens_save_button", label = "Save Tokens"))
                   )
                 )
          ),
          
          column(width = 9, offset = 0,
                 fluidRow(tabBox(width = 12, title = "Keys",
                                 id = "selected_keys_tab",
                                 tabPanel("Twitter",
                                          fluidRow(
                                                 
                                                 column(width = 6, offset = 0,
                                                        fluidRow(
                                                                #sidebarPanel(width = 12, class = "custom_well_for_keys", style = "padding-bottom: 10px",
                                                                column(width = 12, offset = 0,
                                                                       div(
                                                                             p("Web Auth Tokens require an App name, Consumer Key, Consumer Secret, a twitter account and 
                                                                               a web browser that allows new tabs to be opened. The user will be asked to log into 
                                                                               twitter and authorize the app before the token can be created."),
                                                                             p("Dev App Tokens require an App Name and all four API Keys as found in their twitter 
                                                                               developer app settings.")), style = "margin-left:10px"),
                                                                #),                                
                                                          sidebarPanel(width = 12, class = "custom_well_for_keys",
                                                                       h4(icon("coins", class = "twitter_blue"), "Create Token"),
                                                                       textInput("keys_twitter_app_name_input", label = "App Name", value = ""),
                                                                       textInput("keys_twitter_api_key_input", label = "Consumer Key", value = ""),
                                                                       textInput("keys_twitter_api_secret_input", label = "Consumer Secret", value = ""),
                                                                       
                                                                       fluidRow(
                                                                       div(checkboxInput("web_auth_check",
                                                                                         div("Note: Incomplete process will end session", vpopover(po_web_auth()$title, po_web_auth()$content), class = "div_inline"), 
                                                                                         # "Note: aborting will end session", 
                                                                                         value = FALSE, width = NULL), style = "display:inline-block; margin-left:15px;"),
                                                                       div(disabled(actionButton("create_web_auth_token", "Create Web Auth Token", icon("compass-drafting"))), style = "display:inline-block;float:right;margin-right:15px;margin-left:5px;"),
                                                                       style = "padding-bottom:0px; margin-bottom:0px"),
                                                                       
                                                                       textInput("keys_twitter_access_token_input", label = "Access Token", value = ""),
                                                                       textInput("keys_twitter_access_token_secret_input", label = "Access Token Secret", value = ""),
                                                                       fluidRow(div(actionButton("create_app_token", "Create Dev App Token", icon("compass-drafting"), style = "float:right;margin-right:15px;padding-bottom:2px")))
                                                          , style = "margin-left:10px"),
                                                          sidebarPanel(width = 12, class = "custom_well_for_keys",
                                                                  h4("Created Token"),
                                                                  verbatimTextOutput("save_token_output"),
                                                                  disabled(actionButton("save_token", "Add Token to Select List"))
                                                          , style = "margin-left:10px")                           
                                                        )
                                                 ),
                 
                                                 column(width = 6, offset = 0,
                                                        fluidRow(
                                                        column(width = 12, offset = 0,
                                                               div(p("Select the token to use for data collection.")
                                                                   , style = "margin-left:10px")
                                                        ),        
                                                          sidebarPanel(width = 12, class = "custom_well_for_controls", style = "padding-bottom: 5px",
                                                                       h4(icon("twitter", class = "twitter_blue"), "Select Token"),
                                                                       selectInput("twitter_token_select", "Select twitter token", c("None"), selected = NULL, width = 340, size = NULL),
                                                                       #verbatimTextOutput("twitter_set_token", placeholder = FALSE),
                                                                       textOutput("twitter_set_token", container = span, inline = FALSE),
                                                                       fluidRow(actionButton("use_selected_token", "Use Token", icon("copy"), style = "margin-left:15px;"),
                                                                                actionButton("delete_selected_token", "Delete", icon("circle-minus"), style = "float:right;margin-right:15px;"))
                                                          , style = "margin-left:15px; margin-right:10px")
                                                        )
                                                 )
                 
                                        ), value = "twitter_keys_tab"),

                                          tabPanel("Youtube",
                                                   fluidRow(
                                                   column(width = 12, offset = 0,
                                                          fluidRow(
                                                   sidebarPanel(width = 6, class = "custom_well_for_keys",
                                                                h4(icon("youtube", class = "youtube_red"), "Youtube Auth"),
                                                                textInput("keys_youtube_api_key_input", label = "Data API Key", value = ""),
                                                                actionButton("keys_youtube_populate_button", "Use Key", icon("copy"))
                                        ,style = "margin-left:10px;")))), value = "youtube_keys_tab")
          )
          )
        )),
        
        fluidRow(
          column(width = 12, offset = 0,
                 h4("Keys & Token Log"),
                 verbatimTextOutput('api_keys_log_output', placeholder = TRUE)
          )
        )
)
#### end keys_tab