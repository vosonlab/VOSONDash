sidebarPanel(
  width = 12,
  class = "custom_well_for_keys",
  style = "padding-bottom: 5px",
  h4(icon("coins", class = "twitter_blue"), "User & Developer Authentication (API v1.1)"),
  p(paste0("OAuth 1.0a allows an authorized Twitter developer App to access private account information or perform a ",
           "Twitter action on behalf of a Twitter account (within the scope of permissions granted). API rate-limits ",
           "applied individually to the user.")),
  
  fluidRow(
    div(
      checkboxInput(
        "rtweet_auth_check",
        div(
          "Use rtweet embedded app 'rstats2twitter'", class = "div_inline"
        ),
        value = FALSE,
        width = NULL
      ),
      style = "display:inline-block; margin-left:15px;"
    )
  ),
  textInput(
    "keys_twitter_app_name_input",
    label = "App Name",
    value = ""
  ),
  textInput(
    "keys_twitter_api_key_input",
    label = "API Key (Consumer Key)",
    value = ""
  ),
  textInput(
    "keys_twitter_api_secret_input",
    label = "API Secret (Consumer Secret)",
    value = ""
  ),

  fluidRow(
    div(
      checkboxInput(
        "web_auth_check",
        div(
          "Note: Incomplete process will end session",
          vpopover(po_web_auth()$title, po_web_auth()$content),
          class = "div_inline"
        ),
        value = FALSE,
        width = NULL
      ),
      style = "display:inline-block; margin-left:15px;"
    ),
    div(disabled(
      actionButton(
        "create_web_auth_token",
        "Create User Auth Token",
        icon("compass-drafting")
      )
    ), style = "display:inline-block;float:right;margin-right:15px;margin-left:5px;"),
    style = "padding-bottom:0px; margin-bottom:0px"
  ),
  hr(style = "border-top:1px dotted;"),

  textInput(
    "keys_twitter_access_token_input",
    label = "Access Token",
    value = ""
  ),
  textInput(
    "keys_twitter_access_token_secret_input",
    label = "Access Token Secret",
    value = ""
  ),
  fluidRow(div(
    actionButton(
      "create_app_token",
      "Create Dev App Token",
      icon("compass-drafting"),
      style = "float:right;margin-right:15px;padding-bottom:2px"
    )
  )),
  style = "margin-left:10px"
)
