sidebarPanel(
  width = 12,
  class = "custom_well_for_keys",
  style = "padding-bottom: 5px",
  h4(icon("coins", class = "twitter_blue"), "App Authentication (API v1.1)"),
  p(paste0("OAuth 2.0 Bearer Token allows your Twitter developer app or project to access information publicly ",
           "available on Twitter. API rate-limits apply to the app.")),
  textInput(
    "keys_twitter_bearer_name_input",
    label = "Label",
    value = ""
  ),
  textInput(
    "keys_twitter_bearer_token_input",
    label = "Bearer Token",
    value = ""
  ),
  fluidRow(div(
    actionButton(
      "create_bearer_auth_token",
      "Create App Token",
      icon("compass-drafting"),
      style = "float:right;margin-right:15px;padding-bottom:2px"
    )
  )),
  style = "margin-left:10px;"
)