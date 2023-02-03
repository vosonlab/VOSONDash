fluidRow(
  column(
    width = 12, offset = 0,
    h4("Social Media API Creds")
  ),
  sidebarPanel(
    width = 12,
    class = "custom_well_for_controls",
    style = "padding-bottom: 10px",
    h4("Keys"),
    textOutput("user_keys_path"),
    checkboxInput('load_and_use_keys_check', 'Load and Use API Keys on app start', FALSE),
    actionButton("keys_load_button", label = "Load Keys", style = "margin-right: 10px"),
    disabled(actionButton("keys_save_button", label = "Save Keys"))
  ),
  sidebarPanel(
    width = 12,
    class = "custom_well_for_controls",
    style = "padding-bottom: 10px",
    h4("Tokens"),
    textOutput("user_tokens_path"),
    actionButton("tokens_load_button", label = "Load Tokens", style = "margin-right: 10px"),
    disabled(actionButton("tokens_save_button", label = "Save Tokens"))
  )
)
