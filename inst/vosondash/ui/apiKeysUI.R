#### api keys tab ----------------------------------------------------------------------------------------------------- #
tabItem(
  tabName = "keys_tab",
  fluidRow(
    column(
      width = 3,
      source("ui/tokens/ui_load_files.R")$value
    ),
    column(
      width = 9,
      fluidRow(
        tabBox(
          width = 12,
          title = div(span(icon("key"), "API Keys")),
          id = "selected_keys_tab",
          
          tabPanel(
            "Twitter",
            fluidRow(
              column(
                width = 6,
                source("ui/tokens/tw/ui_select_token.R")$value,
                sidebarPanel(
                  width = 12,
                  class = "custom_well_for_keys",
                  style = "padding-bottom: 5px",
                  h4("Created Token"),
                  verbatimTextOutput("save_token_output"),
                  disabled(actionButton("save_token", "Add Token to Select List")),
                  style = "margin-left:10px"
                )
              ),
              column(
                width = 6,
                source("ui/tokens/tw/ui_create_app_auth.R")$value,
                source("ui/tokens/tw/ui_create_auth.R")$value
              )
            ),
            value = "twitter_keys_tab"
          ),
                            
          tabPanel(
            "Youtube",
            fluidRow(
              column(
                width = 12,
                source("ui/tokens/yt/ui_input_api_key.R")$value
              )
            ),
            value = "youtube_keys_tab"
          ),
          
          tabPanel(
            "Logs",
            fluidRow(
              column(
                width = 12,
                h4("Keys & Token Log"),
                verbatimTextOutput('api_keys_log_output', placeholder = TRUE)
              )
            ),
            value = "log_keys_tab"
          )
        )
      )
    )
  )
)
#### end keys_tab