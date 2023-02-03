sidebarPanel(
  width = 12,
  class = "custom_well_for_controls",
  style = "padding-bottom: 5px",
  h4(icon("twitter", class = "twitter_blue"), "Select Token"),
  selectInput(
    "twitter_token_select",
    "Select twitter token",
    c("None"),
    selected = NULL,
    width = 340,
    size = NULL
  ),
  textOutput("twitter_set_token", container = span, inline = FALSE),
  fluidRow(
    disabled(actionButton("use_selected_token", "Use Token", icon("copy"), style = "margin-left:15px;")),
    disabled(actionButton("delete_selected_token", "Delete", icon("circle-minus"), style = "float:right;margin-right:15px;"))
  ),
  conditionalPanel(
    condition = "output.use_selected_token_toggle",
    htmlOutput("selected_token_msg")
  ),
  style = "margin-left:10px;"
)