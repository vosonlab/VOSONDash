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
    actionButton("use_selected_token", "Use Token", icon("copy"), style = "margin-left:15px;"),
    actionButton("delete_selected_token", "Delete", icon("circle-minus"), style = "float:right;margin-right:15px;")
  ),
  style = "margin-left:10px;"
)