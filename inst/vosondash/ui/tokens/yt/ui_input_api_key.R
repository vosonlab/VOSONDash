sidebarPanel(
  width = 6,
  class = "custom_well_for_keys",
  h4(icon("youtube", class = "youtube_red"), "Youtube Auth"),
  textInput(
    "keys_youtube_api_key_input",
    label = "Data API Key",
    value = ""
  ),
  actionButton("keys_youtube_populate_button", "Use Key", icon("copy")),
  style = "margin-left:10px;"
)
