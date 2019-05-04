# voson dashboard shiny app globals

# app version
app_version <- "v0.3.8"
app_date <- "04May19"

isLocal <- Sys.getenv('SHINY_PORT') == ""

source("packages.R", local = TRUE)

# set to launch in browser so download buttons work correctly
# if (isLocal) {
#   shinyLaunchOption <- getOption('shiny.launch.browser') # save shiny launch option
#   cat("Shiny launch set to browser.\n")
#   options(shiny.launch.browser = TRUE) # set to browser
# }

# file upload sizes
ifelse(isLocal, options(shiny.maxRequestSize = 128*1024^2), # 128 MB
                options(shiny.maxRequestSize = 48*1024^2))  # 48 MB

# plots
g_random_number_range <- c(1, 5000)
g_plot_default_label_color <- "#333333"
g_plot_default_vertex_color <- "orange"
g_plot_selected_vertex_color <- "#be29ec"
g_plot_palette <- function(n = 8) brewer.pal(n, "Dark2")

# data tables
options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))

g_dt_length_menu <- list(c(10, 50, -1), c('10', '50', 'All'))
g_dt_page_length <- 10
g_dt_horiz_scroll <- TRUE

g_dt_col_defs <- list(list(
  render = JS(
    "function(data, type, row, meta) {",
    "return type === 'display' && data.length > 36 ?",
    "'<span title=\"' + data + '\">' + data.substr(0, 36) + '...</span>' : data;",
    "}")
))

# collection
g_default_tweet_count <- 100
g_default_youtube_count <- 200

# api keys
g_api_keys_path <- paste0(getwd(), "/voson_keys.rds", sep = "")

# helpers
source("utils.R", local = TRUE)
source("sml.R", local = TRUE)

# modules
source("modules/collectionModule.R", local = TRUE)
source("modules/textAnalysisModule.R", local = TRUE)
