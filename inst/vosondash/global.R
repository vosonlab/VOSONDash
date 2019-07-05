# voson dashboard shiny app globals

# app version
app_version <- "v0.4.0"
app_date <- "16June19"

isLocal <- Sys.getenv('SHINY_PORT') == ""
suppressLibraryMessages <- TRUE

source("packages.R", local = TRUE)

# file upload sizes
ifelse(isLocal, options(shiny.maxRequestSize = 128*1024^2), # 128 MB
                options(shiny.maxRequestSize = 48*1024^2))  # 48 MB

# plots
g_random_number_range <- c(1, 5000)
g_plot_default_label_color <- "#333333"
g_plot_default_vertex_color <- "orange"
g_plot_selected_vertex_color <- "#006cb7"

g_plot_palette <- function(n = 8) brewer.pal(n, "Dark2")
g_plot_height <- 500

ta_plot_height <- "450px"

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

# modules
source("modules/collectionModule.R")
source("modules/textAnalysisModule.R")
