# voson dashboard shiny app globals

# app version
app_version <- "v0.3.7 dev"
app_date <- "14Mar19"

# app libraries
library(shiny)
suppressMessages(library(shinydashboard))
suppressMessages(library(shinyjs))
library(RColorBrewer)
suppressMessages(library(DT))
library(htmlwidgets)
suppressMessages(library(networkD3))
library(visNetwork)
library(syuzhet)

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
  # targets = "_all",
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

# modules
source("modules/textAnalysisModule.R", local = TRUE)
