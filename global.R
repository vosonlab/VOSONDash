# voson dashboard shiny app globals

# app version
app_version <- "v0.2.1"
app_date <- "26Nov18"

# app libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(RColorBrewer)
library(DT)
library(htmlwidgets)
library(networkD3)
library(visNetwork)

# plots
g_random_number_range <- c(1, 5000)
g_plot_default_label_color <- "#333333"
g_plot_default_vertex_color <- "orange"
g_plot_selected_vertex_color <- "#be29ec"
g_plot_palette <- brewer.pal(8, "Dark2")

# data tables
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

# twitter
g_default_tweet_count <- 100

# modules
source("modules/textAnalysisModule.R", local = TRUE)