# voson dashboard shiny app globals

# app version
app_version <- paste0("v", VOSONDash::getVOSONDashVer())

isLocal <- Sys.getenv('SHINY_PORT') == ""
suppressLibMsgs <- TRUE

source("packages.R", local = TRUE)

# file upload sizes
ifelse(isLocal, options(shiny.maxRequestSize = 128*1024^2), # 128 MB
                options(shiny.maxRequestSize = 48*1024^2))  # 48 MB

# graph data
voson_cat_prefix <- "^vosonCA_"
voson_txt_prefix <- "^vosonTxt_"

# plots
gbl_rng_range <- c(1, 5000)
gbl_plot_def_label_color <- "#333333"
gbl_plot_def_vertex_color <- "orange"
gbl_plot_sel_vertex_color <- "#006cb7"

gbl_plot_palette <- function(n = 8) brewer.pal(n, "Dark2")
gbl_plot_height <- 500

ta_plot_height <- "450px"

# data tables
options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))

gbl_dt_menu_len <- list(c(10, 50, -1), c('10', '50', 'All'))
gbl_dt_page_len <- 10

gbl_dt_col_defs <- list(list(
  render = JS(
    "function(data, type, row, meta) {",
    "return type === 'display' && data.length > 36 ?",
    "'<span title=\"' + data + '\">' + data.substr(0, 36) + '...</span>' : data;",
    "}")
))

# collection
gbl_def_tweet_count <- 100
gbl_def_youtube_count <- 200

# modules
source("modules/collectionModule.R")
source("modules/textAnalysisModule.R")
