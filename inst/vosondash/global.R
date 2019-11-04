# voson dashboard shiny app globals

v029 <- isVosonSML0290()

# app version
app_version <- paste0("v", VOSONDash::getVOSONDashVer())

isLocal <- Sys.getenv('SHINY_PORT') == ""
# if (exists(".VOSONIsLocal")) {
if (!is.null(getShinyOption("VOSONIsLocal"))) {
  # isLocal <- .VOSONIsLocal
  isLocal <- getShinyOption("VOSONIsLocal")
}

pkgMsgs <- TRUE
# if (exists(".VOSONPkgMsgs")) {
if (!is.null(getShinyOption("VOSONPkgMsgs"))) {
  # pkgMsgs <- .VOSONPkgMsgs
  pkgMsgs <- getShinyOption("VOSONPkgMsgs")
}

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

vpopover <- function(title, content) {
  div(HTML(paste("<a href = \"#\"",
                 "class = \"popover-link\"",
                 "data-toggle = \"popover\"",
                 "data-container = \"body\"",
                 "data-content = \"", content, "\"",
                 "data-html = \"true\"",
                 "data-trigger = \"focus\"",
                 "tabindex = \"0\"",
                 #"data-original-title = \"\"",
                 "title = \"", title, "\"",
                 ">",
                 "<i class=\"fa fa-question-circle\" style = \"font-size:0.90em;vertical-align:top;\"></i></a>"))
      , style = "width:4px;display:inline-block;")
}

gbl_scroll_delay <- 250 # ms
gbl_scroll_console <- "
  shinyjs.scroll_console = function(id) {
    $('#' + id + '').scrollTop($('#' + id + '')[0].scrollHeight);
  }" 

disable_tab_jscode <- "
  shinyjs.disableTab = function(name) {
    var tab = $('.nav li a[data-value=' + name + ']');
    tab.bind('click.tab', function(e) {
      e.preventDefault();
      return false;
    });
    tab.addClass('disabled');
  }"

disable_tab_css <- "
  .nav li a.disabled {
    background-color: #f5f5f5 !important;
    color: #444 !important;
    cursor: not-allowed !important;
    border-color: #f5f5f5 !important;
  }"

# collection
gbl_def_tweet_count <- 100
gbl_def_youtube_count <- 200

source("ui/popovers.R")

# modules
source("modules/collectionModule.R")
source("modules/textAnalysisModule.R")
