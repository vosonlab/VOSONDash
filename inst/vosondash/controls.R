#### batch reset, enable and disable graph contols ####

disableGraphFilterControls <- function() {
  ui_controls <- c("graph_isolates_check",
                   "graph_multi_edge_check",
                   "graph_loops_edge_check",
                   "graph_names_check",
                   "graph_cat_select",
                   "graph_sub_cats_select",
                   "graph_node_size_degree_select", 
                   "analysis_graphml_download_button",
                   "graph_reseed_button",
                   "graph_layout_select", 
                   "graph_spread_slider",
                   "graph_component_type_select",
                   "graph_component_slider")
  
  sapply(ui_controls, function(x) { shinyjs::disable(x) })  
}

resetEnableGraphFilterControls <- function() {
  ui_controls <- c("graph_isolates_check", 
                   "graph_multi_edge_check", 
                   "graph_loops_edge_check", 
                   "graph_names_check", 
                   "graph_node_size_degree_select", 
                   "graph_sub_cats_select", 
                   "graph_layout_select", 
                   "graph_spread_slider",
                   "graph_component_type_select")
  
  sapply(ui_controls, function(x) { shinyjs::reset(x)
    shinyjs::enable(x) })
}

disableTextAnalysisControls <- function() {
  ui_controls <- c("ta_stopwords_check",
                   "ta_user_stopwords_input", 
                   "ta_user_stopwords_check",
                   "ta_twitter_hashtags_check", 
                   "ta_twitter_usernames_check",
                   "ta_stem_check",
                   "ta_wf_top_count",
                   "ta_wf_min_word_freq",
                   "ta_wc_min_word_freq",
                   "ta_wc_max_word_count",
                   "ta_cc_max_word_count")
  
  sapply(ui_controls, function(x) { shinyjs::disable(x) })
}

resetEnableTextAnalysisControls <- function() {
  ui_controls <- c("ta_stopwords_check",
                   "ta_user_stopwords_input", 
                   "ta_user_stopwords_check",
                   "ta_twitter_hashtags_check",
                   "ta_twitter_usernames_check", 
                   "ta_stem_check",
                   "ta_wf_top_count",
                   "ta_wf_min_word_freq",
                   "ta_wc_min_word_freq",
                   "ta_wc_max_word_count",
                   "ta_cc_max_word_count")
  
  sapply(ui_controls, function(x) { shinyjs::reset(x)
    shinyjs::enable(x) })
}

enablePlotControls <- function() {
  shinyjs::disable("graph_download_button")
  
  # added "graph_multi_edge_check", "graph_loops_edge_check" for bug switching back to plot from visnetwork
  # and multi, loops checkbox remaining disabled
  ui_controls <- c("graph_names_check",
                   "graph_reseed_button",
                   "graph_layout_select",
                   "graph_node_size_degree_select",
                   "graph_node_size_slider",
                   "graph_spread_slider",
                   "graph_multi_edge_check",
                   "graph_loops_edge_check")
  
  sapply(ui_controls, function(x) { shinyjs::enable(x) })
}

enableD3Controls <- function() {
  shinyjs::enable("graph_download_button")
  
  ui_controls <- c("graph_names_check",
                   "graph_reseed_button",
                   "graph_layout_select",
                   "graph_node_size_degree_select",
                   "graph_node_size_slider",
                   "graph_spread_slider")
  
  sapply(ui_controls, function(x) { shinyjs::disable(x) })
}

enableVisNetworkControls <- function() {
  shinyjs::enable("graph_download_button")
  
  ui_controls <- c("graph_names_check", 
                   "graph_multi_edge_check", 
                   "graph_loops_edge_check", 
                   "graph_spread_slider")
  
  sapply(ui_controls, function(x) { shinyjs::disable(x) })
}
