#' VOSON Dashboard textAnalysisServer
#'
#' Network text analysis and filters. Word frequency and cloud visualisations.
#'

#### values ---------------------------------------------------------------------------------------------------------- #

ta_rv <- reactiveValues(
  plot_data_list = list(),          # list of base text corpus data sets
  has_text = FALSE,                 # does graphml have voson text data
  txt_attr_type = "",               # is text atttribute a vertex or edge
  txt_attr_name = "",               # text attribute name in graphml
  wc_seed = 100                     # wordcloud seed value
)

#### events ---------------------------------------------------------------------------------------------------------- #

# ui has a disable issue
disableTextAnalysisControls()

# enable text analysis tab if text data 
observeEvent(ta_rv$has_text, {
  if (ta_rv$has_text == FALSE) {
    addCssClass(selector = "a[data-value = 'text_analysis_tab']", class = "inactive_menu_link")
  } else {
    removeCssClass(selector = "a[data-value = 'text_analysis_tab']", class = "inactive_menu_link")
  }
})

# text analysis tab events
observeEvent(input$selected_text_analysis_tab, {
  plotWordFrequencies()
  plotWordClouds()
  if (input$selected_text_analysis_tab == "Sentiment") {
    plotSentiments()  
  }
}, ignoreInit = TRUE)

# enter text analysis section or controls toggled
# generate plots data and plot
observeEvent({ input$sidebar_menu
  input$ta_stopwords_check
  input$ta_twitter_hashtags_check
  input$ta_twitter_usernames_check
  input$ta_stem_check
  input$ta_word_length_slider
  input$ta_rem_url_check
  input$ta_rem_num_check
  input$ta_html_decode_check
  input$ta_iconv_check
  input$ta_rem_punc_check }, {
    
    if (input$sidebar_menu == "text_analysis_tab") {
      taPlotListData()
      plotWordFrequencies()
      plotWordClouds()
      if (input$selected_text_analysis_tab == "Sentiment") {
        plotSentiments()  
      }
    }
  }, ignoreInit = TRUE)

# replot when word cloud sliders change
observeEvent({ input$ta_wc_min_word_freq
  input$ta_wc_max_word_count
  input$wc_random_col
  input$ta_wc_vert_prop }, {
    
    plotWordClouds()           
  }, ignoreInit = TRUE)

# replot when word frequency sliders change
observeEvent({ input$ta_wf_top_count
  input$ta_wf_min_word_freq }, {
    
    plotWordFrequencies()           
  }, ignoreInit = TRUE)

# generate plots data and plot if user stop words selected
observeEvent(input$ta_user_stopwords_check, {
  if (input$ta_user_stopwords_check == TRUE && !nchar(input$ta_user_stopwords_input)) {
    showModal(modalDialog("You need to enter some stopwords!"))
    updateCheckboxInput(session, "ta_user_stopwords_check", value = FALSE)
  } else {
    taPlotListData()
    plotWordFrequencies()
    plotWordClouds()
    if (input$selected_text_analysis_tab == "Sentiment") {
      plotSentiments()  
    }
  }
}, ignoreInit = TRUE)

observeEvent(input$ta_user_stopwords_input, {
  updateCheckboxInput(session, "ta_user_stopwords_check", value = FALSE)
})

observeEvent(input$wc_reseed_button, {
  ta_rv$wc_seed <- sample(gbl_rng_range[1]:gbl_rng_range[2], 1)
  
  plotWordClouds()
}, ignoreInit = TRUE)

observeEvent(ta_rv$wc_seed, {
  html("wc_seed", ta_rv$wc_seed)
})

#### output ----------------------------------------------------------------------------------------------------------- #

output$ta_details_output <- renderText({
  textAnalysisDetailsOutput()
})

output$comparison_cloud_plot <- renderPlot({
  saved_par <- par(no.readonly = TRUE)
  on.exit(par(saved_par))
  
  par(mar = rep(0, 4))
  comparisonCloudPlotData()
})

#### reactives -------------------------------------------------------------------------------------------------------- #

# plot word frequencies
plotWordFrequencies <- reactive({
  top_count <- input$ta_wf_top_count
  min_freq <- input$ta_wf_min_word_freq
  word_len <- input$ta_word_length_slider
  mac_arial <- input$macos_font_check
  
  # create placeholders and plot word frequency charts from list of base text corpus data
  withProgress(message = "Processing word frequencies...", {
    callModule(taPlotPlaceholders, "word_freqs", ta_rv$plot_data_list)
    callModule(taPlotList, "word_freqs", ta_rv$plot_data_list, NULL, isolate(ng_rv$graph_cats), 
               min_freq, NULL, top_count, "wf", col_palette = gbl_plot_palette(),
               word_len, mac_arial)
  })
})

plotSentiments <- reactive({
  word_len <- input$ta_word_length_slider
  
  # create placeholders and plot charts from list of base text corpus data
  withProgress(message = "Processing sentiment...", {
    callModule(taPlotPlaceholders, "word_sentiments", ta_rv$plot_data_list, sub_plots = 2)
    callModule(taPlotList, "word_sentiments", ta_rv$plot_data_list, NULL, isolate(ng_rv$graph_cats), 
               NULL, NULL, NULL, "ws", col_palette = gbl_plot_palette(),
               word_len)
  })
})

# plot word clouds
plotWordClouds <- reactive({
  min_freq <- input$ta_wc_min_word_freq
  max_words <- input$ta_wc_max_word_count
  wc_seed <- ta_rv$wc_seed
  wc_random_order <- FALSE
  wc_random_col <- input$wc_random_col
  wc_vert_prop <- (input$ta_wc_vert_prop/100)
  word_len <- input$ta_word_length_slider
  mac_arial <- input$macos_font_check
  
  # create placeholders and plot word clouds from list of base text corpus data
  withProgress(message = "Processing word clouds...", {      
    callModule(taPlotPlaceholders, "word_clouds", ta_rv$plot_data_list)
    callModule(taPlotList, "word_clouds", ta_rv$plot_data_list, isolate(ng_rv$graph_seed), 
               isolate(ng_rv$graph_cats), min_freq, max_words, NULL, "wc",
               col_palette = gbl_plot_palette(),
               word_len, mac_arial,
               wc_seed, wc_random_order, wc_random_col, wc_vert_prop)
  })
})

# plot a combined comparison word cloud
# does not yet support selected subset category comparison as they are combined in ta_rv$plot_data_list
comparisonCloudPlotData <- reactive({
  plot_data_list <- ta_rv$plot_data_list
  # cats <- isolate(ng_rv$graph_cats)
  
  max_words <- input$ta_cc_max_word_count
  word_len <- input$ta_word_length_slider
  mac_arial <- setArialUnicodeMS(input$macos_font_check)
  
  if (is.null(plot_data_list)) { return(VOSONDash::emptyPlotMessage("No text data.")) }
  
  if (length(plot_data_list) == 1) {
    VOSONDash::emptyPlotMessage("No comparison plot: requires a Categorical variable and selected View as \"All\".")
  } else {
    # to get a comparison cloud, need new corpus with N documents where N is the number of categories
    #    i.e. collapse all content for each category into single document
    #
    # probably better way to do this, but for time being...
    df <- NULL
    for (i in 2:length(plot_data_list)) {   # first corpus in list is "All"
      df_t <- data.frame(text = unlist(sapply(plot_data_list[[i]]$corp, `[`, "content")), stringsAsFactors = FALSE)
      
      tmp <- paste0(unlist(plot_data_list[[i]]$graph_attr$sub_cats), collapse = " / ")
      df <- rbind(df, data.frame(catval = tmp, text = paste(df_t$text, collapse = " ", stringsAsFactors = FALSE)))
    }
    
    corp <- tm::VCorpus(tm::VectorSource(df$text))
    corp <- tm::tm_map(corp, tm::stripWhitespace)   # do not need to do any more text processing, already done in base corpus
    tdm <- tm::TermDocumentMatrix(corp, control = list(wordLengths = word_len))
    tdm <- removeSparseTerms(tdm, 0.99)
    tdm <- as.matrix(tdm)
    colnames(tdm) <- df$catval
    
    if (ncol(tdm) < 2) {
      VOSONDash::emptyPlotMessage("No comparison plot: only one categorical variable present.")
    } else {
      plot_parameters <- list(tdm,
                              max.words = max_words,
                              random.order = FALSE,
                              use.r.layout = FALSE, 
                              title.size = 2, 
                              colors = gbl_plot_palette())
      
      if (!is.null(mac_arial)) { plot_parameters['family'] <- mac_arial }
      
      do.call(wordcloud::comparison.cloud, plot_parameters)
    }
  }
})

getFiltersDesc <- reactive({
  output <- c()
  
  if (ng_rv$graph_cat_selected != "All") {
    if (!("All" %in% input$graph_sub_cats_select)) {
      output <- append(output, paste0(input$graph_sub_cats_select, collapse = ', '))
    }
  }
  
  output <- append(output, paste0("Filter Component Size: ", 
                                  input$graph_component_slider[1], " - ", 
                                  input$graph_component_slider[2]))
})

# text analysis summary
textAnalysisDetailsOutput <- reactive({
  g <- graphFilters()
  plot_data_list <- ta_rv$plot_data_list
  
  output <- c()
  
  if (!is.null(g)) {
    graph_clusters <- components(g, mode = input$graph_component_type_select) # moved here from below
    
    selected_sub_cats <- input$graph_sub_cats_select
    if (length(selected_sub_cats) == 1 && selected_sub_cats == "All") {
      output <- append(output, c("All Categories"))
    } else {
      output <- append(output, c(paste0("Filter Categories (", ng_rv$graph_cat_selected, "):")))
    }
    
    output <- append(output, getFiltersDesc())
    
    output <- append(output, c(paste0("Components (", input$graph_component_type_select, "): ", graph_clusters$no),
                               paste("Nodes:", vcount(g)),
                               paste("Edges:", ecount(g)), ""))
    
    if (ta_rv$has_text) {
      output <- append(output, c(paste("Text attribute type:", ta_rv$txt_attr_type),
                                 paste("Text attribute name:", ta_rv$txt_attr_name), "",
                                 "Word Counts", "-----------",
                                 paste("Stopwords:", input$ta_stopwords_check)))
      
      if (length(plot_data_list) > 0) {
        data_names <- names(plot_data_list)
        
        for (i in seq_along(plot_data_list)) {
          title_cat <- unlist(plot_data_list[[i]]$graph_attr$cat)
          title_attr <- unlist(plot_data_list[[i]]$graph_attr$sub_cats)
          title <- ""
          if (trimws(title_cat) != "") {
            title <- paste0(title, title_cat, " - ", sep = "")
          }
          title <- paste0(title, paste0(title_attr, collapse = ' / '), "", sep = "")
          output <- append(output, title)
          
          isolate({
            wf <- wordFreqFromCorpus(plot_data_list[[i]]$corp,
                                     word_len = input$ta_word_length_slider)
          })
          
          output <- append(output, paste("Words:", sum(wf$freq)))
          output <- append(output, "")
        }
      }
    }else{
      output <- append(output, "There is no text data.")
    }
  }else {
    output <- append(output, "No data.")
  }
  
  paste0(output, collapse = '\n')
})

# create named list of base text corpus data sets for categories
# 
# named list item data structure is:
#     ta_rv$plot_data_list[plot-id] <- list(graph_attr = list(attribute_name, attribute_value), VCorpus)
# 
# i.e data with text and no categorical attributes:
#     ta_rv$plot_data_list["plot-all"] <- list(list("", "Non-categorical Text"), VCorpus)
#
# i.e data with text and categorical attributes:
#     ta_rv$plot_data_list["plot-all"] <- list(list("", "All Categories"), VCorpus)
#     ta_rv$plot_data_list["plot-1"] <- list(list("Type", "Bio"), VCorpus)
#     ...
#
taPlotListData <- reactive({
  category_list <- ng_rv$graph_cats
  selected_cat <- ng_rv$graph_cat_selected
  selected_sub_cats <- input$graph_sub_cats_select
  
  ta_rv$plot_data_list <- NULL
  
  # remove All if other values selected
  if ("All" %in% selected_sub_cats && length(selected_sub_cats) > 1) {
    selected_sub_cats <- selected_sub_cats[selected_sub_cats != "All"]
  }
  
  withProgress(message = "Processing corpus...", {
    
    if (selected_cat == "All" || selected_sub_cats == "All") {
      # if no categories then one corpus for text data
      # if categories present then first corpus is for all categories
      # first corpus id in plot_list_data is "plot-all"
      all_plot_title <- list("", c("Non-categorical Text"))
      if (length(names(category_list)) > 0) {
        all_plot_title <- list("", c("All Categories"))
      }
      ta_rv$plot_data_list[["plot-all"]] <- taTextCorpusData(graph_attr = all_plot_title) # <<
    }
    
    # create plot id and corpus for each category
    # id in plot_list_data is numbered "plot-1", "plot-2"
    # corpus is created by the taTextCorpusData for each selected attribute in graph category
    if (!(selected_cat %in% c("All", ""))) {
      value_list <- category_list[[selected_cat]]
      plot_counter <- 1
      
      # corpus created for all attributes in graph category
      if ("All" %in% selected_sub_cats && length(selected_sub_cats) == 1) {
        for (i in 1:length(value_list)) {
          local({
            local_i <- i
            
            attribute_name <- selected_cat
            attribute_value <- value_list[local_i]
            
            plot_id <- paste0("plot-", plot_counter, sep = "")
            ta_rv$plot_data_list[[plot_id]] <<- taTextCorpusData(graph_attr = list(attribute_name, attribute_value))
          })
          
          plot_counter <- plot_counter + 1
        }
        
      } else {
        attribute_name <- selected_cat
        attribute_value <- selected_sub_cats
        
        ta_rv$plot_data_list[["plot-1"]] <- taTextCorpusData(graph_attr = list(attribute_name, attribute_value)) # <<
      }
    }
    
  })
})

#### functions ------------------------------------------------------------------------------------------------------- #

taTextCorpusData <- function(graph_attr) {
  g <- graphFilters()
  
  if (is.null(g)) { return(NULL) }
  
  plot_cat <- plot_sub_cats <- ""
  
  if (missing(graph_attr)) {
    graph_attr <- NULL
  } else {
    plot_cat <- graph_attr[1]
    plot_sub_cats <- graph_attr[[2]]
  }
  
  if (plot_cat != "") { g <- VOSONDash::applyCategoricalFilters(g, plot_cat, plot_sub_cats) }
  
  # voson text attributes
  attr_v <- igraph::vertex_attr_names(g)
  attr_v <- attr_v[grep(voson_txt_prefix, attr_v, perl = TRUE)]
  attr_e <- igraph::edge_attr_names(g)
  attr_e <- attr_e[grep(voson_txt_prefix, attr_e, perl = TRUE)]
  
  ta_rv$has_text <- FALSE
  if (length(attr_v)) {
    attr <- c(attr_v[1], 'vertex')
    ta_rv$has_text <- TRUE
  } else if (length(attr_e)) {
    i <- attr_e[1]
    attr <- c(attr_e[1], 'edge')
    ta_rv$has_text <- TRUE
  }
  
  if (ta_rv$has_text) {
    ta_rv$txt_attr_type <- attr[2]
    ta_rv$txt_attr_name <- gsub(voson_txt_prefix, "", attr[1]) # "vosonTxt_"
    
    sw <- usw <- NULL
    if (input$ta_stopwords_check) { sw <- "SMART" }
    if (input$ta_user_stopwords_check) { usw <- input$ta_user_stopwords_input }
    
    corp <- VOSONDash::corpusFromGraph(g,
                                       txt_attr = attr[1],
                                       type = ta_rv$txt_attr_type,
                                       iconv = input$ta_iconv_check,
                                       html_decode = input$ta_html_decode_check,
                                       rm_url = input$ta_rem_url_check,
                                       rm_num = input$ta_rem_num_check,
                                       rm_punct = input$ta_rem_punc_check,
                                       rm_twit_hashtags = input$ta_twitter_hashtags_check,
                                       rm_twit_users = input$ta_twitter_usernames_check,
                                       sw_kind = sw,
                                       rm_words = usw,
                                       stem = input$ta_stem_check)
    
    return(list(graph_attr = list(cat = graph_attr[1], sub_cats = graph_attr[2]), 
                corp = corp))
  } else {
    return(NULL)
  }
}
