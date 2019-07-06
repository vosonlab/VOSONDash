#' VOSON Dashboard textAnalysisServer
#'
#' Network text analysis and filters. Word frequency and cloud visualisations.
#'

#### values ----------------------------------------------------------------------------------------------------------- #

ta_rvalues <- reactiveValues(
  plot_list_data = list(),          # list of base text corpus data sets
  has_text = FALSE,                 # does graphml have voson text data
  attr_type = "",                   # is text atttribute a vertex or edge
  attr_name = ""                    # text attribute name in graphml
)

#### events ----------------------------------------------------------------------------------------------------------- #

# ui has a disable issue
disableTextAnalysisControls()

# enable text analysis tab if text data 
observeEvent(ta_rvalues$has_text, {
  if (ta_rvalues$has_text == FALSE) {
    addCssClass(selector = "a[data-value = 'text_analysis_tab']", class = "inactive_menu_link")
  } else {
    removeCssClass(selector = "a[data-value = 'text_analysis_tab']", class = "inactive_menu_link")
  }
})

# text analysis tab events
observeEvent(input$selected_text_analysis_tab, {
  plotWordFrequencies()
  plotWordClouds()
  plotSentiments()
}, ignoreInit = TRUE)

# enter text analysis section or controls toggled
# generate plots data and plot
observeEvent({ input$sidebar_menu
               input$text_analysis_stopwords_check
               input$text_analysis_twitter_hashtags_check
               input$text_analysis_twitter_usernames_check
               input$text_analysis_stem_check }, {

  if (input$sidebar_menu == "text_analysis_tab") {
    taPlotListData()
    plotWordFrequencies()
    plotWordClouds()
    plotSentiments()
  }
}, ignoreInit = TRUE)

# replot when word cloud sliders change
observeEvent({ input$text_analysis_wc_min_word_freq
               input$text_analysis_wc_max_word_count }, {
  
  plotWordClouds()           
}, ignoreInit = TRUE)

# replot when word frequency sliders change
observeEvent({ input$text_analysis_wf_top_count
               input$text_analysis_wf_min_word_freq }, {
    
  plotWordFrequencies()           
}, ignoreInit = TRUE)

# generate plots data and plot if user stop words selected
observeEvent(input$text_analysis_user_stopwords_check, {
  if (input$text_analysis_user_stopwords_check == TRUE && !nchar(input$text_analysis_user_stopwords_input)) {
    showModal(modalDialog("You need to enter some stopwords!"))
    updateCheckboxInput(session, "text_analysis_user_stopwords_check", value = FALSE)
  } else {
    taPlotListData()
    plotWordFrequencies()
    plotWordClouds()
    plotSentiments()
  }
}, ignoreInit = TRUE)

observeEvent(input$text_analysis_user_stopwords_input, {
  updateCheckboxInput(session, "text_analysis_user_stopwords_check", value = FALSE)
})

#### output ----------------------------------------------------------------------------------------------------------- #

output$text_analysis_details_output <- renderText({
  textAnalysisDetailsOutput()
})

output$comparison_cloud_plot <- renderPlot({
  par(mar = rep(0, 4))
  comparisonCloudPlotData()
})

#### reactives -------------------------------------------------------------------------------------------------------- #

# plot word frequencies
plotWordFrequencies <- reactive({
  top_count <- input$text_analysis_wf_top_count
  min_freq <- input$text_analysis_wf_min_word_freq
  
  # create placeholders and plot word frequency charts from list of base text corpus data
  withProgress(message = "Processing word frequencies...", {
    callModule(taPlotPlaceholders, "word_freqs", ta_rvalues$plot_list_data)
    callModule(taPlotList, "word_freqs", ta_rvalues$plot_list_data, NULL, isolate(ng_rvalues$graph_CA), 
               min_freq, NULL, top_count, "wf", col_palette = g_plot_palette())
  })
})

plotSentiments <- reactive({
  # create placeholders and plot charts from list of base text corpus data
  withProgress(message = "Processing sentiment...", {
    callModule(taPlotPlaceholders, "word_sentiments", ta_rvalues$plot_list_data)
    callModule(taPlotList, "word_sentiments", ta_rvalues$plot_list_data, NULL, isolate(ng_rvalues$graph_CA), 
               NULL, NULL, NULL, "ws", col_palette = g_plot_palette())
  })
})

# plot word clouds
plotWordClouds <- reactive({
  min_freq <- input$text_analysis_wc_min_word_freq
  max_words <- input$text_analysis_wc_max_word_count
  
  # create placeholders and plot word clouds from list of base text corpus data
  withProgress(message = "Processing word clouds...", {      
    callModule(taPlotPlaceholders, "word_clouds", ta_rvalues$plot_list_data)
    callModule(taPlotList, "word_clouds", ta_rvalues$plot_list_data, isolate(ng_rvalues$graph_seed), 
               isolate(ng_rvalues$graph_CA), min_freq, max_words, NULL, "wc", col_palette = g_plot_palette())
  })
})

# plot a combined comparison word cloud
# does not yet support selected subset category comparison as they are combined in ta_rvalues$plot_list_data
comparisonCloudPlotData <- reactive({
  data <- ta_rvalues$plot_list_data
  categories <- isolate(ng_rvalues$graph_CA)
  
  max_words <- input$text_analysis_cc_max_word_count
  
  if (is.null(data)) { return(VOSONDash::emptyPlotMessage("No text data.")) }
  
  if (length(data) == 1) {
    VOSONDash::emptyPlotMessage("No comparison plot: requires a Categorical variable and selected View as \"All\".")
  } else {
    # to get a comparison cloud, need new corpus with N documents where N is the number of categories
    #    i.e. collapse all content for each category into single document
    #
    # probably better way to do this, but for time being...
    df <- NULL
    for (i in 2:length(data)) {   # first corpus in list is "All"
      # df_t <- data.frame(text = unlist(sapply(data[[i]][[2]], `[`, "content")), stringsAsFactors = F)
      df_t <- data.frame(text = unlist(sapply(data[[i]]$corp, `[`, "content")), stringsAsFactors = FALSE)
      # tmp <- paste0(data[[i]][[1]][[2]], collapse = " / ")
      tmp <- paste0(data[[i]]$graph_attr[[2]], collapse = " / ")
      df <- rbind(df, data.frame(catval = tmp, text = paste(df_t$text, collapse = " ", stringsAsFactors = FALSE)))
    }
    
    corp <- VCorpus(VectorSource(df$text))
    corp <- tm_map(corp, stripWhitespace)   # do not need to do any more text processing, already done in base corpus
    tdm <- TermDocumentMatrix(corp, control = list(wordLengths = c(0, Inf)))
    # tdm <- removeSparseTerms(tdm, 0.98)
    tdm <- as.matrix(tdm)
    colnames(tdm) <- df$catval
    
    if (ncol(tdm) < 2) {
      VOSONDash::emptyPlotMessage("No comparison plot: only one categorical variable present.")
    } else {
      # colour seems to be correct but may need to revisit...
      comparison.cloud(tdm, 
                       max.words = max_words, 
                       random.order = FALSE, 
                       use.r.layout = FALSE, 
                       title.size = 2, 
                       colors = g_plot_palette())       
    }
  }
})

# added 2019-06-20
getFiltersDesc <- reactive({
  output <- c()
  
  if (ng_rvalues$graph_CA_selected != "All") {
    if (!("All" %in% input$graph_catAttr_attr_select)) {
      output <- append(output, paste0(input$graph_catAttr_attr_select, collapse = ', '))
    }
  }

  output <- append(output, paste0("Filter Component Size: ", input$graph_component_slider[1], " - ", 
                                  input$graph_component_slider[2]))
})

# text analysis summary
# 2019-06-20 need to review this behavior as to what is expected
textAnalysisDetailsOutput <- reactive({
  g <- graphFilters()
  # gnc <- g # graphFiltersNoCategorical() # 2019-06-20 this wasn't being used as from g not graphFiltersNoCategorical()
  list_data <- ta_rvalues$plot_list_data
  
  output <- c()
  
  if (!is.null(g)) { # 2019-06-20 !is.null(gnc)
    # graph_clusters_nc <- components(gnc, mode = input$graph_component_type_select) 
    # 2019-06-20 this wasn't being used as from g not graphFiltersNoCategorical()
    graph_clusters <- components(g, mode = input$graph_component_type_select) # moved here from below
    
    # added if statement 2019-06-20 this may not be working as intended (duplicated)
    selected_attr <- input$graph_catAttr_attr_select # added 2019-06-20
    if (length(selected_attr) == 1 && selected_attr == "All") {
      output <- append(output, c("All Categories"))
    } else {
      output <- append(output, c(paste0("Filter Categories (", ng_rvalues$graph_CA_selected, "):")))
    }
    
    output <- append(output, getFiltersDesc())
    
    output <- append(output, c(paste0("Components (", input$graph_component_type_select, "): ", graph_clusters$no),
                               paste("Nodes:", vcount(g)),
                               paste("Edges:", ecount(g)), ""))
    
    if (ta_rvalues$has_text) {
      output <- append(output, c(paste("Text attribute type:", ta_rvalues$attr_type),
                                 paste("Text attribute name:", ta_rvalues$attr_name), "",
                                 # paste("Seed:", isolate(ng_rvalues$graph_seed)), "",
                                 "Word Counts", "-----------",
                                 paste("Stopwords:", input$text_analysis_stopwords_check)))
      
      if (length(list_data) > 0) {
        # data_names <- names(sapply(list_data, names))
        data_names <- names(list_data)

        for (i in seq_along(list_data)) {
          # consider naming these lists
          title_cat <- unlist(list_data[[i]]$graph_attr$cat)
          title_attr <- unlist(list_data[[i]]$graph_attr$sub_cats)
          # title_cat <- list_data[[i]]$graph_attr[[1]]
          # title_attr <- list_data[[i]]$graph_attr[[2]]
          # title_cat <- list_data[[i]][[1]][[1]]
          # title_attr <- list_data[[i]][[1]][[2]]
          title <- ""
          if (trimws(title_cat) != "") {
            title <- paste0(title, title_cat, " - ", sep = "")
          }
          title <- paste0(title, paste0(title_attr, collapse = ' / '), "", sep = "")
          output <- append(output, title)
          # removing urls when building base corpus so do not require max word length
          # dtmx <- DocumentTermMatrix(list_data[[i]][[2]], control = list(wordLengths=c(3, 20)))
          # dtmx <- DocumentTermMatrix(list_data[[i]][[2]])
          dtmx <- DocumentTermMatrix(list_data[[i]]$corp)
          freq_terms <- colSums(as.matrix(dtmx))
          output <- append(output, paste("Words:", sum(freq_terms)))
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
#     ta_rvalues$plot_list_data[plot-id] <- list(graph_attr = list(attribute_name, attribute_value), VCorpus)
# 
# i.e data with text and no categorical attributes:
#     ta_rvalues$plot_list_data["plot-all"] <- list(list("", "Non-categorical Text"), VCorpus)
#
# i.e data with text and categorical attributes:
#     ta_rvalues$plot_list_data["plot-all"] <- list(list("", "All Categories"), VCorpus)
#     ta_rvalues$plot_list_data["plot-1"] <- list(list("Type", "Bio"), VCorpus)
#     ...
#
taPlotListData <- reactive({
  # graphFiltersNoCategorical() # 2019-06-22 commented out
  # input$text_analysis_stopwords_check
  
  category_list <- ng_rvalues$graph_CA
  selected_value <- ng_rvalues$graph_CA_selected
  selected_attr <- input$graph_catAttr_attr_select
  
  ta_rvalues$plot_list_data <<- NULL
  
  # 2019-06-21 remove All if other values selected
  if ("All" %in% selected_attr && length(selected_attr) > 1) {
    selected_attr <- selected_attr[selected_attr != "All"]
  }
  
  withProgress(message = "Processing corpus...", {
    
    # added if statement 2019-06-20 this may not be working as intended (plot duplicated)
    # if (length(selected_attr) == 1 && selected_attr == "All") {
    
    # added 2019-06-22
    if (selected_value == "All" || selected_attr == "All") {
      
      # if no categories then one corpus for text data
      # if categories present then first corpus is for all categories
      # first corpus id in plot_list_data is "plot-all"
      all_plot_title <- list("", c("Non-categorical Text"))
      if (length(names(category_list)) > 0) {
        all_plot_title <- list("", c("All Categories"))
      }
      ta_rvalues$plot_list_data[["plot-all"]] <<- taTextCorpusData(graph_attr = all_plot_title)
    
    }
    
    # create plot id and corpus for each category
    # id in plot_list_data is numbered "plot-1", "plot-2"
    # corpus is created by the taTextCorpusData for each selected attribute in graph category
    # if (length(names(category_list)) > 0 && selected_value != "All") {
    if (!(selected_value %in% c("All", ""))) {
      value_list <- category_list[[selected_value]]
      plot_counter <- 1
      
      # corpus created for all attributes in graph category
      if ("All" %in% selected_attr && length(selected_attr) == 1) { # 2019-06-21 length check
        for (i in 1:length(value_list)) {
          local({
            local_i <- i
            
            attribute_name <- selected_value
            attribute_value <- value_list[local_i]
            
            plot_id <- paste0("plot-", plot_counter, sep = "")
            ta_rvalues$plot_list_data[[plot_id]] <<- taTextCorpusData(graph_attr = list(attribute_name, attribute_value))
          })
          
          plot_counter <- plot_counter + 1
        }
        
      } else {
        attribute_name <- selected_value
        attribute_value <- selected_attr
        
        ta_rvalues$plot_list_data[["plot-1"]] <<- taTextCorpusData(graph_attr = list(attribute_name, attribute_value))
      }
    }
    
  })
})

#### functions -------------------------------------------------------------------------------------------------------- #

# return base text corpus data for category and attributes
# todo: parameters passed to function or add to module
taTextCorpusData <- function(graph_attr, simple = FALSE) {
  # g <- graphFiltersNoCategorical()
  g <- graphFilters()
  
  if (is.null(g)) { return(NULL) }
  
  plot_category <- plot_category_attr <- ""
  
  if (missing(graph_attr)) {
    graph_attr <- NULL
  } else {
    plot_category <- graph_attr[1]
    plot_category_attr <- graph_attr[[2]]
  }
  
  # create filtered graph object for category and attribute value passed to the function
  if (plot_category != "") {
    # g <- ng_rvalues$graph_data
    g <- graphFilters()
    # g <- applyPruneFilter(g, pruning_rvalues$prune_verts)
    # g <- applyGraphFilters(g, input$graph_isolates_check, input$graph_multi_edge_check, input$graph_loops_edge_check,
    #                        input$graph_component_type_select, input$graph_component_slider)
    # swapped order to place this filter last
    g <- VOSONDash::applyCategoricalFilters(g, plot_category, plot_category_attr)
  }
  
  # voson text attributes
  attr_v <- igraph::vertex_attr_names(g)
  attr_v <- attr_v[grep("^vosonTxt", attr_v, perl = T)]
  attr_e <- igraph::edge_attr_names(g)
  attr_e <- attr_e[grep("^vosonTxt", attr_e, perl = T)]
  
  ta_rvalues$has_text <<- FALSE
  if (length(attr_v)) {
    attr <- c(attr_v[1], 'vertex')
    ta_rvalues$has_text <<- TRUE
  } else if (length(attr_e)) {
    i <- attr_e[1]
    attr <- c(attr_e[1], 'edge')
    ta_rvalues$has_text <<- TRUE
  }
  
  if (ta_rvalues$has_text) {
    if (attr[2] == "vertex") {
      words <- igraph::vertex_attr(g, attr[1])
    } else {
      words <- igraph::edge_attr(g, attr[1])
    }
    
    ta_rvalues$attr_type <<- attr[2]
    ta_rvalues$attr_name <<- gsub("vosonTxt_", "", attr[1])
    
    toRemove <- which(words == "")
    if (isTRUE(length(toRemove) != 0)) {
      words <- words[-toRemove]
    }
    
    if (VOSONDash::isMac()) {
      words <- iconv(words, to = 'utf-8-mac')
    } else {
      words <- iconv(words, to = 'utf-8')
    }
    
    if (simple) {
      return(list(graph_attr, words))
    }
    
    corp <- tm::VCorpus(tm::VectorSource(words))
    corp <- tm::tm_map(corp, tm::content_transformer(tolower))
    corp <- tm::tm_map(corp, tm::content_transformer(remHTTP))
    
    if (input$text_analysis_twitter_hashtags_check == TRUE) {
      corp <- tm::tm_map(corp, tm::content_transformer(removeHashTags))
    }
    if (input$text_analysis_twitter_usernames_check == TRUE) {
      corp <- tm::tm_map(corp, tm::content_transformer(removeTwitterHandles))
    }

    if (!is.null(igraph::get.graph.attribute(g, "type"))) {
      if (igraph::get.graph.attribute(g, "type") == "twitter") {
        corp <- tm::tm_map(corp, tm::content_transformer(repHTMLApos))
        corp <- tm::tm_map(corp, tm::content_transformer(repHTMLQuote))
        corp <- tm::tm_map(corp, tm::content_transformer(repHTMLAmper))
        corp <- tm::tm_map(corp, tm::content_transformer(remPartAmpGt))
      }      
    }

    corp <- tm::tm_map(corp, removeNumbers)
    corp <- tm::tm_map(corp, removePunctuation)
    
    if (input$text_analysis_stopwords_check == TRUE) {
      corp <- tm::tm_map(corp, removeWords, tm::stopwords("english"), lazy = TRUE)
    }
    
    if (input$text_analysis_user_stopwords_check == TRUE) {
      sw <- tolower(input$text_analysis_user_stopwords_input)
      sw <- trimws(unlist(strsplit(sw, ",")))
      corp <- tm::tm_map(corp, removeWords, sw)
    }
    
    if (input$text_analysis_stem_check == TRUE) {
      corp <- tm::tm_map(corp, stemDocument)
    }
    
    corp <- tm::tm_map(corp, stripWhitespace, lazy = TRUE)
    
    # named items
    # return(list(graph_attr, corp))
    return(list(graph_attr = list(cat = graph_attr[1], 
                                  sub_cats = graph_attr[2]), 
                corp = corp))
  } else {
    return(NULL)
  }
}

remHTTP <- function(x) gsub("http[[:alnum:][:punct:]]*", "", x)   # removes http and https tokens
# remHTTP <- function(x) gsub("http[^[:space:]]*", "", x)         # might need if non-ascii characters in url

removeHashTags <- function(x) gsub("#\\S+", "", x)
removeTwitterHandles <- function(x) gsub("@\\S+", "", x)

# replace html encoding with escaped characters in twitter text
repHTMLApos <- function(x) gsub("&apos;", "\'", x)    # apostrophe
repHTMLQuote <- function(x) gsub("&quot;", "\"", x)   # quote
repHTMLAmper <- function(x) gsub("&amp;", "&", x)     # ampersand
remPartAmpGt <- function(x) gsub("amp;|gt;", "", x)   # 
