#' VOSON Dashboard Text Analysis Module
#'
#' Shiny module functions to create and render groups of word frequency charts or 
#' word cloud plots dynamically.
#'

#' Plot variables
#' todo: pass these in some other way
tamod_palette <- brewer.pal(8, "Dark2") # g_plot_palette
tamod_plot_height <- "450px"
tamod_wc_scale <- c(2, 0.5)

#' Create empty plot with text message.
#' 
#' @param message text message to print in centre of empty plot
#' 
#' @return plot and text code block
#' 
emptyPlotMessage <- function(message) {
  return({ plot(1:10, 1:10, type = "n", axes = F, xlab = "", ylab = "")
    text(5, 5, message, cex = 1.2) })
}

#' UI container for Text Analysis plots.
#' Creates namespaced UI output container "ta_plot".
#' 
#' @param id shiny module namespace id
#' 
#' @return None
#' 
#' @export
taPlotContainerUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ta_plot"))
}

#' Creates UI plot placeholders generated from VOSON Dashboard Text Analysis plot list data.
#' Renders plot placeholders in namespace UI output container "ta_plot".
#' 
#' @param input shiny module namespaced input parameter
#' @param output shiny module namespaced output parameter
#' @param session shiny module namespaced session parameter
#' @param data voson dashboard text analysis plot list data structure
#'   data structure is named list:
#'     data[plot-id] <- list(graph_attr = list(attribute_name, attribute_value), VCorpus)
#'
#' @return None
#' 
#' @export
taPlotPlaceholders <- function(input, output, session, data) {
  ns <- session$ns
  
  plotPlaceholders <- reactive({
    tag_list <- tagList()
    plot_ids <- names(sapply(data, names))
    
    # isolate({ # withProgress(message = "Processing placeholders...", {
    
    for (i in seq_along(data)) {
      title_cat <- data[[i]][[1]][[1]]
      title_attr <- paste0(data[[i]][[1]][[2]], collapse = " / ")
      
      title_tags <- fluidRow(column(width = 12, div(h4(title_cat, " ", title_attr), style = "padding-left:20px;")))
      tag_list <- tagAppendChild(tag_list, title_tags)
      
      plot_id <- ns(plot_ids[i])
      plot_tags <- fluidRow(column(width = 12, plotOutput(plot_id, height = tamod_plot_height)))
      tag_list <- tagAppendChild(tag_list, plot_tags)
    }
    
    # }) # })
    
    # creates a plot placeholder with the namespace id "no-data"
    if (length(tag_list) == 0) {
      tag_list <- tagAppendChild(tag_list, fluidRow(column(width = 12, plotOutput(ns("no-data"),
                                                                                  height = tamod_plot_height))))
    }
    
    return(tag_list)
  })
  
  output$ta_plot <- renderUI({
    plotPlaceholders()
  })
}

#' Renders plots to placeholders for Word Frequency or Word Cloud plots.
#' Generated from VOSON Dashboard Text Analysis plot list data structure.
#'
#' @param input shiny module namespaced input parameter
#' @param output shiny module namespaced output parameter
#' @param session shiny module namespaced session parameter
#' @param data voson dashboard text analysis plot list data structure
#'   data structure is named list:
#'     data[plot-id] <- list(graph_attr = list(attribute_name, attribute_value), VCorpus)
#' @param seed value to seed rendering of word clouds
#' @param categories list of all categorical attribures in the data set
#' @param min_freq minimum word frequency for word frequency charts or word clouds
#' @param max_words maximum number of words to render in word clouds
#' @param top_count number of words to render in word frequency charts
#' @param type code for type of plots "wf" word frequencies or "wc" word clouds
#' 
#' @return None
#'
#' @export
taPlotList <- function(input, output, session, data, seed, categories, min_freq, max_words, top_count, type) {
  ns <- session$ns
  
  wordFreqPlotList <- reactive({
    plot_ids <- names(sapply(data, names))
    
    isolate({ withProgress(message = "Processing frequency plots...", {
      
      for (i in seq_along(data)) {
        local({
          local_i <- i
          plot_id <- plot_ids[local_i]
          output[[plot_id]] <- renderPlot({
            wordFreqChart(data[[local_i]], categories, min_freq, top_count)
          })
        })
      }
      
      # renders empty plot to namespace id "no-data" placeholder
      if (length(data) < 1) {
        output[["no-data"]] <- renderPlot({
          emptyPlotMessage("No text data.")
        })
      }
      
    }) })
  })
  
  wordCloudPlotList <- reactive({
    plot_ids <- names(sapply(data, names))
    
    isolate({ withProgress(message = "Processing cloud plots...", {
      
      for (i in seq_along(data)) {
        local({
          local_i <- i
          plot_id <- plot_ids[local_i]
          output[[plot_id]] <- renderPlot({
            wordCloudPlot(data[[local_i]], seed, categories, min_freq, max_words)
          })
        })
      }
      
      # renders empty plot to namespace id "no-data" placeholder
      if (length(data) < 1) {
        output[["no-data"]] <- renderPlot({
          emptyPlotMessage("No text data.")
        })
      }
    }) })
  })

  wordSentPlotList <- reactive({
    plot_ids <- names(sapply(data, names))
    
    isolate({ withProgress(message = "Processing sentiment plots...", {
      
      for (i in seq_along(data)) {
        local({
          local_i <- i
          plot_id <- plot_ids[local_i]
          output[[plot_id]] <- renderPlot({
            wordSentChart(data[[local_i]], categories)
          })
        })
      }
      
      # renders empty plot to namespace id "no-data" placeholder
      if (length(data) < 1) {
        output[["no-data"]] <- renderPlot({
          emptyPlotMessage("No text data.")
        })
      }
      
    }) })
  })
  
  # determine type of plots to generate
  if (type == "wf") {
    wordFreqPlotList()
  } else if (type == "wc") {
    wordCloudPlotList()
  } else if (type == "ws") {
    wordSentPlotList()
  }
}

getColors <- function(categories, plot_category, plot_category_attrs, default_col) {
  if (plot_category != "") {
    df <- data.frame("cat" = categories[[plot_category]])
    
    # is this needed?
    ncats <- ifelse(nrow(df) == 0, 1, nrow(df))
    df$color <- tamod_palette[1:ncats]
    
    match_t <- match(plot_category_attrs, df$cat)
    colx <- ifelse(!is.na(match_t), df$color[match_t], default_col)
  } else {
    colx <- default_col
  }
}

#' Creates a Word Frequency chart
#'
#' @param data voson dashboard text analysis plot list data structure
#' @param categories list of all categorical attribures in the data set
#' @param min_freq minimum word frequency for word frequency charts
#' @param top_count number of words to render in word frequency charts
#' 
#' @return barchart
#' 
wordFreqChart <- function(data, categories, min_freq, top_count) {
  graph_attr <- data[[1]]
  corp <- data[[2]]
  
  plot_category <- plot_category_attrs <- ""
  plot_category <- graph_attr[[1]]
  plot_category_attrs <- graph_attr[[2]] # can be a list
  
  # min freq uses bounds control
  dtm <- DocumentTermMatrix(corp, control = list(wordLengths = c(3, 20), bounds = list(global = c(min_freq, Inf))))
  dtm_sparse_removed <- removeSparseTerms(dtm, 0.98)
  
  freq_terms <- colSums(as.matrix(dtm_sparse_removed))
  order_terms <- order(freq_terms, decreasing = TRUE)
  
  colx <- getColors(categories, plot_category, plot_category_attrs, "azure2")
  
  # returns empty plot with message if no data to chart
  if (is.null(corp) || length(corp) < 1) {
    return(emptyPlotMessage("No text data."))
  }
  par(mar = rep(0, 4))
  return(barchart(freq_terms[order_terms[1:top_count]], col = colx, xlab = "Frequency"))
}

wordSentChart <- function(data, categories) {
  graph_attr <- data[[1]]
  corp <- data[[2]]
  
  ws_df <- data.frame(content = unlist(sapply(corp, `[`, "content")), stringsAsFactors = FALSE)
  
  plot_category <- plot_category_attrs <- ""
  plot_category <- graph_attr[[1]]
  plot_category_attrs <- graph_attr[[2]] # can be a list

  nrc_sent_df <- get_nrc_sentiment(unlist(ws_df[, 1]))
  nrc_sent_df$neutral <- ifelse(nrc_sent_df$negative + nrc_sent_df$positive == 0, 1, 0)
  chart_data <- 100 * colSums(nrc_sent_df) / sum(nrc_sent_df)
  
  colx <- getColors(categories, plot_category, plot_category_attrs, "azure2")
  colx[seq(1, 8)] <- colx
  colx[9] <- "firebrick1"
  colx[10] <- "steelblue"
  colx[11] <- "gainsboro"
  
  # returns empty plot with message if no data to chart
  if (is.null(corp) || length(corp) < 1) {
    return(emptyPlotMessage("No text data."))
  }

  sent_plot <- barplot(chart_data, las = 2, col = colx, ylab = "Percentage")
  text(sent_plot, ifelse(chart_data <= 1, 2, chart_data - 1), labels = round(chart_data, digits = 2), col = "black")
  
  return(sent_plot)
}

#' Creates a Word Cloud plot
#'
#' @param data voson dashboard text analysis plot list data structure
#' @param seed value to seed rendering of word clouds
#' @param categories list of all categorical attribures in the data set
#' @param min_freq minimum word frequency for word clouds
#' @param max_words maximum number of words to render in word clouds
#' 
#' @return wordcloud
#' 
wordCloudPlot <- function(data, seed, categories, min_freq, max_words) {
  graph_attr <- data[[1]]
  corp <- data[[2]]
  
  plot_category <- plot_category_attrs <- ""
  plot_category <- graph_attr[[1]]
  plot_category_attrs <- graph_attr[[2]]
  
  colx <- getColors(categories, plot_category, plot_category_attrs, "black")
  
  # returns empty plot with message if no data to plot
  if (is.null(corp) || length(corp) < 1) {
    return(emptyPlotMessage("No text data."))
  }
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # colors = colx[factor(df$cat)]
  par(mar = rep(0, 4))
  return(wordcloud(corp, min.freq = min_freq, max.words = max_words, random.order = FALSE, colors = colx))
}