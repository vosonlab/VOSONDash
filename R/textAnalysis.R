#' Create an empty plot with text
#' 
#' @param message text to place on plot character string 
#'
#' @return plot with text message
#' @keywords internal
#' 
#' @export
emptyPlotMessage <- function(message) {
  return({ plot(1:10, 1:10, type = "n", axes = F, xlab = "", ylab = "")
    text(5, 5, message, cex = 1.2) })
}

#' Get the colors to use in a plot based on categorical attribute ordering applied to palette
#'
#' @param categories list of all categorical attribures in the data set
#' @param plot_category selected plot category as character string
#' @param plot_category_attrs list of selected category attributes
#' @param default_col default color to use in plot
#' @param col_palette color palette to use in plot
#'  
#' @return color values as character vector
#' @keywords internal
#'
getColors <- function(categories, plot_category, plot_category_attrs, default_col, col_palette) {
  # needs re-working now that it is in a package
  if (plot_category != "") {
    df <- data.frame("cat" = categories[[plot_category]])
    
    # not sure about this
    if (nrow(df)) {
      # is this needed?
      ncats <- ifelse(nrow(df) == 0, 1, nrow(df))
      df$color <- col_palette[1:ncats]
      
      match_t <- match(plot_category_attrs, df$cat)
      colx <- ifelse(!is.na(match_t), df$color[match_t], default_col)
      
      return(colx)
    }
    
    return(default_col)
  }
  
  default_col
}

#' Creates a Word Frequency chart
#'
#' @param data voson dashboard text analysis plot list data structure
#'   data structure is named list:
#'     list(graph_attr = list(attribute_name, attribute_value), VCorpus)
#' @param categories list of all categorical attribures in the data set
#' @param min_freq minimum word frequency for word frequency charts
#' @param top_count number of words to render in word frequency charts
#' @param col_palette color palette to use in the plot
#' 
#' @return barchart
#' 
#' @export
wordFreqChart <- function(data, categories, min_freq, top_count, col_palette = NULL) {
  graph_attr <- data[[1]]
  corp <- data[[2]]

  # returns empty plot with message if no data to chart
  if (is.null(corp) || length(corp) < 1) {
    return(emptyPlotMessage("No text data."))
  }
  
  plot_category <- plot_category_attrs <- ""
  plot_category <- graph_attr[[1]]
  plot_category_attrs <- graph_attr[[2]] # can be a list
  
  # min freq uses bounds control
  dtm <- DocumentTermMatrix(corp, control = list(wordLengths = c(3, 20), bounds = list(global = c(min_freq, Inf))))
  dtm_sparse_removed <- removeSparseTerms(dtm, 0.98)
  
  freq_terms <- colSums(as.matrix(dtm_sparse_removed))
  order_terms <- order(freq_terms, decreasing = TRUE)
  
  if (is.null(col_palette)) {
    col_palette <- brewer.pal(8, "Dark2")
  }
  colx <- getColors(categories, plot_category, plot_category_attrs, "#f5f5f5", col_palette)
  
  par(mar = rep(0, 4))
  return(barchart(freq_terms[order_terms[1:top_count]], col = colx, xlab = "Frequency"))
}

#' Creates a Sentiment Analysis chart
#'
#' @param data voson dashboard text analysis plot list data structure
#'   data structure is named list:
#'     list(graph_attr = list(attribute_name, attribute_value), VCorpus)
#' @param categories list of all categorical attribures in the data set
#' @param col_palette color palette to use in the plot
#' 
#' @return barchart
#' 
#' @export
wordSentChart <- function(data, categories, col_palette = NULL) {
  graph_attr <- data[[1]]
  corp <- data[[2]]
  
  # returns empty plot with message if no data to chart
  if (is.null(corp) || length(corp) < 1) {
    return(emptyPlotMessage("No text data."))
  }
  
  ws_df <- data.frame(content = unlist(sapply(corp, `[`, "content")), stringsAsFactors = FALSE)
  
  plot_category <- plot_category_attrs <- ""
  plot_category <- graph_attr[[1]]
  plot_category_attrs <- graph_attr[[2]] # can be a list

  nrc_sent_df <- get_nrc_sentiment(unlist(ws_df[, 1]))
  nrc_sent_df$neutral <- ifelse(nrc_sent_df$negative + nrc_sent_df$positive == 0, 1, 0)
  chart_data <- 100 * colSums(nrc_sent_df) / sum(nrc_sent_df)
  
  if (is.null(col_palette)) {
    col_palette <- brewer.pal(8, "Dark2")
  }
  colx <- getColors(categories, plot_category, plot_category_attrs, "#f5f5f5", col_palette)
  colx[seq(1, 8)] <- colx
  colx[9] <- "lightcoral"
  colx[10] <- "mediumaquamarine"
  colx[11] <- "gainsboro"
  
  par(las = 2)
  par(mar = c(4, 6, 0, 4))

  sent_plot <- barplot(chart_data, col = colx, xlab = "Percentage %", horiz = TRUE, xlim = c(0, 100), xpd = FALSE, axes=TRUE)
  text(x = ifelse(chart_data <= 1, 1, chart_data+2.5), sent_plot, labels = round(chart_data, digits = 2), col = "black")
  
  sent_plot
}

#' Creates a Word Cloud plot
#'
#' @param data voson dashboard text analysis plot list data structure
#'   data structure is named list:
#'     list(graph_attr = list(attribute_name, attribute_value), VCorpus)
#' @param seed value to seed rendering of word clouds
#' @param categories list of all categorical attribures in the data set
#' @param min_freq minimum word frequency for word clouds
#' @param max_words maximum number of words to render in word clouds
#' @param col_palette color palette to use in the plot
#' 
#' @return wordcloud
#'
#' @export
wordCloudPlot <- function(data, seed, categories, min_freq, max_words, col_palette = NULL) {
  graph_attr <- data[[1]]
  corp <- data[[2]]
  
  # returns empty plot with message if no data to plot
  if (is.null(corp) || length(corp) < 1) {
    return(emptyPlotMessage("No text data."))
  }
  
  plot_category <- plot_category_attrs <- ""
  plot_category <- graph_attr[[1]]
  plot_category_attrs <- graph_attr[[2]]
  
  if (is.null(col_palette)) {
    col_palette <- brewer.pal(8, "Dark2")
  }  
  colx <- getColors(categories, plot_category, plot_category_attrs, "black", col_palette)
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # colors = colx[factor(df$cat)]
  par(mar = rep(0, 4))
  wordcloud(corp, min.freq = min_freq, max.words = max_words, random.order = FALSE, colors = colx)
}
