#' Create an empty plot with text
#' 
#' @param message text to place on plot character string 
#'
#' @return plot with text message
#' @keywords internal
#' 
#' @export
emptyPlotMessage <- function(message) {
  return({ plot(1:10, 1:10, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(5, 5, message, cex = 1.2) })
}

#' Creates a Word Frequency chart
#'
#' @param corp text corpus
#' @param min_freq minimum word frequency for word frequency charts
#' @param top_count number of words to render in word frequency charts
#' @param pcolors color list to use in the plot
#' 
#' @return barchart plot
#' 
#' @export
wordFreqChart <- function(corp, min_freq, top_count, pcolors = NULL) {

  # returns empty plot with message if no data to chart
  if (is.null(corp) || length(corp) < 1) {
    return(emptyPlotMessage("No text data."))
  }
  
  # min freq uses bounds control
  dtm <- tm::DocumentTermMatrix(corp, control = list(wordLengths = c(3, 20), 
                                                     bounds = list(global = c(min_freq, Inf))))
  dtm_sparse_removed <- tm::removeSparseTerms(dtm, 0.98)
  
  freq_terms <- colSums(as.matrix(dtm_sparse_removed))
  order_terms <- order(freq_terms, decreasing = TRUE)
  
  par(mar = rep(0, 4))
  return(barchart(freq_terms[order_terms[1:top_count]], 
                  col = pcolors, 
                  xlab = "Frequency"))
}

#' Creates a Sentiment Analysis Valence chart
#'
#' @param corp text corpus
#' 
#' @return barchart plot
#' 
#' @export
wordSentValenceChart <- function(corp) {
  # returns empty plot with message if no data to chart
  if (is.null(corp) || length(corp) < 1) {
    return(emptyPlotMessage("No text data."))
  }
  
  ws_df <- data.frame(content = unlist(sapply(corp, `[`, "content")), stringsAsFactors = FALSE)
  nrc_sent_df <- syuzhet::get_nrc_sentiment(unlist(ws_df[, 1]))
  
  chart_data <- colSums(nrc_sent_df[c(9:10)])
  valence <- ((nrc_sent_df[, 9]*-1) + nrc_sent_df[, 10])
  chart_data["valence"] <- sum(valence)
  chart_data["negative"] <- chart_data["negative"]*-1
  # chart_data["mean valence"] <- mean(valence)
  
  valence_col <- "gainsboro"
  if (sum(valence) < 0) {
    valence_col <- "lightcoral"
  } else if (sum(valence) > 0) {
    valence_col <- "mediumaquamarine"
  }
  
  colx <- c("lightcoral", "mediumaquamarine", valence_col) # "gainsboro"
  
  par(las = 2)
  par(mar = c(6, 4, 3, 1))
  
  sent_plot_summary <- barplot(chart_data,
                               col = colx, 
                               ylab = "Sum NRC Sentiment", 
                               horiz = FALSE, 
                               xpd = FALSE, 
                               axes = TRUE)
  
  title("Sentiment Valence", adj = 0, line = 1)
  
  text(y = ifelse(chart_data == 0, 0, chart_data/2),
       x = sent_plot_summary,
       labels = chart_data,
       col = "black",
       cex = 0.7)
  
  sent_plot_summary
}

#' Creates a Sentiment Analysis chart
#'
#' @param corp text corpus
#' @param pcolors color list to use in the plot
#' 
#' @return barchart plot
#' 
#' @export
wordSentChart <- function(corp, pcolors = NULL) {
  
  # returns empty plot with message if no data to chart
  if (is.null(corp) || length(corp) < 1) {
    return(emptyPlotMessage("No text data."))
  }
  
  ws_df <- data.frame(content = unlist(sapply(corp, `[`, "content")), stringsAsFactors = FALSE)
  nrc_sent_df <- syuzhet::get_nrc_sentiment(unlist(ws_df[, 1]))
  chart_data <- sort(colSums(prop.table(nrc_sent_df[, 1:8]))*100)
  
  colx <- pcolors
  if (is.null(colx)) {
    colx <- "#f5f5f5"
  }
  colx[seq(1, 8)] <- colx
  
  par(las = 2)
  par(mar = c(4, 6, 3, 1))
  
  sent_plot <- barplot(chart_data, 
                       col = colx, 
                       xlab = "Percentage %", 
                       horiz = TRUE, 
                       xlim = c(0, 100), 
                       xpd = FALSE, 
                       axes = TRUE)
  
  title("Emotions in Text", adj = 0, line = 1)
  
  text(x = chart_data, 
       y = sent_plot,
       labels = round(chart_data, digits = 2), 
       col = "black",
       cex = 0.8)
  
  sent_plot
}

#' Creates a Word Cloud plot
#'
#' @param corp text corpus
#' @param seed value to seed rendering of word clouds
#' @param min_freq minimum word frequency for word clouds
#' @param max_words maximum number of words to render in word clouds
#' @param pcolors color list to use in the plot
#' 
#' @return wordcloud plot
#' 
#' @export
wordCloudPlot <- function(corp, seed, min_freq, max_words, pcolors = NULL) {
  
  # returns empty plot with message if no data to plot
  if (is.null(corp) || length(corp) < 1) {
    return(emptyPlotMessage("No text data."))
  }
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  par(mar = rep(0, 4))
  wordcloud::wordcloud(corp, 
                       min.freq = min_freq, 
                       max.words = max_words, 
                       random.order = FALSE, 
                       colors = pcolors)
}
