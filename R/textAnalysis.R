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

#' Creates a Word Frequency chart
#'
#' @param corp text corpus
#' @param min_freq minimum word frequency for word frequency charts
#' @param top_count number of words to render in word frequency charts
#' @param pcolors color list to use in the plot
#' 
#' @return barchart plot
#' @keywords internal
#' 
#' @export
wordFreqChart <- function(corp, min_freq, top_count, pcolors = NULL) {

  # returns empty plot with message if no data to chart
  if (is.null(corp) || length(corp) < 1) {
    return(emptyPlotMessage("No text data."))
  }
  
  # min freq uses bounds control
  dtm <- tm::DocumentTermMatrix(corp, control = list(wordLengths = c(3, 20), bounds = list(global = c(min_freq, Inf))))
  dtm_sparse_removed <- tm::removeSparseTerms(dtm, 0.98)
  
  freq_terms <- colSums(as.matrix(dtm_sparse_removed))
  order_terms <- order(freq_terms, decreasing = TRUE)
  
  par(mar = rep(0, 4))
  return(barchart(freq_terms[order_terms[1:top_count]], col = pcolors, xlab = "Frequency"))
}

#' Creates a Sentiment Analysis chart
#'
#' @param corp text corpus
#' @param pcolors color list to use in the plot
#' 
#' @return barchart plot
#' @keywords internal
#' 
#' @export
wordSentChart <- function(corp, pcolors = NULL) {

  # returns empty plot with message if no data to chart
  if (is.null(corp) || length(corp) < 1) {
    return(emptyPlotMessage("No text data."))
  }
  
  ws_df <- data.frame(content = unlist(sapply(corp, `[`, "content")), stringsAsFactors = FALSE)

  nrc_sent_df <- syuzhet::get_nrc_sentiment(unlist(ws_df[, 1]))
  nrc_sent_df$neutral <- ifelse(nrc_sent_df$negative + nrc_sent_df$positive == 0, 1, 0)
  chart_data <- 100 * colSums(nrc_sent_df) / sum(nrc_sent_df)
  
  colx <- pcolors
  if (is.null(colx)) {
    colx <- "f5f5f5"
  }
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
#' @param corp text corpus
#' @param seed value to seed rendering of word clouds
#' @param min_freq minimum word frequency for word clouds
#' @param max_words maximum number of words to render in word clouds
#' @param pcolors color list to use in the plot
#' 
#' @return wordcloud plot
#' @keywords internal
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
  wordcloud::wordcloud(corp, min.freq = min_freq, max.words = max_words, random.order = FALSE, colors = pcolors)
}
