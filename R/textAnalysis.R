#' @title Create an empty plot with text
#' 
#' @description This function creates an empty plot that can be used to display a text message. Intended to be used in 
#' a series of plots to indicate that an individual plot cannot be created for some reason and still maintain a plot 
#' aesthetic.
#' 
#' @param message Character string. Text message to centre on empty plot. Default text is \code{"No plot available."}.
#'
#' @return An empty plot with text message.
#' 
#' @keywords internal
#' @export
emptyPlotMessage <- function(message = "No plot available.") {
  return({ plot(1:10, 1:10, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(5, 5, message, cex = 1.2) })
}

#' @title Create a Word Frequency chart
#'
#' @description This function creates a horizontal barchart of word frequencies in a text corpus.
#' 
#' @param corp \pkg{tm} package document \code{\link[tm]{Corpus}} object.
#' @param min_freq Numeric. Minimum frequency for a word to be included in the chart. Default is \code{1}.
#' @param top_count Numeric. Top count of words to render in word frequency chart. Default is \code{20}.
#' @param pcolors List. Colors to assign categorical variable in the plot. Default is \code{NULL}.
#' 
#' @return A barchart plot.
#' 
#' @export
wordFreqChart <- function(corp, min_freq = 1, top_count = 20, pcolors = NULL) {

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
  
  saved_par <- par(no.readonly = TRUE)
  on.exit(par(saved_par))
  
  par(mar = rep(0, 4))
  return(barchart(freq_terms[order_terms[1:top_count]], 
                  col = pcolors, 
                  xlab = "Frequency"))
}

#' @title Create an NRC Positive and Negative Sentiment valence chart
#'
#' @description This function creates a vertical barchart of negative, positive and the sum of the sentiment values or
#' valence in a text corpus.
#' 
#' @note Uses the \pkg{syuzhet} package implementation of Saif Mohammad’s NRC Emotion lexicon.
#' 
#' @param corp \pkg{tm} package document \code{\link[tm]{Corpus}} object.
#' 
#' @return A barchart plot.
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
  
  saved_par <- par(no.readonly = TRUE)
  on.exit(par(saved_par))
  
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

#' @title Create an NRC Emotion chart 
#' 
#' @description This function creates a horizontal barchart measuring and sorting the eight NRC lexicon emotions in 
#' the text corpus. Emotions are measured as the proportion of the total value of the eight emotions in the text as a 
#' percentage.
#' 
#' @note Uses the \pkg{syuzhet} package implementation of Saif Mohammad’s NRC Emotion lexicon.
#' 
#' @param corp \pkg{tm} package document \code{\link[tm]{Corpus}} object.
#' @param pcolors List. Colors to assign categorical variable in the plot. Default is \code{NULL}.
#' 
#' @return A barchart plot.
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
  
  saved_par <- par(no.readonly = TRUE)
  on.exit(par(saved_par))
  
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

#' @title Create a Word Cloud plot
#'
#' @description This function creates a wordcloud plot of words in a text corpus.
#' 
#' @param corp \pkg{tm} package document \code{\link[tm]{Corpus}} object.
#' @param seed Numeric. Seed value can be supplied to reproduce a word cloud layout.
#' @param min_freq Numeric. Minimum word frequency to include a word in the word cloud. Default is \code{1}.
#' @param max_words Numeric. Maximum number of words to render in the word cloud. Default is \code{50}.
#' @param pcolors List. Colors to assign categorical variable in the plot or palette to use if \code{random.color}.
#' Default is \code{NULL}.
#' @inheritDotParams wordcloud::wordcloud random.order random.color rot.per
#' 
#' @return A wordcloud plot.
#' 
#' @export
wordCloudPlot <- function(corp, seed = NULL, min_freq = 1, max_words = 50, pcolors = NULL, ...) {
  
  # returns empty plot with message if no data to plot
  if (is.null(corp) || length(corp) < 1) {
    return(emptyPlotMessage("No text data."))
  }
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  saved_par <- par(no.readonly = TRUE)
  on.exit(par(saved_par))
  
  plot_parameters <- list(words = corp, 
                          min.freq = min_freq,
                          max.words = max_words)

  plot_parameters[['colors']] <- pcolors

  dots <- list(...)
  plot_parameters <- append(plot_parameters, dots)
  
  par(mar = rep(0, 4))
  do.call(wordcloud::wordcloud, plot_parameters)
}
