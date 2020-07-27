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

#' @title Create a word frequency chart
#'
#' @description This function creates a horizontal barchart of word frequencies.
#'
#' @param word_freqs Dataframe. Word frequencies.
#' @param min_freq Numeric. Minimum frequency for a word to be included in the chart. Default is \code{1}.
#' @param top_count Numeric. Top count of words to render in word frequency chart. Default is \code{20}.
#' @param pcolors List. Colors to assign categorical variable in the plot. Default is \code{NULL}.
#' @param family Character string. Set a font family for plot labels. Default is \code{NULL}.
#'
#' @return A barchart plot.
#'
#' @export
wordFreqChart <- function(word_freqs,
                          min_freq = 1,
                          top_count = 20,
                          pcolors = NULL,
                          family = NULL) {

  # returns empty plot with message if no data to chart
  if (is.null(word_freqs) || nrow(word_freqs) < 1) {
    return(emptyPlotMessage("No text data."))
  }

  saved_par <- par(no.readonly = TRUE)
  on.exit(par(saved_par))

  freq <- NULL
  word_freqs <- word_freqs[freq >= min_freq]
  word_freqs <- word_freqs[order(word_freqs, -freq)]
  word_freqs <- word_freqs[1:top_count, ]
  word_freqs$word <- factor(word_freqs$word, levels = rev(word_freqs$word))
  
  plot_parameters <- list(word ~ freq,
                          data = word_freqs,
                          col = pcolors,
                          xlab = "Frequency")
  
  if (!is.null(family)) { plot_parameters[['scales']] <- list(fontfamily = family) }
  
  par(mar = rep(0, 4))
  do.call(lattice::barchart, plot_parameters)
}

#' @title Create a wordcloud plot
#'
#' @description This function creates a wordcloud plot from word frequencies.
#'
#' @param word_freqs Table. Table of word frequencies.
#' @param seed Numeric. Seed value can be supplied to reproduce a word cloud layout.
#' @param min_freq Numeric. Minimum word frequency to include a word in the word cloud. Default is \code{1}.
#' @param max_words Numeric. Maximum number of words to render in the word cloud. Default is \code{50}.
#' @param pcolors List. Colors to assign categorical variable in the plot or palette to use if \code{random.color}.
#' Default is \code{NULL}.
#' @param family Character. Set a font family for plot labels. Default is \code{NULL}.
#' @inheritDotParams wordcloud::wordcloud random.order random.color rot.per
#'
#' @return A wordcloud plot.
#'
#' @export
wordCloudPlot <- function(word_freqs,
                          seed = NULL,
                          min_freq = 1,
                          max_words = 50,
                          pcolors = NULL,
                          family = NULL,
                          ...) {
  
  # returns empty plot with message if no data to plot
  if (is.null(word_freqs) || nrow(word_freqs) < 1) {
    return(emptyPlotMessage("No text data."))
  }
  
  if (!is.null(seed)) { set.seed(seed) }
  
  saved_par <- par(no.readonly = TRUE)
  on.exit(par(saved_par))
  
  freq <- NULL
  word_freqs <- word_freqs[order(word_freqs, freq)]
  
  plot_parameters <- list(words = word_freqs$word,
                          freq = word_freqs$freq,
                          min.freq = min_freq,
                          max.words = max_words)
  
  plot_parameters[['colors']] <- pcolors
  
  if (!is.null(family)) { plot_parameters['family'] <- family }
  
  dots <- list(...)
  plot_parameters <- append(plot_parameters, dots)
  
  par(mar = rep(0, 4))
  do.call(wordcloud::wordcloud, plot_parameters)
}

#' @title Create NRC emotion data
#'
#' @description This function creates an NRC emotion dataframe from a text corpus.
#'
#' @param corp \pkg{tm} package document \code{\link[tm]{Corpus}} object.
#' @param word_len Numeric vector. Min and max length of words to include. Default is \code{c(3, 26)}.
#' 
#' @note Uses the \pkg{syuzhet} package implementation of Saif Mohammad’s NRC emotion lexicon.
#' 
#' @return An NRC sentiment dataframe.
#' 
#' @export
wordSentData <- function(corp, word_len = c(3, 26)) {
  # best effort without tokenizing
  if (word_len[1] < 1) { word_len[1] <- 1 }
  if (word_len[1] <= word_len[2]) {
    corp <- tm::tm_map(corp, tm::content_transformer(function(x) gsub(paste0("\\b[^\\s]{1,", word_len[1], "}\\b"), " ", x)))
    corp <- tm::tm_map(corp, tm::content_transformer(function(x) gsub(paste0("\\b[^\\s]{", word_len[2], ", }\\b"), " ", x)))
  }
  
  ws_df <- data.table::data.table(content = unlist(sapply(corp, `[`, "content")), stringsAsFactors = FALSE)

  if (is.null(ws_df) || ncol(ws_df) < 1 || nrow(ws_df) < 1) { return(NULL) }
  ws_df <- unlist(ws_df[, 1])
  nrc_sent_df <- syuzhet::get_nrc_sentiment(ws_df)
}

#' @title Create an NRC emotion chart
#'
#' @description This function creates a horizontal barchart measuring and sorting the eight NRC lexicon emotions.
#' Emotions are measured as the proportion of the total value of the eight emotions in the text as a
#' percentage.
#'
#' @note Uses the \pkg{syuzhet} package implementation of Saif Mohammad’s NRC Emotion lexicon.
#'
#' @param data Dataframe. NRC emotions table.
#' @param pcolors List. Colors to assign categorical variable in the plot. Default is \code{NULL}.
#'
#' @return A barchart plot.
#'
#' @export
wordSentChart <- function(data, pcolors = NULL) {

  # returns empty plot with message if no data to chart
  if (is.null(data) || nrow(data) < 1) {
    return(emptyPlotMessage("No text data."))
  }

  chart_data <- sort(colSums(prop.table(data[, 1:8]))*100)

  colx <- pcolors
  if (is.null(colx)) {
    colx <- "#f5f5f5"
  }
  colx[seq(1, 8)] <- colx

  saved_par <- par(no.readonly = TRUE)
  on.exit(par(saved_par))

  par(las = 2)
  par(mar = c(4, 6, 3, 1))

  if (length(chart_data) < 1) {
    return(emptyPlotMessage("No analyzable text data."))
  }
  
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

#' @title Create an NRC sentiment valence chart
#'
#' @description This function creates a vertical barchart of the sum of negative and positive sentiments, and the
#' valence or net sentiment in a text corpus.
#'
#' @param data Dataframe. NRC emotions table.
#'
#' @return A barchart plot.
#'
#' @export
wordSentValenceChart <- function(data) {
  # returns empty plot with message if no data to chart
  if (is.null(data) || nrow(data) < 1) {
    return(emptyPlotMessage("No text data."))
  }
  
  chart_data <- colSums(data[c(9:10)])
  valence <- ((data[, 9]*-1) + data[, 10])
  chart_data["valence"] <- sum(valence)
  chart_data["negative"] <- chart_data["negative"]*-1
  
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

#' @title Create a word frequency dataframe
#'
#' @description Create a word frequency dataframe from a text corpus.
#' 
#' @param corp a \pkg{tm} text corpus object.
#' @param rm_sparse Logical. Remove proportion of sparse terms. Default is \code{0.99}.
#' @param word_len Numeric vector. Min and max length of words to include. Default is \code{c(3, 26)}.
#' @param word_freq Numeric vector. Min and max frequency of words to include. Default is \code{c(1, Inf)}.
#' 
#' @return A data.table of word frequencies.
#' 
#' @export
wordFreqFromCorpus <- function(corp,
                               rm_sparse = 0.99,
                               word_len = c(3, 26),
                               word_freq = c(1, Inf)) {
  
  dtm <- tm::DocumentTermMatrix(corp, control = list(wordLengths = word_len,
                                                     bounds = list(global = word_freq)))
  dtm_sparse_removed <- tm::removeSparseTerms(dtm, rm_sparse)
  
  freq_terms <- colSums(as.matrix(dtm_sparse_removed))
  freq_terms <- freq_terms[order(freq_terms, decreasing = TRUE)]
  
  data.table(word = names(freq_terms), freq = freq_terms)
}

#' @title Create a text corpus from graph text attribute data
#'
#' @description This function creates a text corpus from node or edge text attribute data in an igraph. 
#' 
#' @param g an \pkg{igraph} graph object.
#' @param txt_attr Character string. Name of graph text attribute. Default is \code{NULL}.
#' @param type Character string. Graph attribute type. Default is \code{"vertex"}.
#' @param iconv Logical. Use the \code{iconv} function to attempt UTF8 conversion. Default is \code{FALSE}.
#' @param html_decode Logical. HTML decode text. Default is \code{TRUE}.
#' @param rm_url Logical. Remove URL's. Default is \code{TRUE}.
#' @param rm_num Logical. Remove numbers. Default is \code{TRUE}.
#' @param rm_punct Logical. Remove punctuation. Default is \code{TRUE}.
#' @param rm_twit_hashtags Logical. Remove twitter hashtags. Default is \code{FALSE}.
#' @param rm_twit_users Logical. Remove twitter user names. Default is \code{FALSE}.
#' @param sw_kind Character string. Stopword dictionary. Refer \code{stopwords} \code{kind} parameter.
#' Default is \code{"SMART"}.
#' @param rm_words Character vector. User defined stopwords. Default is \code{NULL}.
#' @param stem Logical. Apply word stemming. Default is \code{FALSE}.
#' 
#' @return A \pkg{tm} text corpus object.
#' 
#' @export
corpusFromGraph <- function(g = NULL,
                        txt_attr = NULL,
                        type = "vertex",
                        iconv = FALSE,
                        html_decode = TRUE,
                        rm_url = TRUE,
                        rm_num = TRUE,
                        rm_punct = TRUE,
                        rm_twit_hashtags = FALSE,
                        rm_twit_users = FALSE,
                        sw_kind = "SMART",
                        rm_words = NULL,
                        stem = FALSE) {
  
  if (is.null(g) | is.null(txt_attr) || !igraph::is.igraph(g)) {
    return(NULL)
  }
  
  if (tolower(type) == "vertex") {
    txt_data <- igraph::vertex_attr(g, txt_attr)
  } else {
    txt_data <- igraph::edge_attr(g, txt_attr)
  }

  if (iconv) {
    if (isMac()) {
      txt_data <- iconv(txt_data, to = 'utf-8-mac')
    } else {
      txt_data <- iconv(txt_data, to = 'utf-8')
    }
  }
  
  rm <- which(txt_data == "")
  if (length(rm) > 0) { txt_data <- txt_data[-rm] }

  corp <- tm::VCorpus(tm::VectorSource(txt_data))
  if (html_decode) { corp <- tm::tm_map(corp, tm::content_transformer(textutils::HTMLdecode)) }
  
  corp <- tm::tm_map(corp, tm::content_transformer(tolower))
  if (rm_url) { corp <- tm::tm_map(corp, tm::content_transformer(function(x) gsub("http[s]?://[^[:space:]]+", "", x))) }
  
  if (rm_twit_hashtags) { corp <- tm::tm_map(corp, tm::content_transformer(function(x) gsub("#\\S+", "", x))) }
  if (rm_twit_users) { corp <- tm::tm_map(corp, tm::content_transformer(function(x) gsub("@\\S+", "", x))) }
  
  if (rm_num) { corp <- tm::tm_map(corp, tm::removeNumbers) }
  if (rm_punct) { corp <- tm::tm_map(corp, tm::removePunctuation) }
  
  sw <- c()
  if (!is.null(sw_kind) & is.character(sw_kind)) { sw <- tm::stopwords(sw_kind) }
  
  if (!is.null(rm_words)) {
    rm_words <- trimws(unlist(strsplit(tolower(rm_words), ",")))
    sw <- c(sw, rm_words)
  }
  
  if (length(sw) > 0) { corp <- tm::tm_map(corp, tm::removeWords, sw, lazy = TRUE) }
  
  if (stem) { corp <- tm::tm_map(corp, tm::stemDocument) }
  
  corp <- tm::tm_map(corp, tm::stripWhitespace, lazy = TRUE)
}
