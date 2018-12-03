#' VOSON Dashboard vosonSML Functions
#'
#' Helper functions to interact with the vosonSML package. 
#'

#' Collect tweets using vosonSML.
#' 
#' @param twitter_api_keyring named list of twitter api keys as character strings
#' @param search_term twitter search term as character string
#' @param search_type search type as character string - mixed, recent, popular
#' @param tweet_count number of tweets to collect
#' @param language language code of tweets to collect as two character ISO 639-1 code
#' @param date_since date to collect tweets from as character string YYYY-MM-DD
#' @param date_until date to collect tweets to as character string YYYY-MM-DD
#'
#' @return data as vosonSML twitter collection dataframe
#'
collectTwitterData <- function(twitter_api_keyring, search_term, search_type, tweet_count, language, date_since, date_until) {
  
  check_keys <- sapply(twitter_api_keyring, isNullOrEmpty)
  
  if (any(check_keys == TRUE) || isNullOrEmpty(search_term)) { return(NULL) }
  
  # vosonSML twitter collection parameters:
  # searchTerm, numTweets, verbose, writeToFile, language, since, until
  # locale, geocode, sinceID, maxID, resultType, retryOnRateLimit
  
  collect_parameters <- list()
  collect_parameters['searchTerm'] <- search_term
  
  if (!isNullOrEmpty(search_type)) {
    collect_parameters['resultType'] <- search_type
  }
  
  if (is.numeric(tweet_count) && tweet_count > 0) {
    collect_parameters['numTweets'] <- tweet_count
  }
  
  if (!isNullOrEmpty(language)) {
    collect_parameters['language'] <- language
  }
  
  # since and until
  if (!isNullOrEmpty(date_since)) {
    collect_parameters['since'] <- date_since
  }
  
  if (!isNullOrEmpty(date_until)) {
    collect_parameters['until'] <- date_until
  }
  
  data <- NULL
  create_token <- FALSE
  cred <- Authenticate("twitter", apiKey = twitter_api_keyring$twitter_api_key, 
                       apiSecret = twitter_api_keyring$twitter_api_secret,
                       accessToken = twitter_api_keyring$twitter_access_token, 
                       accessTokenSecret = twitter_api_keyring$twitter_access_token_secret, 
                       createToken = create_token)
  
  collect_parameters[['credential']] <- cred
  collect_parameters['writeToFile'] <- FALSE
  collect_parameters['verbose'] <- TRUE
  
  data <- do.call(Collect, collect_parameters)
  
  return(data)
}

#' Create twitter actor network using vosonSML.
#' 
#' @param data as vosonSML twitter collection dataframe
#'
#' @return list of twitter actor networks as graphml objects
#'
createTwitterActorNetwork <- function(data) {
  network <- NULL
  
  network <- data %>% Create("Actor", writeToFile = FALSE)
  
  network <- set.graph.attribute(network,"type", "twitter")
  
  networkWT <- network # with text data
  
  # with twitter data, text is edge attribute (tweet payload leading to the edge)
  E(networkWT)$vosonTxt_payload <- data$text[match(E(networkWT)$tweet_id, data$id)]
  
  # return(network)
  return(list(network = network, networkWT = networkWT))
  
}

#' Collect youtube comments using vosonSML.
#' 
#' @param youtube_api_key youtube api key as character string
#' @param youtube_video_id_list vector of youtube video ids to collect comments from
#'
#' @return data as vosonSML youtube collection dataframe
#'
collectYoutubeData <- function(youtube_api_key, youtube_video_id_list) {
  data <- NULL
  
  if ((!is.null(youtube_api_key) && nchar(youtube_api_key) > 1) && (length(youtube_video_id_list) > 0)){
    data <- Authenticate("youtube", apiKey = youtube_api_key) %>% 
      Collect(videoIDs = youtube_video_id_list, writeToFile = FALSE, verbose = TRUE)
  }
  
  return(data)
}

#' Create youtube actor network using vosonSML.
#' 
#' @param data as vosonSML youtube collection dataframe
#'
#' @return youtube actor networks as graphml object
#'
createYoutubeNetwork <- function(data) {
  network <- NULL
  
  network <- data %>% Create('actor', writeToFile = FALSE)
  
  network <- set.graph.attribute(network, "type", "youtube")
  
  # Rob started work on getting text data into network, but needs to make change to vosonSML (comment id as edge attribute)
  networkWT <- network # with text data
  
  # with YouTube data, text is edge attribute (comment leading to the edge)
  E(networkWT)$vosonTxt_comment <- data$Comment[match(E(networkWT)$commentId, data$CommentId)]
  
  # return(network)
  return(list(network = network, networkWT = networkWT))
}

#' Collect reddit thread comments using vosonSML.
#' 
#' @param reddit_url_list vector of thread urls to collect comments from
#'
#' @return data as vosonSML reddit collection dataframe
#'
collectRedditData <- function(reddit_url_list) {
  data <- NULL
  
  if (length(reddit_url_list) > 0) {
    data <- Authenticate("reddit") %>% 
      Collect(threadUrls = reddit_url_list, waitTime = 5, writeToFile = FALSE)
  }
  
  return(data)
}

#' Create reddit actor network using vosonSML.
#' 
#' @param data as vosonSML reddit collection dataframe
#'
#' @return list of reddit actor networks as graphml objects
#'
createRedditActorNetwork <- function(data) {
  network <- networkWT <- NULL
  
  network <- data %>% Create("actor", writeToFile = FALSE)
  networkWT <- data %>% Create("actor", includeTextData = TRUE, writeToFile = FALSE)
  
  return(list(network = network, networkWT = networkWT))
}

#' Check if graph object has vertex or edge voson text attributes.
#' 
#' @param g as graphml graph object
#' 
#' @return has_text as boolean
#' 
hasVosonTextData <- function(g) {
  attr_v <- vertex_attr_names(g)
  attr_v <- attr_v[grep("^vosonTxt", attr_v, perl = T)]
  attr_e <- edge_attr_names(g)
  attr_e <- attr_e[grep("^vosonTxt", attr_e, perl = T)]
  
  has_text <- FALSE
  if (length(attr_v)) {
    attr <- c(attr_v[1], 'vertex')
    has_text <- TRUE
  } else if (length(attr_e)) {
    i <- attr_e[1]
    attr <- c(attr_e[1], 'edge')
    has_text <- TRUE
  }
  
  return(has_text)
}

#' Installed vosonSML package version.
#' 
#' @return info about package as character string
#' 
getInfo <- function() {
  info <- version[['version.string']]
  
  if ("vosonSML" %in% loadedNamespaces()) {
    info <- paste0(info, "\nvosonSML package v.", packageVersion("vosonSML"), sep = "")
  } else {
    info <- paste0(info, "\nvosonSML package not found.", sep = "")
  }
  
  return(info)
}
