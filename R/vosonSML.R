#' Create a auth token with twitter app dev keys
#' 
#' @param app_name twitter app name
#' @param keys named list of app API keys
#' @return cred
#' @keywords internal
#' 
#' @export
createTwitterDevToken <- function(app_name, keys) {
  check_keys <- sapply(keys, isNullOrEmpty)
  
  if (any(check_keys == TRUE)) { 
    return(NULL) 
  }
  
  cred <- vosonSML::Authenticate("twitter", 
                                 appName = app_name,
                                 apiKey = keys$apiKey, 
                                 apiSecret = keys$apiSecret,
                                 accessToken = keys$accessToken,
                                 accessTokenSecret = keys$accessTokenSecret, 
                                 useCachedToken = FALSE)
  
  cred$type <- "dev"
  cred$created <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  
  cred
}

#' Create a auth token with twitter app consumer keys and interactive web authorization
#' 
#' @param app_name twitter app name
#' @param keys named list of app API keys
#' @return cred
#' @keywords internal
#' 
#' @export
createTwitterWebToken <- function(app_name, keys) {
  check_keys <- sapply(keys, isNullOrEmpty)
  
  if (any(check_keys == TRUE)) { 
    return(NULL) 
  }
  
  cred <- list(socialmedia = "twitter", auth = NULL)
  class(cred) <- append(class(cred), c("credential", "twitter")) 
  
  cred$auth <- rtweet::create_token(
    app = app_name,
    consumer_key = keys$apiKey,
    consumer_secret = keys$apiSecret,
    set_renv = FALSE)
  
  cred$type <- "web"
  cred$created <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  
  cred
}

#' Create voson dash twitter auth token id
#' 
#' @param token twitter auth token
#' @return token_id as character string 
#' @keywords internal
#'
#' @export
createTokenId <- function(token) {
  token_id <- paste0(token$created, " ", token$auth$app$appname, " (", token$type ,")")
}

#' Wrapper for collecting tweets using the vosonSML package
#' 
#' @param token twitter auth token
#' @param search_term twitter search term as character string
#' @param search_type search type as character string - mixed, recent, popular
#' @param tweet_count number of tweets to collect
#' @param include_retweets logical whether to include retweets in the results
#' @param retry_on_rate_limit logical whether to wait and retry when the twitter api rate limit is hit
#' @param language language code of tweets to collect as two character ISO 639-1 code
#' @param date_until date to collect tweets to as character string YYYY-MM-DD
#' @param since_id numeric id get results with an id more recent than this id
#' @param max_id numeric id get results with an id older than this id
#' @return tweets as vosonSML Collect dataframe
#' @keywords internal
#' 
#' @export
collectTwitterData <- function(token, search_term, search_type, tweet_count, 
                               include_retweets, retry_on_rate_limit,
                               language, date_until, since_id, max_id) {
  
  if (is.null(token) || isNullOrEmpty(search_term)) { return(NULL) }
  
  collect_params <- list()
  
  collect_params[['credential']] <- token
  collect_params['searchTerm'] <- search_term
  
  if (!isNullOrEmpty(search_type)) {
    collect_params['searchType'] <- search_type
  }
  
  if (is.numeric(tweet_count) && tweet_count > 0) {
    collect_params['numTweets'] <- tweet_count
  }
  
  collect_params['includeRetweets'] <- TRUE
  if (!include_retweets) {
    collect_params['includeRetweets'] <- FALSE
  }
  
  collect_params['retryOnRateLimit'] <- TRUE
  if (!retry_on_rate_limit) {
    collect_params['retryOnRateLimit'] <- FALSE
  }
  
  if (!isNullOrEmpty(language)) {
    collect_params['lang'] <- language
  }
  
  if (!isNullOrEmpty(date_until)) {
    collect_params['until'] <- date_until
  }
  
  if (!isNullOrEmpty(since_id)) {
    collect_params['since_id'] <- since_id
  }
  
  if (!isNullOrEmpty(max_id)) {
    collect_params['max_id'] <- max_id
  }
  
  collect_params['writeToFile'] <- FALSE
  collect_params['verbose'] <- TRUE
  
  data <- do.call(vosonSML::Collect, collect_params)
}

#' Create twitter actor networks
#' 
#' @param data vosonSML twitter Collect dataframe
#'
#' @return named list of twitter actor networks
#' @keywords internal
#' 
#' @export
createTwitterActorNetwork <- function(data) {
  network <- data %>% vosonSML::Create("actor", verbose = TRUE)
  
  g <- igraph::set_graph_attr(network$graph, "type", "twitter")
  
  g_wt <- g
  E(g_wt)$vosonTxt_tweet <- data$text[match(E(g_wt)$status_id, data$status_id)]
  
  list(network = g, networkWT = g_wt)
}

#' Wrapper for collecting youtube video comments using the vosonSML package
#' 
#' @param youtube_api_key youtube api key as character string
#' @param youtube_video_id_list vector of youtube video ids to collect comments from
#' @param youtube_max_comments maximum number of comments to collect
#' 
#' @return data as vosonSML youtube collection dataframe
#' @keywords internal
#' 
#' @export
collectYoutubeData <- function(youtube_api_key, youtube_video_id_list, youtube_max_comments) {
  
  if (is.null(youtube_api_key) || (length(youtube_video_id_list) < 1)) {
    return(NULL)
  }
  
  collect_params <- list()

  cred <- vosonSML::Authenticate("youtube", apiKey = youtube_api_key)
  
  collect_params[['credential']] <- cred
  collect_params[['videoIDs']] <- youtube_video_id_list
  
  if (is.numeric(youtube_max_comments) && youtube_max_comments > 1) {
    collect_params['maxComments'] <- youtube_max_comments
  }
  
  collect_params['writeToFile'] <- FALSE
  collect_params['verbose'] <- FALSE
  
  data <- do.call(vosonSML::Collect, collect_params)
}

#' Create youtube actor networks
#' 
#' @param data as vosonSML youtube collection dataframe
#'
#' @return named list of youtube actor networks
#' @keywords internal
#' 
#' @export
createYoutubeNetwork <- function(data) {
  network <- data %>% Create('actor', writeToFile = FALSE)
  
  g <- igraph::set_graph_attr(network$graph, "type", "youtube")
  
  g_wt <- g
  E(g_wt)$vosonTxt_comment <- data$Comment[match(E(g_wt)$commentId, data$CommentId)]
  
  list(network = g, networkWT = g_wt)
}

#' Wrapper for collecting reddit thread comments using the vosonSML package
#' 
#' @param reddit_url_list vector of thread urls to collect comments from
#'
#' @return data as vosonSML reddit collection dataframe
#' @keywords internal
#' 
#' @export
collectRedditData <- function(reddit_url_list) {
  data <- NULL
  
  if (length(reddit_url_list) > 0) {
    data <- vosonSML::Authenticate("reddit") %>% 
      vosonSML::Collect(threadUrls = reddit_url_list, waitTime = 5, writeToFile = FALSE)
  }
  
  data
}

#' Create reddit actor networks
#' 
#' @param data as vosonSML reddit collection dataframe
#'
#' @return list of reddit actor networks as graphml objects
#' @keywords internal
#' 
#' @export
createRedditActorNetwork <- function(data) {
  network <- data %>% vosonSML::Create("actor", writeToFile = FALSE)
  networkWT <- data %>% vosonSML::Create("actor", textData = TRUE, cleanText = TRUE, writeToFile = FALSE)
  
  list(network = network$graph, networkWT = networkWT$graph)
}

#' Loaded vosonSML package version
#' 
#' @return package version as character string
#' @keywords internal
#' 
#' @export
getVosonSMLVersion <- function(compare_ver) {
  if ("vosonSML" %in% loadedNamespaces()) {
    if (missing(compare_ver)) {
      return(utils::packageVersion("vosonSML"))
    } else {
      return(utils::packageVersion("vosonSML") >= compare_ver)
    }
  }
  
  NULL
}

