#' @title Get the vosonSML package version
#' 
#' @description This function returns the version of the loaded vosonSML package.
#' 
#' @return Package version as character string.
#' 
#' @keywords internal
#' @export
getVosonSMLVersion <- function() {
  if ("vosonSML" %in% loadedNamespaces()) { return(utils::packageVersion("vosonSML")) }
  "unknown"
}

#' @title Return logical if vosonSML version later than 0.29
#' 
#' @description This function returns if the installed version of vosonSML is later than v0.29
#'
#' @return Logical.
#' 
#' @keywords internal
#' @export
isVosonSML0290 <- function() {
  if (utils::packageVersion("vosonSML") >= "0.29.0") { return(TRUE) }
  FALSE
}

#' @title Create an auth token with twitter app dev keys
#' 
#' @description This function is a wrapper for \code{vosonSML::Authenticate} with twitter app developer keys. The 
#' properties \code{type} and \code{created} are added to the credential object to assist with \pkg{VOSONDash} token 
#' management. 
#' 
#' @param app_name Character string. Twitter app name.
#' @param keys List. Named list of twitter app API keys.
#'
#' @return A vosonSML twitter credential object.
#' 
#' @keywords internal
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

#' @title Create a auth token with twitter app consumer keys 
#' 
#' @description This function creates a \code{vosonSML::Authenticate} credential object with twitter app consumer keys 
#' and interactive web authorization. \code{rtweet::create_token} is used to create the access token and the properties 
#' \code{type} and \code{created} are added to the credential object to assist with \pkg{VOSONDash} token management. 
#' 
#' @param app_name Character string. Twitter app name.
#' @param keys List. Named list of twitter app API keys.
#'
#' @return A \pkg{vosonSML} twitter credential object.
#' 
#' @keywords internal
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

#' @title Create a twitter auth token id
#' 
#' @description This function uses properties of the twitter credential object to create a unique token id.
#' 
#' @param cred \pkg{vosonSML} twitter credential object.
#' 
#' @return A token id as character string. 
#' 
#' @keywords internal
#' @export
createTokenId <- function(cred) {
  token_id <- paste0(cred$created, " ", cred$auth$app$appname, " (", cred$type ,")")
}

#' @title Collect twitter data
#' 
#' @description This function is a wrapper for collecting tweets using \code{vosonSML::Collect}.
#' 
#' @param cred \pkg{vosonSML} twitter credential object.
#' @param search_term Character string. Twitter search term.
#' @param search_type Character string. Search type \code{"mixed"}, \code{"recent"} or \code{"popular"}.
#' @param tweet_count Numeric. Number of tweets to collect.
#' @param include_retweets Logical. Include retweets in the results.
#' @param retry_on_rate_limit Logical. Wait and retry when the twitter api rate limit is reached.
#' @param language Character string. Language code of tweets to collect as two character ISO 639-1 code.
#' @param date_until Character string. Date to collect tweets to in format \code{"YYYY-MM-DD"}.
#' @param since_id Numeric. Collect tweets with a tweet id more recent than \code{since_id}.
#' @param max_id Numeric. Collect tweets with a tweet id older than \code{max_id}.
#' 
#' @return A vosonSML twitter dataframe.
#' 
#' @keywords internal
#' @export
collectTwitterData <- function(cred, search_term, search_type, tweet_count, 
                               include_retweets, retry_on_rate_limit,
                               language, date_until, since_id, max_id) {
  
  if (is.null(cred) || isNullOrEmpty(search_term)) { return(NULL) }
  
  collect_params <- list()
  
  collect_params[['credential']] <- cred
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

#' @title Create twitter actor networks
#' 
#' @description This function is a wrapper for creating a twitter actor networks using \code{vosonSML::Create}.
#' 
#' @param data \pkg{vosonSML} twitter dataframe.
#'
#' @return Twitter actor networks as named list.
#' 
#' @keywords internal
#' @export
createTwitterActorNetwork <- function(data) {
  network <- vosonSML::Create(data, "actor", verbose = TRUE)
  
  g <- igraph::set_graph_attr(network$graph, "type", "twitter")
  
  g_wt <- g
  E(g_wt)$vosonTxt_tweet <- data$text[match(E(g_wt)$status_id, data$status_id)]
  
  list(network = g, networkWT = g_wt)
}

#' @title Collect youtube data
#' 
#' @description This function is a wrapper for collecting youtube video comments using \code{vosonSML::Collect}. 
#' 
#' @param youtube_api_key Character string. Youtube api key.
#' @param youtube_video_id_list Character vector. Youtube video ids to collect comments from.
#' @param youtube_max_comments Numeric. Maximum number of comments to collect.
#' 
#' @return A vosonSML youtube dataframe.
#' 
#' @keywords internal
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

#' @title Create youtube actor networks
#' 
#' @description This function is a wrapper for creating a youtube actor networks using \code{vosonSML::Create}.
#' 
#' @param data \pkg{vosonSML} youtube dataframe.
#'
#' @return Youtube actor networks as named list.
#' 
#' @keywords internal
#' @export
createYoutubeNetwork <- function(data) {
  network <- Create(data, 'actor', writeToFile = FALSE)
  
  g <- igraph::set_graph_attr(network$graph, "type", "youtube")
  
  g_wt <- g
  E(g_wt)$vosonTxt_comment <- data$Comment[match(E(g_wt)$commentId, data$CommentId)]
  
  list(network = g, networkWT = g_wt)
}

#' @title Collect reddit data
#' 
#' @description This function is a wrapper for collecting reddit thread comments using \code{vosonSML::Collect}. 
#' 
#' @param reddit_url_list Character vector. Thread urls to collect comments from.
#'
#' @return A vosonSML reddit dataframe.
#' 
#' @keywords internal
#' @export
collectRedditData <- function(reddit_url_list) {
  data <- NULL
  
  if (length(reddit_url_list) > 0) {
    data <- vosonSML::Collect(vosonSML::Authenticate("reddit"), threadUrls = reddit_url_list, waitTime = 5, 
                              writeToFile = FALSE)
  }
  
  data
}

#' @title Create reddit actor networks
#' 
#' @description This function is a wrapper for creating a reddit actor networks using \code{vosonSML::Create}.
#' 
#' @param data \pkg{vosonSML} reddit dataframe.
#'
#' @return Reddit actor networks as named list.
#' 
#' @keywords internal
#' @export
createRedditActorNetwork <- function(data) {
  network <- vosonSML::Create(data, "actor", writeToFile = FALSE)
  networkWT <- vosonSML::Create(data, "actor", textData = TRUE, cleanText = TRUE, writeToFile = FALSE)
  
  list(network = network$graph, networkWT = networkWT$graph)
}
