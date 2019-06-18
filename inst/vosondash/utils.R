#' VOSON Dashboard Utils
#'
#' Helper functions. 
#'

#' Check a shiny app input value for a range of empty conditions.
#' 
#' @param x shiny input value
#'
#' @return result as boolean
#'
isNullOrEmpty <- function(x) {
  if (is.null(x) || identical(x, character(0)) || is.na(x) || trimws(x) == "") {
    return(TRUE)
  }
  
  return(FALSE)
}

#' Create a friendly file name with system date time prefix.
#' 
#' @param name_suffix name part of file name to append to date time as character string
#' @param name_ext file name extension as character string
#' @param clean remove problematic characters from file name as boolean
#'
#' @return file_name as character string
#'
systemTimeFilename <- function(name_suffix, name_ext, clean = FALSE) {
  current_time <- Sys.time()
  
  if (!missing(clean) && clean == TRUE) {
    name_suffix <- gsub("\\s+", "_", name_suffix, perl = TRUE)
    name_suffix <- gsub(":", "_", name_suffix, perl = TRUE)
    
    name_ext <- gsub("\\s+", "", name_ext, perl = TRUE)
    name_ext <- gsub(":", "", name_ext, perl = TRUE)  
    name_ext <- gsub("\\.", "", name_ext, perl = TRUE)    
  }
  
  file_name <- paste0(format(current_time, "%Y-%m-%d_%H-%M-%S"), 
                      "_", name_suffix, ".", name_ext, sep = "")
  return(file_name)
}

createRedditRequestUrl <- function(url) {
  url <- tolower(url)
  
  # base_url       <- "https://reddit.com/r/"
  # base_url_oauth <- "https://oauth.reddit.com/r/"
  
  # https://www.reddit.com/r/MakeupAddiction/comments/9yrj6k/pink_everything/
  if(!grepl("^https://(www\\.)?reddit.com/r/(.*?)/comments/([0-9A-Za-z]{6})?/.*$", 
            url, ignore.case = TRUE, perl = TRUE)) {
    return(NULL)
  } else {
    # "https://oauth.reddit.com/r/"
    url <- gsub("^https://(www\\.)?reddit.com/r/", "r/", 
                url, ignore.case = TRUE, perl = TRUE)
    
    url <- gsub("^(.*)?/comments/([0-9A-Za-z]{6})?/.*?(/)?$", "\\1/comments/\\2/", 
                url, ignore.case = TRUE, perl = TRUE)
  }
}

getRedditUrlThreadId <- function(url) {
  thread_id <- gsub("^(.*)?/comments/([0-9A-Za-z]{6})?/.*?(/)?$", "\\2", 
                    url, ignore.case = TRUE, perl = TRUE)
}

getRedditUrlSubreddit <- function(url) {
  subreddit <- gsub("^(.*)?/r/(.*)?/comments/.*?(/)?$", "\\2", 
                    url, ignore.case = TRUE, perl = TRUE)  
}

getYoutubeVideoId <- function(url) {
  # already an id
  if (grepl("^[0-9A-Za-z_\\-]{11}$", url, ignore.case = TRUE, perl = TRUE)) {
    return(url)
  }  
  
  url <- parse_url(url)
  video_id <- NULL
  
  if (is.null(url$hostname)) {
    return(NULL)
  }
  
  # https://youtu.be/xxxxxxxxxxx
  if (tolower(trimws(url$hostname)) == "youtu.be") {
    if (length(url$path) > 0) {
      video_id <- url$path[1]
    }
  }
  
  # https://www.youtube.com/watch?v=xxxxxxxxxxx
  if (tolower(trimws(url$hostname)) == "www.youtube.com") {
    if (!is.null(url$query$v)) {
      video_id <- url$query$v
    }
  }
  
  # check extracted id
  if (!grepl("^[0-9A-Za-z_\\-]{11}$", video_id, ignore.case = TRUE, perl = TRUE)) {
    return(NULL)
  }
  
  return(video_id)
}

# check if R is running on macos
isMac <- function() {
  macMatch <- grep("darwin", R.Version()$os)
  
  if (length(macMatch) != 0) {
    return(TRUE)
  }
  
  return(FALSE)
}

# simple log
logMessage <- function(messages, add_message, count = 25) {
  
  add_message <- c(paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), add_message))
  log_messages <- c(add_message, messages)

  if (length(log_messages) > count) {
    return(log_messages[1:count])
  }
  
  return(log_messages)
}

createTokenId <- function(token) {
  token_id <- paste0(token$created, " ", token$auth$app$appname, " (", token$type ,")")
}

# helper functions
removeURL <- function(x) gsub("http[[:alnum:][:punct:]]*", "", x)   # removes http and https
# removeURL <- function(x) gsub("http[^[:space:]]*", "", x)         # might need if non-ascii characters in url
removeHashTags <- function(x) gsub("#\\S+", "", x)
removeTwitterHandles <- function(x) gsub("@\\S+", "", x)

# various other to clean up twitter text
removeOther1 <- function(x) gsub("&apos;", "\'", x)
removeOther2 <- function(x) gsub("&quot;", "\"", x)
removeOther3 <- function(x) gsub("&amp;", "&", x)
removeOther4 <- function(x) gsub("amp;|gt;", "", x)