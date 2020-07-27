#' @title Get the VOSONDash package version
#' 
#' @description This function returns the version of the loaded VOSONDash package. 
#' 
#' @return Package version character string.
#' 
#' @keywords internal
#' @export
getVOSONDashVer <- function() {
  if ("VOSONDash" %in% loadedNamespaces()) {
    return(utils::packageVersion("VOSONDash"))
  }
  "unknown"
}

#' @title Check a value for a range of empty conditions
#' 
#' @description This function checks a value is not NULL, NA or an empty string. 
#' 
#' @param x Input value.
#' 
#' @return Result as logical.
#' 
#' @keywords internal
#' @export
isNullOrEmpty <- function(x) {
  if (is.null(x) || identical(x, character(0)) || is.na(x) || trimws(x) == "") {
    return(TRUE)
  }
  FALSE
}

#' @title Create a file name with system date time prefix
#' 
#' @description This function uses the system date and time to create a unique file name.
#' 
#' @param name_suffix Character string. Name part of file name to append to date time part.
#' @param name_ext Character string. File extension without the period. For example, \code{"txt"}.
#' @param clean Logical. Remove problematic file system characters from file name part. Default is \code{FALSE}.
#' 
#' @return A unique date time file name as character string.
#' 
#' @keywords internal
#' @export
systemTimeFilename <- function(name_suffix, name_ext, clean = FALSE) {
  current_time <- Sys.time()
  
  if (clean) {
    name_suffix <- gsub("\\s+", "_", name_suffix, perl = TRUE)
    name_suffix <- gsub(":", "_", name_suffix, perl = TRUE)
    
    name_ext <- gsub("\\s+", "", name_ext, perl = TRUE)
    name_ext <- gsub(":", "", name_ext, perl = TRUE)  
    name_ext <- gsub("\\.", "", name_ext, perl = TRUE)    
  }
  
  file_name <- paste0(format(current_time, "%Y-%m-%d_%H-%M-%S"), 
                      "_", name_suffix, ".", name_ext, sep = "")
}

#' @title Create a reddit url to request thread comments
#' 
#' @description This function creates a url from specified thread url that can be used to request the thread comments.
#' 
#' @param url Character string. Reddit thread url.
#' 
#' @return Reddit API url as character string.
#' 
#' @keywords internal
#' @export
createRedditRequestUrl <- function(url) {
  # url <- tolower(url)
  
  if(!grepl("^https://(www\\.)?reddit.com/r/(.*?)/comments/([0-9A-Za-z]{6})?/.*?/$", 
            url, ignore.case = TRUE, perl = TRUE)) {
    return(NULL)
  }
  
  url <- gsub("^https://(www\\.)?reddit.com/r/", "r/", url, ignore.case = TRUE, perl = TRUE)
  
  # url <- gsub("^(.*)?/comments/([0-9A-Za-z]{6})?/.*?(/)?$", "\\1/comments/\\2/", 
  #             url, ignore.case = TRUE, perl = TRUE)
  
  # url <- gsub("^(.*)?/comments/([0-9A-Za-z]{6})?/(.*)?/$", "\\1/comments/\\2/\\3/", 
  #            url, ignore.case = TRUE, perl = TRUE)
}

#' @title Get a reddit thread id from url
#' 
#' @description This function extracts the thread id from a reddit thread url.
#' 
#' @param url Character string. Reddit thread url.
#' 
#' @return Reddit thread id as character string.
#'
#' @export
getRedditUrlThreadId <- function(url) {
  thread_id <- gsub("^(.*)?/comments/([0-9A-Za-z]{6})?/.*?(/)?$", "\\2", 
                    url, ignore.case = TRUE, perl = TRUE)
}

#' @title Get subreddit name from url
#' 
#' @description This function extracts the subreddit name from a reddit thread url. 
#' 
#' @param url Character string. Reddit thread url.
#' 
#' @return Subreddit name as character string.
#'
#' @export
getRedditUrlSubreddit <- function(url) {
  subreddit <- gsub("^(.*)?/r/(.*)?/comments/.*?(/)?$", "\\2", 
                    url, ignore.case = TRUE, perl = TRUE)  
}

#' @title Get a youtube video id from url
#' 
#' @description This function extracts the youtube video id from a youtube video url.
#' 
#' @param url Character string. Youtube video url.
#' 
#' @return Video id as character string.
#'
#' @export
getYoutubeVideoId <- function(url) {
  # already an id
  if (grepl("^[0-9A-Za-z_\\-]{11}$", url, ignore.case = TRUE, perl = TRUE)) {
    return(url)
  }  
  
  url <- httr::parse_url(url)
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

  video_id
}

#' @title Check if macOS
#' 
#' @description This function checks if running the macOS version of R.
#' 
#' @return Result as logical.
#' 
#' @keywords internal
#' @export
isMac <- function() {
  macMatch <- grep("darwin", R.Version()$os)
  if (length(macMatch) != 0) {  return(TRUE) }
  FALSE
}

#' @title Check system fonts
#' 
#' @description Looks up installed system fonts.
#' 
#' @return Returns installed system font families.
#' 
#' @keywords internal
#' @export
getSystemFontFamilies <- function() {
  unique(systemfonts::system_fonts()$family)
}

#' @title Add message to log queue
#' 
#' @description This function adds a text message to a queue or list with a count limiting how many messages are 
#' stored. The queue stores count messages based on first in first out.
#' 
#' @param messages Character vector. Lines or text log messages.
#' @param add_message Character string. Text log message to add to messages
#' @param txt Logical. Return messages as single character string delimited by newline characters.
#' @param count Numeric. Return queue of count messages and discard the rest.
#' 
#' @return Messages as vector or character string.
#'  
#' @keywords internal
#' @export
logMessage <- function(messages, add_message, txt = FALSE, count = 20) {
  
  add_message <- c(paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), add_message))
  log_messages <- c(add_message, messages)

  if (length(log_messages) > count) { log_messages <- log_messages[1:count] }
  if (txt) { return(paste0(log_messages, collapse = '\n')) }
  
  log_messages
}
