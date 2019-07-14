#' Check a shiny app input value for a range of empty conditions
#' 
#' @param x shiny input value
#' @return result as logical
#' @keywords internal
#'
#' @export
isNullOrEmpty <- function(x) {
  if (is.null(x) || identical(x, character(0)) || is.na(x) || trimws(x) == "") {
    return(TRUE)
  }
  
  FALSE
}

#' Create a file-system friendly file name with system date time prefix
#' 
#' @param name_suffix name part of file name to append to date time as character string
#' @param name_ext file name extension as character string
#' @param clean remove problematic characters from file name as boolean
#' @return file_name as character string
#' @keywords internal
#' 
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

#' Return reddit url appropriate for reddit API comments request
#' 
#' @param url reddit thread url
#' @return url as character string
#' @keywords internal
#'
#' @export
createRedditRequestUrl <- function(url) {
  url <- tolower(url)
  
  # base_url       <- "https://reddit.com/r/"
  # base_url_oauth <- "https://oauth.reddit.com/r/"
  
  if(!grepl("^https://(www\\.)?reddit.com/r/(.*?)/comments/([0-9A-Za-z]{6})?/.*$", 
            url, ignore.case = TRUE, perl = TRUE)) {
    return(NULL)
  }
  
  # "https://oauth.reddit.com/r/"
  url <- gsub("^https://(www\\.)?reddit.com/r/", "r/", 
              url, ignore.case = TRUE, perl = TRUE)
  
  url <- gsub("^(.*)?/comments/([0-9A-Za-z]{6})?/.*?(/)?$", "\\1/comments/\\2/", 
              url, ignore.case = TRUE, perl = TRUE)
}

#' Extract thread id from a reddit thread url
#' 
#' @param url reddit thread url
#' @return thread_id as character string
#'
#' @export
getRedditUrlThreadId <- function(url) {
  thread_id <- gsub("^(.*)?/comments/([0-9A-Za-z]{6})?/.*?(/)?$", "\\2", 
                    url, ignore.case = TRUE, perl = TRUE)
}

#' Extract subreddit name from a reddit thread url
#' 
#' @param url reddit thread url
#' @return subreddit as character string
#'
#' @export
getRedditUrlSubreddit <- function(url) {
  subreddit <- gsub("^(.*)?/r/(.*)?/comments/.*?(/)?$", "\\2", 
                    url, ignore.case = TRUE, perl = TRUE)  
}

#' Extract youtube video id from url
#' 
#' @param url youtube video url
#' @return video_id as character string
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

#' Check if macOS version of R
#' 
#' @return result as boolean
#' @keywords internal
#'
#' @export
isMac <- function() {
  macMatch <- grep("darwin", R.Version()$os)
  
  if (length(macMatch) != 0) {
    return(TRUE)
  }
  
  FALSE
}

#' Simple text aggregation
#' 
#' @param lines vector of text lines
#' @param add_line text line to add to lines
#' @param txt return lines as character string
#' @return text lines as vector or character string 
#' @keywords internal
#'
#' @export
addLine <- function(lines, add_line, txt = FALSE) {
  
  if (isNullOrEmpty(lines)) { lines <- c() }
  lines <- append(lines, add_line)
  
  if (txt) { return(paste0(lines, collapse = '\n')) }
  
  lines
}

#' Simple logging queue with count FIFO
#' 
#' @param messages vector of text messages
#' @param add_message text message to add to the messages queue
#' @param txt return lines as character string
#' @param count return queue of count messages and discard the rest
#' @return messages as vector or character string 
#' @keywords internal
#'
#' @export
logMessage <- function(messages, add_message, txt = FALSE, count = 20) {
  
  add_message <- c(paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), add_message))
  log_messages <- c(add_message, messages)

  if (length(log_messages) > count) { log_messages <- log_messages[1:count] }
  if (txt) { return(paste0(log_messages, collapse = '\n')) }
  
  log_messages
}
