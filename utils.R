#' VOSON Dashboard Utils
#'
#' Helper functions. 
#'

#' Writes captured R console output to a shiny app ui element.
#' Output is written upon completion not in real time.
#' 
#' @param ui_id shiny ui element id as character string
#' @param value code block with console output
#'
#' @return results as captured output
#' 
withConsoleRedirect <- function(ui_id, value) {
  
  input_text <- capture.output(results <- value, type = "output")
  
  if (length(input_text) > 0) {
    output <- gsub("\n{2,}", "\n", input_text)
    insertUI(paste0("#", ui_id), where = "beforeEnd",
             ui = paste0(output, "\n", collapse = "")
    )
  }

  return(results)
}

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