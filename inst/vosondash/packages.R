# required packages
requiredPackages <- c("htmlwidgets", 
                      "shinydashboard",
                      "shinyjs", 
                      "DT",
                      "igraph",
                      "networkD3",
                      "visNetwork",
                      "tm",
                      "dplyr",
                      "RColorBrewer",
                      "wordcloud")

# if app is local print package information
if (isLocal) {
  cat("=================================================\n")
  cat(paste("VOSONDash", app_version, app_date, "\n"))
  cat(paste0(format(Sys.time(), "%d %b %Y %H:%M"), "\n\n"))
  
  cat(paste(trimws(Sys.getenv("os")), R.Version()$platform, "\n"))
  cat(paste0(R.version.string, "\n"))
  cat(paste("R shiny", packageVersion("shiny"), "\n"))
  
  cat(paste("\nHome:", Sys.getenv("HOME"), "\n"))
  # cat(paste("Encoding:", save_enc, "\n"))
  cat("\n")
  
  cat("Checking packages...\n")
  missingPackages <- requiredPackages[!(requiredPackages %in% installed.packages()[, "Package"])]
  
  if (length(missingPackages) > 0) {
    cat("Required Packages Missing:\n")
    packageStr <- sapply(missingPackages, function(x) paste0("- ", x))
    cat(paste0(packageStr, collapse = "\n"))

    cat("\n\nPlease install required packages before using VOSONDash:\n\n")
  
    packageStr <- sapply(missingPackages, function(x) paste0("\"", x, "\""))
    cat(paste0("install.packages(c(", paste0(packageStr, collapse = ","), "))\n"))
    cat("\n")
    
    stop("Missing packages.", call. = FALSE)
  } else {
    cat("Found all required packages.\n")
    packageStr <- sapply(requiredPackages, function(x) paste0("- ", x, " [", packageVersion(x), "]"))
    cat(paste0(packageStr, collapse = "\n"))
    cat("\n\nStarting VOSONDash...\n")
  }
}

# load required packages
if (suppressLibMsgs) {
  suppressMessages({
    sapply(requiredPackages, function(x) library(x, character.only = TRUE))
  })  
} else {
  sapply(requiredPackages, function(x) library(x, character.only = TRUE))
}
