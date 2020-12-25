# required packages
requiredPackages <- c("dplyr",
                      "DT",
                      "htmlwidgets",
                      "igraph",
                      "RColorBrewer",
                      "shinydashboard",
                      "shinyjs",
                      "tm",
                      "visNetwork",
                      "wordcloud")

# if app is local print package information
if (isLocal) {
  message("=================================================\n",
          paste0("VOSONDash",
                 " v", utils::packageVersion("VOSONDash"),
                 " (", utils::packageDate("VOSONDash"), ")\n\n"),
  
          paste0(trimws(paste(Sys.getenv("os"), R.Version()$platform)), "\n"),
          paste0(R.version.string, "\n"),
          paste0("R shiny v", utils::packageVersion("shiny"), "\n"),
  
          paste0("\nHome: ", Sys.getenv("HOME"), "\n\n"), 
          paste0("Checking packages...\n"))
}

if (pkgMsgs == FALSE) {
  loadedPackages <- sapply(requiredPackages, function(x) { 
    suppressWarnings(suppressPackageStartupMessages(require(x, character.only = TRUE)))
  })
} else {
  loadedPackages <- sapply(requiredPackages, function(x) { require(x, character.only = TRUE) })  
}

if (isLocal) {
  if (any(loadedPackages == FALSE)) {
    missingPackages <- names(which(loadedPackages == FALSE))
    
    err_msg <- paste0("Required packages missing.\n",
                      paste0(sapply(missingPackages, function(x) paste0("- ", x)), 
                             collapse = "\n"),
                      "\n\nPlease install required packages before using VOSONDash:\n\n")
    
    packageStr <- sapply(missingPackages, function(x) paste0("\"", x, "\""))
    err_msg <- paste0(err_msg, 
                      paste0("install.packages(c(", paste0(packageStr, collapse = ","), "))\n"), 
                      "\n")
  
    stop(err_msg, call. = FALSE)
  } else {
    message("Found all required packages.\n",
            "\nStarting VOSONDash...\n")
  }
}
