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
          paste("VOSONDash", paste0("v", VOSONDash::getVOSONDashVer()), "\n"),
          paste0(format(Sys.time(), "%d %b %Y %H:%M"), "\n\n"),
  
          paste0(trimws(paste(Sys.getenv("os"), R.Version()$platform)), "\n"),
          paste0(R.version.string, "\n"),
          paste("R shiny", packageVersion("shiny"), "\n"),
  
          paste("\nHome:", Sys.getenv("HOME"), "\n\n", 
                "Checking packages...\n"))
}

if (pkgMsgs == FALSE) {
  loadedPackages <- sapply(requiredPackages, function(x) { 
    suppressPackageStartupMessages(require(x, character.only = TRUE))
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
