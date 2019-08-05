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
  message("=================================================\n",
          paste("VOSONDash", paste0("v", VOSONDash::getVOSONDashVer()), "\n"),
          paste0(format(Sys.time(), "%d %b %Y %H:%M"), "\n\n"),
  
          paste0(trimws(paste(Sys.getenv("os"), R.Version()$platform)), "\n"),
          paste0(R.version.string, "\n"),
          paste("R shiny", packageVersion("shiny"), "\n"),
  
          paste("\nHome:", Sys.getenv("HOME"), "\n\n", 
                "Checking packages...\n"))
}

if (suppressLibWarn) {
  suppressWarnings({
    loadedPackages <- sapply(requiredPackages, function(x) { require(x, character.only = TRUE, quietly = TRUE) })
  })
} else {
  loadedPackages <- sapply(requiredPackages, function(x) { require(x, character.only = TRUE) })
}

if (isLocal) {
  if (any(loadedPackages == FALSE)) {
    missingPackages <- names(which(loadedPackages == FALSE))
    
    message("Required Packages Missing:\n",
            paste0(missingPackages, collapse = "\n"),
            "\n\nPlease install required packages before using VOSONDash:\n\n")
    
    packageStr <- sapply(missingPackages, function(x) paste0("\"", x, "\""))
    message(paste0("install.packages(c(", paste0(packageStr, collapse = ","), "))\n"), "\n")
  
    stop("Missing packages.", call. = FALSE)
  } else {
    message("Found all required packages.\n",
            "\nStarting VOSONDash...\n")
  }
}
