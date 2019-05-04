isMissingPackages <- FALSE

# if app is local print package information
if (isLocal) {
  cat("=================================================\n")
  cat(paste0("VOSONDash ", app_version, " ", app_date, "\n"))
  cat(paste0(format(Sys.time(), "%d %b %Y %H:%M"), "\n\n"))
  
  cat(paste0(Sys.getenv("os"), " ", R.Version()$platform, "\n"))
  cat(paste0(R.version.string, "\n"))
  cat(paste0("R shiny ", packageVersion("shiny"), "\n"))
  # cat("Locales:\n")
  # cat(paste0(as.list(strsplit(Sys.getlocale(), ";")[[1]]), collapse = "\n"), "\n")
  cat("\n")
  
  # required packages
  requiredPackages <- c("htmlwidgets", 
                        "shinydashboard",
                        "shinyjs", 
                        "DT",
                        "networkD3",
                        "visNetwork",
                        "igraph",
                        "SnowballC",
                        "tm",
                        "lattice",
                        "RColorBrewer",
                        "wordcloud",
                        "syuzhet",
                        "dplyr",
                        "httr",
                        "vosonSML")
  
  cat("Checking packages...\n")
  missingPackages <- requiredPackages[!(requiredPackages %in% installed.packages()[, "Package"])]
  
  if(length(missingPackages) > 0) {
    cat("Required Packages Missing:\n")
    packageStr <- sapply(missingPackages, function(x) paste0("- ", x))
    cat(paste0(packageStr, collapse = "\n"))
    cat("\n\n")
    
    cat("Please install required packages before using VOSONDash:\n\n")
  
    packageStr <- sapply(missingPackages, function(x) paste0("\"", x, "\""))
    cat(paste0("install.packages(c(", paste0(packageStr, collapse = ","), "), dependencies = TRUE)\n"))
    cat("\n")
    
    isMissingPackages <- TRUE
  } else {
    cat("Found all required packages.\n")
    packageStr <- sapply(requiredPackages, function(x) paste0("- ", x, " [", packageVersion(x), "]"))
    cat(paste0(packageStr, collapse = "\n"))
    cat("\n\n")
    cat("Starting VOSONDash...\n")
  }
}

# load libraries
suppressMessages({
    library(shiny)
    library(htmlwidgets)
    library(shinydashboard)
    library(shinyjs)
    library(DT)
    library(networkD3)
    library(visNetwork)
    library(igraph)
    library(SnowballC)
    library(tm)
    library(lattice)
    library(RColorBrewer)
    library(wordcloud)
    library(syuzhet)
    library(dplyr)
    library(httr)
    library(vosonSML)
})