#' VOSON Dashboard Text Analysis Module
#'
#' Shiny module functions to create and render groups of plots dynamically.
#'

#### ui ---------------------------------------------------------------------------------------------------------------

#' UI container for plots
#' 
#' @param id shiny module namespace id
#' 
#' @return None
#'
taPlotContainerUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ta_plot"))
}

#' Creates dynamic ui plot placeholders generated from list of data
#' 
#' @param input shiny module namespaced input parameter
#' @param output shiny module namespaced output parameter
#' @param session shiny module namespaced session parameter
#' @param data voson dashboard text analysis plot list data structure
#'   data structure is named list:
#'     data[plot-id] <- list(graph_attr = list(attribute_name, attribute_value), VCorpus)
#'
#' @return None
#' 
taPlotPlaceholders <- function(input, output, session, data) {
  ns <- session$ns
  
  plotPlaceholders <- reactive({
    tag_list <- tagList()
    plot_ids <- names(sapply(data, names))
    
    for (i in seq_along(data)) {
      title_cat <- data[[i]][[1]][[1]]
      title_attr <- paste0(data[[i]][[1]][[2]], collapse = " / ")
      
      title_tags <- fluidRow(column(width = 12, div(h4(title_cat, " ", title_attr), style = "padding-left:20px;")))
      tag_list <- tagAppendChild(tag_list, title_tags)
      
      plot_id <- ns(plot_ids[i])
      plot_tags <- fluidRow(column(width = 12, plotOutput(plot_id, height = ta_plot_height)))
      tag_list <- tagAppendChild(tag_list, plot_tags)
    }
    
    # creates a plot placeholder with the namespace id "no-data"
    if (length(tag_list) == 0) {
      tag_list <- tagAppendChild(tag_list, fluidRow(column(width = 12, plotOutput(ns("no-data"),
                                                                                  height = ta_plot_height))))
    }
    
    tag_list
  })
  
  output$ta_plot <- renderUI({
    plotPlaceholders()
  })
}

#### server -----------------------------------------------------------------------------------------------------------

#' Renders plots to placeholders
#'
#' @param input shiny module namespaced input parameter
#' @param output shiny module namespaced output parameter
#' @param session shiny module namespaced session parameter
#' @param data voson dashboard text analysis plot list data structure
#'   data structure is named list:
#'     data[plot-id] <- list(graph_attr = list(attribute_name, attribute_value), VCorpus)
#' @param seed value to seed rendering of word clouds
#' @param categories list of all categorical attribures in the data set
#' @param min_freq minimum word frequency for word frequency charts or word clouds
#' @param max_words maximum number of words to render in word clouds
#' @param top_count number of words to render in word frequency charts
#' @param type code for type of plots "wf" word frequencies or "wc" word clouds
#' 
#' @return None
#'
taPlotList <- function(input, output, session, data, seed, categories, min_freq, max_words, top_count, type) {
  ns <- session$ns
  
  wordFreqPlotList <- reactive({
    plot_ids <- names(sapply(data, names))
    
    isolate({ withProgress(message = "Processing frequency plots...", {
      
      for (i in seq_along(data)) {
        local({
          local_i <- i
          plot_id <- plot_ids[local_i]
          output[[plot_id]] <- renderPlot({
            VOSONDash::wordFreqChart(data[[local_i]], categories, min_freq, top_count)
          })
        })
      }
      
      # renders empty plot to namespace id "no-data" placeholder
      if (length(data) < 1) {
        output[["no-data"]] <- renderPlot({
          VOSONDash::emptyPlotMessage("No text data.")
        })
      }
      
    }) })
  })
  
  wordCloudPlotList <- reactive({
    plot_ids <- names(sapply(data, names))
    
    isolate({ withProgress(message = "Processing cloud plots...", {
      
      for (i in seq_along(data)) {
        local({
          local_i <- i
          plot_id <- plot_ids[local_i]
          output[[plot_id]] <- renderPlot({
            VOSONDash::wordCloudPlot(data[[local_i]], seed, categories, min_freq, max_words)
          })
        })
      }
      
      # renders empty plot to namespace id "no-data" placeholder
      if (length(data) < 1) {
        output[["no-data"]] <- renderPlot({
          VOSONDash::emptyPlotMessage("No text data.")
        })
      }
    }) })
  })

  wordSentPlotList <- reactive({
    plot_ids <- names(sapply(data, names))
    
    isolate({ withProgress(message = "Processing sentiment plots...", {
      
      for (i in seq_along(data)) {
        local({
          local_i <- i
          plot_id <- plot_ids[local_i]
          output[[plot_id]] <- renderPlot({
            VOSONDash::wordSentChart(data[[local_i]], categories)
          })
        })
      }
      
      # renders empty plot to namespace id "no-data" placeholder
      if (length(data) < 1) {
        output[["no-data"]] <- renderPlot({
          VOSONDash::emptyPlotMessage("No text data.")
        })
      }
      
    }) })
  })
  
  # determine type of plots to generate
  if (type == "wf") {
    wordFreqPlotList()
  } else if (type == "wc") {
    wordCloudPlotList()
  } else if (type == "ws") {
    wordSentPlotList()
  }
}
