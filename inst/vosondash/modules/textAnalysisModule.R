#' VOSON Dashboard Text Analysis Module
#'
#' Shiny module functions to create and render groups of plots dynamically.
#'

#### ui ---------------------------------------------------------------------------------------------------------------

#' @title UI container for plots
#' 
#' @param id shiny module namespace id
#' 
#' @return None
#'
taPlotContainerUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ta_plot"))
}

#' @title Create dynamic UI plot placeholders generated from list of datasets
#' 
#' @param input shiny module namespaced input parameter
#' @param output shiny module namespaced output parameter
#' @param session shiny module namespaced session parameter
#' @param data voson dashboard text analysis plot list data structure
#'   data structure is named list:
#'     data[plot-id] <- list(graph_attr = list(cat = c("category name"), 
#'                                             sub_cats = c("sub category", "sub category 2")), 
#'                           corp = VCorpus)
#'
#' @return None
#' 
taPlotPlaceholders <- function(input, output, session, data, sub_plots = 1) {
  ns <- session$ns
  
  plotPlaceholders <- reactive({
    tag_list <- tagList()
    plot_ids <- names(data)

    for (i in seq_along(data)) {
      title_cat <- unlist(data[[i]]$graph_attr$cat)
      title_sub_cats <- paste0(unlist(data[[i]]$graph_attr$sub_cats), collapse = " / ")          
      
      title_tags <- fluidRow(column(width = 12, div(h4(title_cat, " ", title_sub_cats), style = "padding-left:20px;")))
      tag_list <- tagAppendChild(tag_list, title_tags)

      plot_id <- ns(plot_ids[i])
      if (sub_plots == 2) {
        plot_tags <- fluidRow(column(width = 8, plotOutput(paste0(plot_id, "-a"), height = ta_plot_height)),
                              column(width = 4, plotOutput(paste0(plot_id, "-b"), height = ta_plot_height)))
      } else {
        plot_tags <- fluidRow(column(width = 12, plotOutput(plot_id, height = ta_plot_height)))
      }
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
#'     data[plot-id] <- list(graph_attr = list(cat = c("category name"), 
#'                                             sub_cats = c("sub category", "sub category 2")), 
#'                           corp = VCorpus)
#' @param seed value to seed rendering of word clouds
#' @param categories list of all categorical attribures in the data set
#' @param min_freq minimum word frequency for word frequency charts or word clouds
#' @param max_words maximum number of words to render in word clouds
#' @param top_count number of words to render in word frequency charts
#' @param type code for type of plots "wf" word frequencies or "wc" word clouds
#' @param col_palette color palette for plots
#' @param word_length word bounds min max
#' @param mac_arial use arial unicode for macos
#' @param wc_seed wordcloud plot seed value
#' @param wc_random_order wordcloud random word order
#' @param wc_random_col wordcloud random colors
#' @param wc_vert_prop wordcloud proportion of vertical words
#'  
#' @return None
#'
taPlotList <- function(input, output, session, data, seed, categories, min_freq, max_words, top_count, type, 
                       col_palette, word_length = c(3, 26), mac_arial = TRUE,
                       wc_seed = 100, wc_random_order = FALSE, wc_random_col = FALSE, wc_vert_prop = 0.1) {
  ns <- session$ns
  
  wordFreqPlotList <- reactive({
    plot_ids <- names(data)
    
    isolate({ withProgress(message = "Processing frequency plots...", {
      
      for (i in seq_along(data)) {
        local({
          local_i <- i
          plot_id <- plot_ids[local_i]
          output[[plot_id]] <- renderPlot({
            data_item <- data[[local_i]]
            
            pcolors <- getColors(categories, 
                                 unlist(data_item$graph_attr$cat), 
                                 unlist(data_item$graph_attr$sub_cats), 
                                 "#f5f5f5", col_palette)
            
            wf <- VOSONDash::wordFreqFromCorpus(data_item$corp, word_len = word_length)

            VOSONDash::wordFreqChart(wf,
                                     min_freq,
                                     top_count,
                                     pcolors,
                                     family = setArialUnicodeMS(mac_arial))
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
    plot_ids <- names(data)
    
    isolate({ withProgress(message = "Processing cloud plots...", {
      
      for (i in seq_along(data)) {
        local({
          local_i <- i
          plot_id <- plot_ids[local_i]
          output[[plot_id]] <- renderPlot({
            data_item <- data[[local_i]]
            
            pcolors <- getColors(categories, 
                                 unlist(data_item$graph_attr$cat), 
                                 unlist(data_item$graph_attr$sub_cats), 
                                 "#000000", col_palette)
            
            if (wc_random_col) {
              pcolors <- col_palette
            }
            
            wf <- VOSONDash::wordFreqFromCorpus(data_item$corp, word_len = word_length)
            
            VOSONDash::wordCloudPlot(wf,
                                     wc_seed, min_freq, max_words, pcolors,
                                     random.order = wc_random_order, random.color = wc_random_col,
                                     rot.per = wc_vert_prop,
                                     family = setArialUnicodeMS(mac_arial))
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
    plot_ids <- names(data)
    
    isolate({ withProgress(message = "Processing sentiment plots...", {
      for (i in seq_along(data)) {
        local({
          local_i <- i
          data_item <- data[[local_i]]
          pcolors <- getColors(categories, 
                               unlist(data_item$graph_attr$cat), 
                               unlist(data_item$graph_attr$sub_cats), 
                               "#f5f5f5", col_palette)          
          
          sent_data <- VOSONDash::wordSentData(corp = data_item$corp, word_len = word_length)

          plot_id <- paste0(plot_ids[local_i], "-a")
          output[[plot_id]] <- renderPlot({
            plots <- VOSONDash::wordSentChart(sent_data, pcolors)
          })
          
          plot_id <- paste0(plot_ids[local_i], "-b")
          output[[plot_id]] <- renderPlot({
            plots <- VOSONDash::wordSentValenceChart(sent_data)
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

setArialUnicodeMS <- function(enabled) {
  if (.Platform$OS.type != "windows" & ("Arial Unicode MS" %in% VOSONDash::getSystemFontFamilies()) &
      enabled) {
    return("Arial Unicode MS")
  }
  NULL
}

getColors <- function(categories, plot_category, plot_category_attrs, default_col, col_palette) {
  if (plot_category != "") {
    df <- data.frame("cat" = categories[[plot_category]])
    
    if (nrow(df)) {
      ncats <- ifelse(nrow(df) == 0, 1, nrow(df))
      df$color <- col_palette[1:ncats]
      
      match_t <- match(plot_category_attrs, df$cat)
      colx <- ifelse(!is.na(match_t), df$color[match_t], default_col)
      
      return(colx)
    }
    
    return(default_col)
  }
  
  default_col
}
