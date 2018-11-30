#' VOSON Dashboard twitterServer
#'
#' Collects tweets and creates an actor network using the vosonSML package.
#'

#### values ----------------------------------------------------------------------------------------------------------- #

twitter_rvalues <- reactiveValues()
twitter_rvalues$twitter_data <- NULL      # dataframe returned by vosonSML collection
twitter_rvalues$twitter_graphml <- NULL   # graphml object returned from collection

# twitter api keys
twitter_api_keyring <- list(
  twitter_api_key = "",
  twitter_api_secret = "",
  twitter_access_token = "",
  twitter_access_token_secret = ""
)

# twitter search parameters
twitter_search_term <- NULL
twitter_search_type <- NULL

twitter_retweets <- NULL
twitter_tweet_count <- NULL
twitter_language <- NULL

twitter_date_since <- NULL
twitter_date_until <- NULL

twitter_filter_from <- NULL
twitter_filter_to <- NULL
twitter_filter_safe <- NULL
twitter_filter_media <- NULL
twitter_filter_url <- NULL
twitter_filter_negative <- NULL
twitter_filter_positive <- NULL

#### events ----------------------------------------------------------------------------------------------------------- #

# set twitter api keys on input
observeEvent({input$twitter_api_key_input
  input$twitter_api_secret_input
  input$twitter_access_token_input
  input$twitter_access_token_secret_input}, {
    
    setTwitterAPIKeys()
  })

# set twitter parameters on input
observeEvent({input$twitter_search_term_input
  input$twitter_retweets_check
  input$twitter_search_type_select
  input$twitter_date_since_input
  input$twitter_date_until_input
  input$twitter_filter_from
  input$twitter_filter_to
  input$twitter_filter_safe
  input$twitter_filter_media
  input$twitter_filter_url
  input$twitter_filter_negative
  input$twitter_filter_positive              
}, {
  
  setTwitterParams()
})

# set twitter language parameter and reset if greater than two alpha characters
observeEvent(input$twitter_language_input, {
  if (!is.na(input$twitter_language_input)) {
    alpha_check <- grep("^[[:alpha:]]+$", input$twitter_language_input, value = FALSE)
    if (nchar(input$twitter_language_input) > 2 || identical(alpha_check, integer(0))) {
      updateTextInput(session, "twitter_language_input", value = "")
    }
  }
  
  setTwitterParams()
})

# set tweet count parameter and reset if not numeric or less than one
observeEvent(input$twitter_tweet_count_input, {
  if (!is.na(input$twitter_tweet_count_input)) {
    if (!is.numeric(input$twitter_tweet_count_input) || input$twitter_tweet_count_input < 1) {
      updateNumericInput(session, "twitter_tweet_count_input", value = g_default_tweet_count)
    }
  }
  
  setTwitterParams()
})

# twitter collection button pushed
observeEvent(input$twitter_collect_button, {
  
  # disable button so it is not pushed again
  shinyjs::disable("twitter_collect_button")
  
  withProgress(message = 'Collecting tweets', value = 0.5, {
    
    withConsoleRedirect("twitter_console", {
      # collect twitter data and print any output to console
      tryCatch({
        search_term <- twitter_search_term
        search_type <- twitter_search_type
        
        twitter_search_options <- c()
        
        if (nchar(twitter_filter_from) > 2) {
          twitter_search_options <- append(twitter_search_options, paste0("from:", twitter_filter_from))
        }
        
        if (nchar(twitter_filter_to) > 2) {
          twitter_search_options <- append(twitter_search_options, paste0("to:", twitter_filter_to))
        }
        
        opts <- list("-filter:nativeretweets" = !twitter_retweets,
                     "filter:safe" = !twitter_filter_safe,
                     "filter:media" = !twitter_filter_media,
                     "filter:links" = !twitter_filter_url,
                     ":)" = !twitter_filter_positive,
                     ":(" = !twitter_filter_negative)
        
        sapply(names(opts), function(x) {
          if (!isNullOrEmpty(opts[x])) {
            if (opts[x] == FALSE) { twitter_search_options <<- append(twitter_search_options, x) }
          }
        })
        
        if (length(twitter_search_options) > 0) {
          search_term <- ifelse(nchar(trimws(search_term)) > 0, paste0(search_term, " "), "")
          search_term <- paste0(search_term, paste0(twitter_search_options, collapse = " "))
        }
        
        twitter_rvalues$twitter_data <<- collectTwitterData(twitter_api_keyring, search_term, search_type,
                                                            twitter_tweet_count, twitter_language, 
                                                            twitter_date_since, twitter_date_until)
        
      }, error = function(err) {
        incProgress(1, detail = "Error")
        cat(paste("twitter collection error: ", err))
        return(NULL)
      })
      
      incProgress(0.5, detail = "Creating network")
      
      # if twitter data collected create graphml object
      if (!is.null(twitter_rvalues$twitter_data)) {
        tryCatch({
          # twitter_rvalues$twitter_graphml <<- createTwitterActorNetwork(twitter_rvalues$twitter_data)
          netList <- createTwitterActorNetwork(twitter_rvalues$twitter_data)
          twitter_rvalues$twitter_graphml <<- netList$network
          twitter_rvalues$twitterWT_graphml <<- netList$networkWT   # "with text" (edge attribute)
        }, error = function(err) {
          incProgress(1, detail = "Error")
          cat(paste("twitter graphml error: ", err))
          return(NULL)
        })
      }
      
      incProgress(1, detail = "Finished")
    })
    
  }) # withProgress
  
  # enable button
  twitterArgumentsOutput()
})

# enable twitter download data button when there is twitter data
observeEvent(twitter_rvalues$twitter_data, {
  if (!is.null(twitter_rvalues$twitter_data) && nrow(twitter_rvalues$twitter_data) > 0) {
    shinyjs::enable("download_twitter_data_button")
  } else {
    shinyjs::disable("download_twitter_data_button")
  }
})

# enable twitter download graphml button when there is twitter graphml data
observeEvent(twitter_rvalues$twitter_graphml, {
  if (!is.null(twitter_rvalues$twitter_graphml)) {
    shinyjs::enable("download_twitter_graph_button")
    shinyjs::enable("download_twitter_graphWT_button")
    shinyjs::enable("view_twitter_graph_button")
    shinyjs::enable("view_twitter_graphWT_button")
  } else {
    shinyjs::disable("download_twitter_graph_button")
    shinyjs::disable("download_twitter_graphWT_button")
    shinyjs::disable("view_twitter_graph_button")
    shinyjs::disable("view_twitter_graphWT_button")
  }
})

observeEvent(input$view_twitter_graph_button, {
  
  if (!is.null(isolate(twitter_rvalues$twitter_graphml))) {
    # clear graph file data
    shinyjs::reset("graphml_data_file")
    
    # set graph data
    ng_rvalues$graph_data <<- isolate(twitter_rvalues$twitter_graphml)
    
    ng_rvalues$graph_desc <<- paste0("Twitter network for search term: ", twitter_search_term, sep = "")
    ng_rvalues$graph_type <<- "twitter"
    ng_rvalues$graph_name <<- "" # only used when graph loaded from file
    
    # get a random number to seed graphs - experimental
    ng_rvalues$graph_seed <<- sample(g_random_number_range[1]:g_random_number_range[2], 1)
    
    ng_rvalues$graph_CA <- c()
    ng_rvalues$graph_CA_selected <- ""
    
    # until reactivity issue
    setGraphFilterControls()
    
    # change to graphs tab
    updateTabItems(session, "sidebar_menu", selected = "network_graphs_tab")
  }
})

observeEvent(input$view_twitter_graphWT_button, {
  
  if (!is.null(isolate(twitter_rvalues$twitterWT_graphml))) {
    # clear graph file data
    shinyjs::reset("graphml_data_file")
    
    # set graph data
    ng_rvalues$graph_data <<- isolate(twitter_rvalues$twitterWT_graphml)
    
    ng_rvalues$graph_desc <<- paste0("Twitter network for search term: ", twitter_search_term, sep = "")
    ng_rvalues$graph_type <<- "twitter"
    ng_rvalues$graph_name <<- "" # only used when graph loaded from file
    
    # get a random number to seed graphs - experimental
    ng_rvalues$graph_seed <<- sample(g_random_number_range[1]:g_random_number_range[2], 1)
    
    ng_rvalues$graph_CA <- c()
    ng_rvalues$graph_CA_selected <- ""
    
    # until reactivity issue
    setGraphFilterControls()
    
    # change to graphs tab
    updateTabItems(session, "sidebar_menu", selected = "network_graphs_tab")
  }
})

#### output ----------------------------------------------------------------------------------------------------------- #

# render twitter collection arguments
output$twitter_arguments_output <- renderText({
  # dependencies
  input$twitter_api_key_input
  input$twitter_api_secret_input
  input$twitter_access_token_input
  input$twitter_access_token_secret_input
  
  input$twitter_search_term_input
  input$twitter_search_type_select
  input$twitter_retweets_check
  input$twitter_tweet_count_input
  input$twitter_language_input
  input$twitter_date_since_input
  input$twitter_date_until_input
  
  input$twitter_filter_from
  input$twitter_filter_to
  input$twitter_filter_safe
  input$twitter_filter_media
  input$twitter_filter_url
  input$twitter_filter_negative
  input$twitter_filter_positive
  
  # get twitter collection arguments output
  twitterArgumentsOutput()
})

# render twitter data table
output$dt_twitter_data <- DT::renderDataTable({
  datatableTwitterData()
})

# set file name and content for twitter data download
output$download_twitter_data_button <- downloadHandler(
  filename = function() {
    systemTimeFilename("twitter-data", "rds")
  },
  
  content = function(file) {
    saveRDS(isolate(twitter_rvalues$twitter_data), file)
    # data <- isolate(twitter_rvalues$twitter_data)
    # data$users_mentioned <- vapply(data$users_mentioned, paste, collapse = ", ", character(1L))
    # write.csv(data, file)
  }
)

# set file name and content for twitter graphml download
output$download_twitter_graph_button <- downloadHandler(
  filename = function() {
    systemTimeFilename("twitter", "graphml")
  },
  
  content = function(file) {
    write_graph(isolate(twitter_rvalues$twitter_graphml), file, format=c("graphml"))
  }
)

# set file name and content for twitter graphml (with text) download
output$download_twitter_graphWT_button <- downloadHandler(
  filename = function() {
    systemTimeFilename("twitter-with-text", "graphml")
  },
  
  content = function(file) {
    write_graph(isolate(twitter_rvalues$twitterWT_graphml), file, format=c("graphml"))
  }
)

#### reactives -------------------------------------------------------------------------------------------------------- #

setTwitterAPIKeys <- reactive({
  twitter_api_keyring$twitter_api_key <<- trimws(input$twitter_api_key_input)
  twitter_api_keyring$twitter_api_secret <<- trimws(input$twitter_api_secret_input)
  twitter_api_keyring$twitter_access_token <<- trimws(input$twitter_access_token_input)
  twitter_api_keyring$twitter_access_token_secret <<- trimws(input$twitter_access_token_secret_input)
})

setTwitterParams <- reactive({
  twitter_search_term <<- trimws(input$twitter_search_term_input)
  twitter_search_term <<- gsub("\n{1, }", " ", twitter_search_term)
  # twitter_search_term <<- gsub(" {2, }", " ", trimws(input$twitter_search_term_input)) # too helpful
  
  twitter_search_type <<- input$twitter_search_type_select
  twitter_retweets <<- input$twitter_retweets_check
  twitter_tweet_count <<- input$twitter_tweet_count_input
  twitter_language <<- input$twitter_language_input
  
  twitter_date_since <<- trimws(input$twitter_date_since_input)
  twitter_date_until <<- trimws(input$twitter_date_until_input)
  
  twitter_filter_from <<- trimws(input$twitter_filter_from)
  twitter_filter_from <<- gsub("^@", "", twitter_filter_from)
  twitter_filter_to <<- trimws(input$twitter_filter_to)
  twitter_filter_to <<- gsub("^@", "", twitter_filter_to)
  
  twitter_filter_safe <<- input$twitter_filter_safe
  twitter_filter_media <<- input$twitter_filter_media
  twitter_filter_url <<- input$twitter_filter_url
  twitter_filter_negative <<- input$twitter_filter_negative
  twitter_filter_positive <<- input$twitter_filter_positive
})

# create data table from collected twitter data
datatableTwitterData <- reactive({
  
  data <- twitter_rvalues$twitter_data
  
  # x <- vapply(data$users_mentioned, length, 1L)
  # data <- data[rep(rownames(data), x), ]
  # data$users_mentioned <- unlist(data$users_mentioned, use.names = FALSE)
  
  data$users_mentioned <- vapply(data$users_mentioned, paste, collapse = ", ", character(1L))
  data$hashtags_used <- vapply(data$hashtags_used, paste, collapse = ", ", character(1L))
  
  if (!is.null(twitter_rvalues$twitter_data)) {
    col_defs <- NULL
    if (input$dt_twitter_truncate_text_check == TRUE) {
      col_defs <- g_dt_col_defs
      col_defs[[1]]$targets <- c(1)
    }
    DT::datatable(data, extensions = 'Buttons', 
                  options = list(lengthMenu = g_dt_length_menu, pageLength = g_dt_page_length, scrollX = TRUE,
                                 columnDefs = col_defs, dom = 'lBfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'print')), class = 'cell-border stripe compact hover')
  }
})

#### functions -------------------------------------------------------------------------------------------------------- #

# format twitter collection arguments output
twitterArgumentsOutput <- function() {
  
  output <- c()
  check_keys <- sapply(twitter_api_keyring, isNullOrEmpty)
  
  if (any(check_keys == FALSE)) {
    output <- append(output, paste0("api keys: ", 
                                    trimws(paste0(sapply(strtrim(twitter_api_keyring, 6), function(x) paste0(x, "...", sep = "")), collapse = ', '))))
  }
  
  search_term_flag <- FALSE
  if (!isNullOrEmpty(twitter_search_term)) {
    temp_search_term <- twitter_search_term
    output <- append(output, paste0("search term: ", trimws(temp_search_term)))
    search_term_flag <- TRUE
  }
  
  # if (search_term_flag) {
  output <- append(output, paste0("results type: ", trimws(twitter_search_type)))
  # }
  
  twitter_search_options <- c()
  
  if (nchar(twitter_filter_from) > 2) {
    twitter_search_options <- append(twitter_search_options, paste0("from:", twitter_filter_from))
    search_term_flag <- TRUE
  }
  
  if (nchar(twitter_filter_to) > 2) {
    twitter_search_options <- append(twitter_search_options, paste0("to:", twitter_filter_to))
    search_term_flag <- TRUE
  }
  
  opts <- list("-filter:nativeretweets" = !twitter_retweets,
               "filter:safe" = !twitter_filter_safe,
               "filter:media" = !twitter_filter_media,
               "filter:links" = !twitter_filter_url,
               ":)" = !twitter_filter_positive,
               ":(" = !twitter_filter_negative)
  
  sapply(names(opts), function(x) {
    if (!isNullOrEmpty(opts[x])) {
      if (opts[x] == FALSE) { twitter_search_options <<- append(twitter_search_options, x) }
    }
  })
  
  if (!isNullOrEmpty(twitter_tweet_count) && is.numeric(twitter_tweet_count)) {
    output <- append(output, paste0("number of tweets: ", twitter_tweet_count))
  }
  
  if (!isNullOrEmpty(twitter_language)) {
    output <- append(output, paste0("language: ", twitter_language))
  }
  
  if (!isNullOrEmpty(twitter_date_since)) {
    output <- append(output, paste0("since date: ", twitter_date_since, sep = ""))
  }
  
  if (!isNullOrEmpty(twitter_date_until)) {
    output <- append(output, paste0("until date: ", twitter_date_until, sep = ""))
  }
  
  if (length(twitter_search_options) > 0) {
    output <- append(output, paste0("filters: ", paste0(twitter_search_options, collapse = " ")))
  }
  
  # if api key and video ids have been inputed enable collect button
  if (!any(check_keys == TRUE) && search_term_flag) {
    shinyjs::enable("twitter_collect_button")
  } else {
    shinyjs::disable("twitter_collect_button")
  }
  
  paste0(output, collapse = '\n')
}
