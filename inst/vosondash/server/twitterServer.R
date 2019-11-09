#' VOSON Dashboard twitterServer
#'
#' Collects tweets and creates an actor network using the vosonSML package.
#'

#### values ----------------------------------------------------------------------------------------------------------- #

tw_rv <- reactiveValues(
  tw_data = NULL,        # dataframe returned by vosonSML collection
  tw_network = NULL,
  tw_graphml = NULL      # igraph graph object returned from collection
)

test_data <- NULL

tw_rv$data_cols <- NULL

# twitter api keys
tw_api_keyring <- list(
  twitter_app_name = "",
  twitter_api_key = "",
  twitter_api_secret = "",
  twitter_access_token = "",
  twitter_access_token_secret = ""
)

# twitter search parameters
twitter_search_term <- NULL
twitter_search_type <- NULL

twitter_retweets <- NULL
twitter_retry <- NULL
twitter_tweet_count <- NULL
twitter_language <- NULL
twitter_date_until <- NULL

twitter_since_id <- NULL
twitter_max_id <- NULL

twitter_filter_from <- NULL
twitter_filter_to <- NULL
twitter_filter_safe <- NULL
twitter_filter_media <- NULL
twitter_filter_url <- NULL
twitter_filter_negative <- NULL
twitter_filter_positive <- NULL

#### events ----------------------------------------------------------------------------------------------------------- #

# set twitter parameters on input
observeEvent({input$twitter_search_term_input
  input$twitter_retweets_check
  input$twitter_retry_check
  input$twitter_search_type_select
  input$twitter_date_until_input
  input$twitter_since_id_input
  input$twitter_max_id_input
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
      updateNumericInput(session, "twitter_tweet_count_input", value = gbl_def_tweet_count)
    }
  }
  
  setTwitterParams()
})

observeEvent(input$clear_twitter_console, {
  resetConsole("twitter_console")
  # callModule(resetConsoleMod, "twitter")
})
  
# twitter collection button pushed
observeEvent(input$twitter_collect_button, {
  
  # disable button so it is not pushed again
  shinyjs::disable("twitter_collect_button")
  
  withProgress(message = 'Collecting tweets', value = 0.5, {
    
    # callModule(withConsoleRedirectMod, "twitter", value = {
    withConsoleRedirect("twitter_console", {
      
      # collect twitter data and print any output to console
      withCallingHandlers(
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
        
        # "-filter:nativeretweets" = !twitter_retweets,
        opts <- list("filter:safe" = !twitter_filter_safe,
                     "filter:media" = !twitter_filter_media,
                     "filter:links" = !twitter_filter_url,
                     ":)" = !twitter_filter_positive,
                     ":(" = !twitter_filter_negative)
        
        sapply(names(opts), function(x) {
          if (!VOSONDash::isNullOrEmpty(opts[x])) {
            if (opts[x] == FALSE) { twitter_search_options <<- append(twitter_search_options, x) }
          }
        })
        
        if (length(twitter_search_options) > 0) {
          search_term <- ifelse(nchar(trimws(search_term)) > 0, paste0(search_term, " "), "")
          search_term <- paste0(search_term, paste0(twitter_search_options, collapse = " "))
        }
        
        # tw_api_keyring, search_term, search_type, tweet_count, 
        # include_retweets, retry_on_rate_limit,
        # language, date_until, since_id, max_id
        test_data <<- suppressWarnings({
          VOSONDash::collectTwitterData(cred = creds_rv$use_token, search_term, search_type,
                                        twitter_tweet_count, twitter_retweets, twitter_retry, 
                                        twitter_language, twitter_date_until,
                                        twitter_since_id, twitter_max_id) })
        
        tw_rv$tw_data <<- test_data
        
        tw_rv$data_cols <<- names(tw_rv$tw_data)
      }, error = function(err) {
        incProgress(1, detail = "Error")
        cat(paste("twitter collection error: ", err))
        NULL
      }, warning = function(w) {
        incProgress(1, detail = "Warning")
        cat(paste("twitter collection warning: ", w))
        # invokeRestart("muffleWarning")
      })
      )
      
      if (!v029) {
        # if twitter data collected create igraph graph object
        if (!is.null(tw_rv$tw_data) && nrow(tw_rv$tw_data) > 0) {
          incProgress(0.5, detail = "Creating network")
          tryCatch({
            # tw_rv$tw_graphml <<- createTwitterActorNetwork(tw_rv$tw_data)
            netList <- VOSONDash::createTwitterActorNetwork(tw_rv$tw_data)
            tw_rv$tw_graphml <<- netList$network
            tw_rv$twitterWT_graphml <<- netList$networkWT   # "with text" (edge attribute)
          }, error = function(err) {
            incProgress(1, detail = "Error")
            cat(paste("twitter graphml error: ", err))
          })
        }
      }
      
      incProgress(1, detail = "Finished")
      
    }) # withConsoleRedirect
    
  }) # withProgress
  
  # enable button
  twitterArgumentsOutput()
  
  delay(gbl_scroll_delay, js$scroll_console("twitter_console"))
})

observeEvent(tw_rv$tw_data, {
  if (!is.null(tw_rv$tw_data) && nrow(tw_rv$tw_data)) {
    shinyjs::enable("twitter_create_button")
  } else {
    shinyjs::disable("twitter_create_button")
  }
})

observeEvent(input$twitter_create_button, {
  net_type <- input$twitter_network_type_select
  add_text <- input$twitter_network_text
  add_user_data <- input$twitter_network_user_data
  network <- NULL
  
  parse_rem_terms <- function(rem_terms) {
    rem_terms <- unlist(strsplit(rem_terms, ","), use.names = FALSE)
    rem_terms <- sapply(rem_terms, function(x) { 
      term <- trimws(x)
      if (term == "") { return(NA) }
      term
    }, USE.NAMES = FALSE)
    rem_terms <- na.omit(rem_terms)    
  }
  
  shinyjs::disable("twitter_create_button")
  
  withProgress(message = 'Creating network', value = 0.5, {
  
  withConsoleRedirect("twitter_console", {
    if (net_type == "activity") {
      network <- vosonSML::Create(isolate(tw_rv$tw_data), "activity", verbose = TRUE)
      if (add_text) { network <- vosonSML::AddText(network, isolate(tw_rv$tw_data)) }
    } else if (net_type == "actor") {
      network <- vosonSML::Create(isolate(tw_rv$tw_data), "actor", verbose = TRUE)
      if (add_text) { network <- vosonSML::AddText(network, isolate(tw_rv$tw_data)) }
      if (add_user_data) { 
        network <- vosonSML::AddUserData(network, isolate(tw_rv$tw_data), twitterAuth = creds_rv$use_token) 
      }
    } else if (net_type == "twomode") {
      rem_terms <- parse_rem_terms(input$twitter_twomode_remove)
      if (length(rem_terms)) {
        network <- vosonSML::Create(isolate(tw_rv$tw_data), "twomode", removeTermsOrHashtags = rem_terms,
                                    verbose = TRUE)
      } else {
        network <- vosonSML::Create(isolate(tw_rv$tw_data), "twomode", verbose = TRUE) 
      }
    } else if (net_type == "semantic") {
      rem_terms <- parse_rem_terms(input$twitter_semantic_remove)

      network <- vosonSML::Create(isolate(tw_rv$tw_data), "semantic", 
                                  removeTermsOrHashtags = rem_terms,
                                  stopwordsEnglish = input$twitter_semantic_stopwords,
                                  termFreq = input$twitter_term_freq,
                                  hashtagFreq = input$twitter_hashtag_freq,
                                  verbose = TRUE)
    }
    if (!is.null(network)) {
      tw_rv$tw_network <- network
      tw_rv$tw_graphml <- vosonSML::Graph(network) 
    }
  })
  
  incProgress(1, detail = "Finished")
  })
  
  shinyjs::enable("twitter_create_button")
  
  # shinyjs::runjs("jQuery( function() { var pre = jQuery('#twitter_console');
  #                                      pre.scrollTop( pre.prop('scrollHeight') ); }); ")
  
  delay(gbl_scroll_delay, js$scroll_console("twitter_console"))
})

# download and view actions
callModule(collectDataButtons, "twitter", data = reactive({ tw_rv$tw_data }), file_prefix = "twitter")

callModule(collectNetworkButtons, "twitter", network = reactive({ tw_rv$tw_network }), file_prefix = "twitter")

if (v029) {
  callModule(collectGraphButtons_, "twitter", graph_data = reactive({ tw_rv$tw_graphml }), file_prefix = "twitter")
  
  twitter_view_rvalues <- callModule(collectViewGraphButtons, "twitter", graph_data = reactive({ tw_rv$tw_graphml }))  
} else {
  callModule(collectGraphButtons, "twitter", graph_data = reactive({ tw_rv$tw_graphml }), 
             graph_wt_data = reactive({ tw_rv$twitterWT_graphml }), file_prefix = "twitter")
  
  twitter_view_rvalues <- callModule(collectViewGraphButtons, "twitter", 
                                     graph_data = reactive({ tw_rv$tw_graphml }), 
                                     graph_wt_data = reactive({ tw_rv$twitterWT_graphml }))
}

observeEvent(twitter_view_rvalues$data, {
  setGraphView(data = isolate(twitter_view_rvalues$data), 
               desc = paste0("Twitter network for search term: ", twitter_search_term, sep = ""),
               type = "twitter",
               name = "",
               seed = sample(gbl_rng_range[1]:gbl_rng_range[2], 1))
  updateCheckboxInput(session, "expand_demo_data_check", value = FALSE)
}, ignoreInit = TRUE)

#### output ----------------------------------------------------------------------------------------------------------- #

output$twitter_collect_token_output <- renderText({
  if (!is.null(creds_rv$use_token)) {
    createTokenId(creds_rv$use_token)
  } else {
    "No token set"
  }
})

# render twitter collection arguments
output$twitter_arguments_output <- renderText({
  # dependencies
  input$twitter_app_name_input
  input$twitter_api_key_input
  input$twitter_api_secret_input
  input$twitter_access_token_input
  input$twitter_access_token_secret_input
  
  input$twitter_search_term_input
  input$twitter_search_type_select
  input$twitter_retweets_check
  input$twitter_retry_check
  input$twitter_tweet_count_input
  input$twitter_language_input
  input$twitter_date_until_input
  
  input$twitter_since_id_input
  input$twitter_max_id_input
  
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

observeEvent(input$select_all_twitter_dt_columns, {
  updateCheckboxGroupInput(session, "show_twitter_cols", label = NULL,
                           choices = isolate(tw_rv$data_cols),
                           selected = isolate(tw_rv$data_cols),
                           inline = TRUE)
})

observeEvent(input$clear_all_twitter_dt_columns, {
  updateCheckboxGroupInput(session, "show_twitter_cols", label = NULL,
                           choices = isolate(tw_rv$data_cols),
                           selected = character(0),
                           inline = TRUE)
})

observeEvent(input$reset_twitter_dt_columns, {
  updateCheckboxGroupInput(session, "show_twitter_cols", label = NULL,
                           choices = isolate(tw_rv$data_cols),
                           selected = c("user_id", "status_id", "created_at", "screen_name", "text",
                                        "is_retweet"),
                           inline = TRUE)
})

output$twitter_data_cols_ui <- renderUI({
  data <- tw_rv$data_cols
  
  if (is.null(data)) {
    return(NULL)
  }
  
  conditionalPanel(condition = 'input.expand_show_twitter_cols',
    div(actionButton("select_all_twitter_dt_columns", "Select all"), 
        actionButton("clear_all_twitter_dt_columns", "Clear all"),
        actionButton("reset_twitter_dt_columns", "Reset")),
    checkboxGroupInput("show_twitter_cols", label = NULL,
                       choices = tw_rv$data_cols,
                       selected = c("user_id", "status_id", "created_at", "screen_name", "text",
                                    "is_retweet"), 
                       inline = TRUE, width = '98%')
  )
})

#### reactives -------------------------------------------------------------------------------------------------------- #

setTwitterAPIKeys <- reactive({
  tw_api_keyring$twitter_app_name <<- input$twitter_app_name_input
  tw_api_keyring$twitter_api_key <<- trimws(input$twitter_api_key_input)
  tw_api_keyring$twitter_api_secret <<- trimws(input$twitter_api_secret_input)
  tw_api_keyring$twitter_access_token <<- trimws(input$twitter_access_token_input)
  tw_api_keyring$twitter_access_token_secret <<- trimws(input$twitter_access_token_secret_input)
})

setTwitterParams <- reactive({
  twitter_search_term <<- trimws(input$twitter_search_term_input)
  twitter_search_term <<- gsub("\n{1, }", " ", twitter_search_term)
  
  twitter_search_type <<- input$twitter_search_type_select
  twitter_retweets <<- input$twitter_retweets_check
  twitter_retry <<- input$twitter_retry_check
  twitter_tweet_count <<- input$twitter_tweet_count_input
  twitter_language <<- input$twitter_language_input
  
  twitter_date_until <<- trimws(input$twitter_date_until_input)
  
  twitter_since_id <<- trimws(input$twitter_since_id_input)
  twitter_max_id <<- trimws(input$twitter_max_id_input)
  
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
  data <- tw_rv$tw_data
  
  if (is.null(data)) {
    return(NULL)
  }
  
  if (!is.null(input$show_twitter_cols)) {
    # data <- tw_rv$tw_data[, input$show_twitter_cols, drop = FALSE]
    if (length(input$show_twitter_cols) > 0) {
      data <- dplyr::select(tw_rv$tw_data, input$show_twitter_cols)
    } else {
      return(NULL)
    }
  } else {
    return(NULL)
  }
  
  if (nrow(data) < 1) {
    return(NULL)
  }
  
  col_classes <- sapply(data, class)
  for (i in seq(1, length(col_classes))) {
    if ("list" %in% col_classes[i]) {
      var <- names(col_classes)[i]
      data[var] <- lapply(data[var], function(x) sapply(x, paste, collapse = ", ", character(1L)))
    }
  }
  
  if (!is.null(tw_rv$tw_data)) {
    col_defs <- NULL
    if (input$dt_twitter_truncate_text_check == TRUE) {
      col_defs <- gbl_dt_col_defs
      col_defs[[1]]$targets = "_all"
    }
    DT::datatable(data, extensions = 'Buttons', filter = "top",
                  options = list(lengthMenu = gbl_dt_menu_len, pageLength = gbl_dt_page_len, scrollX = TRUE,
                                 columnDefs = col_defs, dom = 'lBfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'print')), class = 'cell-border stripe compact hover')
  }
})

#### functions -------------------------------------------------------------------------------------------------------- #

# format twitter collection arguments output
twitterArgumentsOutput <- function() {
  
  output <- c()
  check_keys <- sapply(tw_api_keyring, VOSONDash::isNullOrEmpty)
  
  search_term_flag <- FALSE
  if (!VOSONDash::isNullOrEmpty(twitter_search_term)) {
    temp_search_term <- twitter_search_term
    output <- append(output, paste0("search term: ", trimws(temp_search_term)))
    search_term_flag <- TRUE
  }
  
  output <- append(output, paste0("include retweets: ", ifelse(twitter_retweets, "yes", "no")))
  output <- append(output, paste0("retry on rate limit: ", ifelse(twitter_retry, "yes", "no")))
  
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
  
  opts <- list("filter:safe" = !twitter_filter_safe,
               "filter:media" = !twitter_filter_media,
               "filter:links" = !twitter_filter_url,
               ":)" = !twitter_filter_positive,
               ":(" = !twitter_filter_negative)
  
  sapply(names(opts), function(x) {
    if (!VOSONDash::isNullOrEmpty(opts[x])) {
      if (opts[x] == FALSE) { twitter_search_options <<- append(twitter_search_options, x) }
    }
  })
  
  if (!VOSONDash::isNullOrEmpty(twitter_tweet_count) && is.numeric(twitter_tweet_count)) {
    output <- append(output, paste0("number of tweets: ", twitter_tweet_count))
  }
  
  if (!VOSONDash::isNullOrEmpty(twitter_language)) {
    output <- append(output, paste0("language: ", twitter_language))
  }
  
  if (!VOSONDash::isNullOrEmpty(twitter_date_until)) {
    output <- append(output, paste0("until date: ", twitter_date_until, sep = ""))
  }
  
  if (!VOSONDash::isNullOrEmpty(twitter_since_id)) {
    output <- append(output, paste0("since ID: ", twitter_since_id, sep = ""))
  }
  
  if (!VOSONDash::isNullOrEmpty(twitter_max_id)) {
    output <- append(output, paste0("max ID: ", twitter_max_id, sep = ""))
  }
  
  if (length(twitter_search_options) > 0) {
    output <- append(output, paste0("filters: ", paste0(twitter_search_options, collapse = " ")))
  }
  
  # if api key and video ids have been inputed enable collect button
  if (!is.null(creds_rv$use_token) && search_term_flag) {
    shinyjs::enable("twitter_collect_button")
  } else {
    shinyjs::disable("twitter_collect_button")
  }
  
  paste0(output, collapse = '\n')
}
