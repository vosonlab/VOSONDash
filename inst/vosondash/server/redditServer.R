#' VOSON Dashboard redditServer
#'
#' Collects Reddit thread comments and creates an actor network using the vosonSML package.
#'

#### values ----------------------------------------------------------------------------------------------------------- #

reddit_rvalues <- reactiveValues()
reddit_rvalues$reddit_data <- NULL      # dataframe returned by vosonSML collection
reddit_rvalues$reddit_graphml <- NULL   # graphml object returned from collection
reddit_rvalues$redditWT_graphml <- NULL

reddit_rvalues$data_cols <- NULL

reddit_url_list <- c()   # list of reddit threads to collect on

#### events ----------------------------------------------------------------------------------------------------------- #

# when the reddit add thread url button is pushed update the list and input fields
observeEvent(input$reddit_add_url_button, {
  updateTextInput(session, "reddit_url_input", value = "")
  
  urlListAdd()
  
  updateSelectInput(session, "reddit_url_list_output",
                    choices = reddit_url_list)
})

# when the reddit remove url button is pushed update the list and input fields
observeEvent(input$reddit_remove_url_button, {
  urlListRemove()
  
  updateSelectInput(session, "reddit_url_list_output",
                    choices = reddit_url_list)
})

# reddit collection button pushed
observeEvent(input$reddit_collect_button, {
  
  # disable button so it is not pushed again
  shinyjs::disable("reddit_collect_button")
  
  withProgress(message = 'Collecting threads', value = 0.5, {
    
    withConsoleRedirect("reddit_console", {
      # withCallingHandlers({
      # shinyjs::html(id = "reddit_console", html = "")
      
      url_list <- sapply(reddit_url_list, function(x) paste0("https://reddit.com/", x))
      
      # collect reddit data and print any output to console
      tryCatch({
        reddit_rvalues$reddit_data <<- collectRedditData(url_list)
        reddit_rvalues$data_cols <<- names(reddit_rvalues$reddit_data)
      }, error = function(err) {
        incProgress(1, detail = "Error")
        cat(paste('reddit collection error:', err))
        return(NULL)
      })
      
      incProgress(0.5, detail = "Creating network")
      
      # if reddit data collected create graphml object
      if (!is.null(reddit_rvalues$reddit_data)) {
        tryCatch({
          netList <- createRedditActorNetwork(reddit_rvalues$reddit_data)
          reddit_rvalues$reddit_graphml <<- netList$network
          reddit_rvalues$redditWT_graphml <<- netList$networkWT
        }, error = function(err) {
          incProgress(1, detail = "Error")
          cat(paste('reddit graphml error:', err))
          return(NULL)
        })
      }
      
      incProgress(1, detail = "Finished")
      
    }) # withConsoleRedirect
    
  }) # withProgress
  
  # enable button
  redditArgumentsOutput()
})

# download and view actions
callModule(collectDataButtons, "reddit", data = reactive({ reddit_rvalues$reddit_data }), file_prefix = "reddit")

callModule(collectGraphButtons, "reddit", graph_data = reactive({ reddit_rvalues$reddit_graphml }), 
           graph_wt_data = reactive({ reddit_rvalues$redditWT_graphml }), file_prefix = "reddit")

reddit_view_rvalues <- callModule(collectViewGraphButtons, "reddit", 
                                   graph_data = reactive({ reddit_rvalues$reddit_graphml }), 
                                   graph_wt_data = reactive({ reddit_rvalues$redditWT_graphml }))

observeEvent(reddit_view_rvalues$data, {
  setGraphView(data = isolate(reddit_view_rvalues$data), 
               desc = paste0("Reddit actor network for threads: ", paste0(reddit_url_list, collapse = ', '), sep = ""),
               type = "reddit",
               name = "",
               seed = sample(g_random_number_range[1]:g_random_number_range[2], 1))
}, ignoreInit = TRUE)

observeEvent(input$clear_reddit_console, {
  resetConsole("reddit_console")
})
#### output ----------------------------------------------------------------------------------------------------------- #

# render reddit collection arguments
output$reddit_arguments_output <- renderText({
  input$reddit_add_url_button
  input$reddit_remove_url_button
  
  # do not update arguments text on input field or list changes
  isolate({input$reddit_url_input
    input$reddit_url_list_output})
  
  # get reddit collection arguments output
  redditArgumentsOutput()
})

# render reddit data table
output$dt_reddit_data <- DT::renderDataTable({
  datatableRedditData()
})

observeEvent(input$select_all_reddit_dt_columns, {
  updateCheckboxGroupInput(session, "show_reddit_cols", label = NULL,
                           choices = isolate(reddit_rvalues$data_cols),
                           selected = isolate(reddit_rvalues$data_cols),
                           inline = TRUE)
})

observeEvent(input$clear_all_reddit_dt_columns, {
  updateCheckboxGroupInput(session, "show_reddit_cols", label = NULL,
                           choices = isolate(reddit_rvalues$data_cols),
                           selected = character(0),
                           inline = TRUE)
})

observeEvent(input$reset_reddit_dt_columns, {
  updateCheckboxGroupInput(session, "show_reddit_cols", label = NULL,
                           choices = isolate(reddit_rvalues$data_cols),
                           selected = c("structure", "comm_date", "subreddit", "user", "comment_score", 
                                        "comment", "thread_id"),
                           inline = TRUE)
})

output$reddit_data_cols_ui <- renderUI({
  data <- reddit_rvalues$data_cols
  
  if (is.null(data)) {
    return(NULL)
  }
  
  conditionalPanel(condition = 'input.expand_show_reddit_cols',
                   div(actionButton("select_all_reddit_dt_columns", "Select all"), 
                       actionButton("clear_all_reddit_dt_columns", "Clear all"),
                       actionButton("reset_reddit_dt_columns", "Reset")),
                   checkboxGroupInput("show_reddit_cols", label = NULL,
                                      choices = reddit_rvalues$data_cols,
                                      selected = c("structure", "comm_date", "subreddit", "user", "comment_score", 
                                                   "comment", "thread_id"),
                                      inline = TRUE, width = '98%')
  )
})

#### reactives -------------------------------------------------------------------------------------------------------- #

# add to the list of reddit thread urls to collect on
urlListAdd <- reactive({
  url <- trimws(input$reddit_url_input)
  
  url <- createRedditRequestUrl(url)
  
  if (is.null(url) || url == "") {
    return(NULL)
  }
  
  # only add if not already in list
  if (!(url %in% reddit_url_list)) {
    reddit_url_list <<- append(reddit_url_list, url)
  }
  
  return(reddit_url_list)
})

# remove from the list of reddit thread urls to collect on
urlListRemove <- reactive({
  if (is.null(input$reddit_url_list_output) || trimws(input$reddit_url_list_output) == "") {
    return(NULL)
  }
  
  reddit_url_list <<- reddit_url_list[!(reddit_url_list %in% input$reddit_url_list_output)]
  
  return(reddit_url_list)
})

datatableRedditData <- reactive({
  data <- reddit_rvalues$reddit_data
  
  if (is.null(data)) {
    return(NULL)
  }
  
  if (!is.null(input$show_reddit_cols)) {
    if (length(input$show_reddit_cols) > 0) {
      data <- dplyr::select(reddit_rvalues$reddit_data, input$show_reddit_cols)
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
  
  if (!is.null(reddit_rvalues$reddit_data)) {
    col_defs <- NULL
    if (input$dt_reddit_truncate_text_check == TRUE) {
      col_defs <- g_dt_col_defs
      col_defs[[1]]$targets = "_all"
    }
    DT::datatable(data, extensions = 'Buttons', filter = "top",
                  options = list(lengthMenu = g_dt_length_menu, pageLength = g_dt_page_length, scrollX = TRUE,
                                 columnDefs = col_defs, dom = 'lBfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'print')), class = 'cell-border stripe compact hover')
  }
})

#### functions -------------------------------------------------------------------------------------------------------- #

# format reddit collection arguments output
redditArgumentsOutput <- function() {
  output <- c()
  
  thread_flag <- FALSE
  
  if (!is.null(reddit_url_list) && length(reddit_url_list) > 0) {
    thread_flag <- TRUE
    output <- append(output, paste0("threads: ", trimws(paste0(reddit_url_list, collapse = ", "))))
  }
  
  # if thread urls have been inputed enable collect button
  if (thread_flag) {
    shinyjs::enable("reddit_collect_button")
  } else {
    shinyjs::disable("reddit_collect_button")
  }
  
  paste0(output, collapse = '\n')
}