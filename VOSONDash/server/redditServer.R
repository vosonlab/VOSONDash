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

# enable reddit download data button when there is reddit data
observeEvent(reddit_rvalues$reddit_data, {
  if (!is.null(reddit_rvalues$reddit_data) && nrow(reddit_rvalues$reddit_data) > 0) {
    shinyjs::enable("download_reddit_data_button")
  } else {
    shinyjs::disable("download_reddit_data_button")
  }
})

# enable reddit download graphml button when there is reddit graphml data
observeEvent(reddit_rvalues$reddit_graphml, {
  if (!is.null(reddit_rvalues$reddit_graphml)) {
    shinyjs::enable("download_reddit_graph_button")
    shinyjs::enable("download_reddit_graphWT_button")
    shinyjs::enable("view_reddit_graph_button")
    shinyjs::enable("view_reddit_graphWT_button")
  } else {
    shinyjs::disable("download_reddit_graph_button")
    shinyjs::disable("download_reddit_graphWT_button")
    shinyjs::disable("view_reddit_graph_button")
    shinyjs::disable("view_reddit_graphWT_button")
  }
})

observeEvent(input$view_reddit_graph_button, {
  
  if (!is.null(isolate(reddit_rvalues$reddit_graphml))) {
    # clear graph file data
    shinyjs::reset("graphml_data_file")
    
    # set graph data
    ng_rvalues$graph_data <<- isolate(reddit_rvalues$reddit_graphml)
    
    url_list_text <- paste0(reddit_url_list, collapse = ', ')
    
    ng_rvalues$graph_desc <<- paste0("Reddit actor network for threads: ", url_list_text, sep = "")
    ng_rvalues$graph_type <<- "reddit"
    ng_rvalues$graph_name <<- ""                 # unset - only used when graph loaded from file
    
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

observeEvent(input$view_reddit_graphWT_button, {
  
  if (!is.null(isolate(reddit_rvalues$redditWT_graphml))) {
    # clear graph file data
    shinyjs::reset("graphml_data_file")
    
    # set graph data
    ng_rvalues$graph_data <<- isolate(reddit_rvalues$redditWT_graphml)
    
    url_list_text <- paste0(reddit_url_list, collapse = ', ')
    
    ng_rvalues$graph_desc <<- paste0("Reddit actor network for threads: ", url_list_text, sep = "")
    ng_rvalues$graph_type <<- "reddit"
    ng_rvalues$graph_name <<- ""                 # unset - only used when graph loaded from file
    
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

# set file name and content for reddit data download
output$download_reddit_data_button <- downloadHandler(
  filename = function() {
    systemTimeFilename("reddit-data", "csv")
  },
  
  content = function(file) {
    write.csv(reddit_rvalues$reddit_data, file)
  }
)

# set file name and content for reddit graphml download
output$download_reddit_graph_button <- downloadHandler(
  filename = function() {
    systemTimeFilename("reddit", "graphml")
  },
  
  content = function(file) {
    write_graph(reddit_rvalues$reddit_graphml, file, format=c("graphml"))
  }
)

# set file name and content for reddit graphml (with text) download
output$download_reddit_graphWT_button <- downloadHandler(
  filename = function() {
    systemTimeFilename("reddit-with-text", "graphml")
  },
  
  content = function(file) {
    write_graph(isolate(reddit_rvalues$redditWT_graphml), file, format=c("graphml"))
  }
)

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

# create data table from collected reddit data
# datatableRedditData <- reactive({
#   if (!is.null(reddit_rvalues$reddit_data)) {
#     col_defs <- NULL
#     if (input$dt_reddit_truncate_text_check == TRUE) {
#       col_defs <- g_dt_col_defs
#       # col_defs[[1]]$targets <- c(3, 4, 5, 6)
#       col_defs[[1]]$targets = "_all"
#     }
#     DT::datatable(reddit_rvalues$reddit_data, extensions = 'Buttons', 
#                   options = list(lengthMenu = g_dt_length_menu, pageLength = g_dt_page_length, scrollX = TRUE,
#                                  columnDefs = col_defs, dom = 'lBfrtip',
#                                  buttons = c('copy', 'csv', 'excel', 'print')), class = 'cell-border stripe compact hover')
#   }
# })

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
  command_str <- ""
  command_str_2 <- ""
  
  collect_state <- 1
  
  if (!is.null(reddit_url_list) && length(reddit_url_list) > 0) {
    command_str_2 <- paste0("threads: ", trimws(paste0(reddit_url_list, collapse = ", ")))
    
    if (collect_state == 1) {
      collect_state <- 2
    }
  }
  
  # if api key and video ids have been inputed enable collect button
  if (collect_state == 2) {
    shinyjs::enable("reddit_collect_button")
  } else {
    shinyjs::disable("reddit_collect_button")
  }
  
  paste0(command_str, command_str_2, sep='\n')
}