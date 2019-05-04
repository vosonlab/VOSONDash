#' VOSON Dashboard youtubeServer
#'
#' Collects Youtube video comments and creates an actor network using the vosonSML package.
#'

#### values ----------------------------------------------------------------------------------------------------------- #

youtube_rvalues <- reactiveValues()
youtube_rvalues$youtube_data <- NULL      # dataframe returned by vosonSML collection
youtube_rvalues$youtube_graphml <- NULL   # graphml object returned from collection
youtube_rvalues$youtubeWT_graphml <- NULL

youtube_rvalues$data_cols <- NULL

youtube_api_key <- NULL        # youtube api key
youtube_video_id_list <- c()   # list of youtube video ids to collect on
youtube_max_comments <- 200

#### events ----------------------------------------------------------------------------------------------------------- #

# update youtube api key when field input changes
observeEvent(input$youtube_api_key_input, {
  setYoutubeAPIKey()
})

# when the youtube add video id button is pushed update the list and input fields
observeEvent(input$youtube_add_video_id_button, {
  updateTextInput(session, "youtube_video_id_input", value = "")
  
  videoListAdd()
  
  updateSelectInput(session, "youtube_video_id_list_output",
                    choices = youtube_video_id_list)
})

# when the youtube remove video id button is pushed update the list and input fields
observeEvent(input$youtube_remove_video_id_button, {
  videoListRemove()
  
  updateSelectInput(session, "youtube_video_id_list_output",
                    choices = youtube_video_id_list)
})

# set count parameter and reset if not numeric or less than one
observeEvent(input$youtube_max_comments_input, {
  if (!is.na(input$youtube_max_comments_input)) {
    if (!is.numeric(input$youtube_max_comments_input) ||input$youtube_max_comments_input < 1) {
      updateNumericInput(session, "youtube_max_comments_input", value = g_default_youtube_count)
    }
  }
  youtube_max_comments <<- input$youtube_max_comments_input
})

# youtube collection button pushed
observeEvent(input$youtube_collect_button, {
  
  # disable button so it is not pushed again
  shinyjs::disable("youtube_collect_button")
  
  withProgress(message = 'Collecting comments', value = 0.5, {
    
    withConsoleRedirect("youtube_console", {
      # withCallingHandlers({
      # shinyjs::html(id = "youtube_console", html = "")
      
      youtube_video_id_list <- sapply(youtube_video_id_list, 
                                      function(x) gsub("^v=", "", x, ignore.case = TRUE, perl = TRUE))
      
      # collect youtube data and print any output to console
      tryCatch({
        youtube_rvalues$youtube_data <<- collectYoutubeData(youtube_api_key, youtube_video_id_list, 
                                                            youtube_max_comments)
        
        youtube_rvalues$data_cols <<- names(youtube_rvalues$youtube_data)
      }, error = function(err) {
        incProgress(1, detail = "Error")
        cat(paste('youtube collection error:', err))
        return(NULL)
      })
      
      incProgress(0.5, detail = "Creating network")
      
      # if youtube data collected create graphml object
      if (!is.null(youtube_rvalues$youtube_data)) {
        tryCatch({
          # youtube_rvalues$youtube_graphml <<- createYoutubeNetwork(youtube_rvalues$youtube_data)
          netList <- createYoutubeNetwork(youtube_rvalues$youtube_data)
          youtube_rvalues$youtube_graphml <<- netList$network
          youtube_rvalues$youtubeWT_graphml <<- netList$networkWT   # "with text" (edge attribute)          
        }, error = function(err) {
          incProgress(1, detail = "Error")
          cat(paste('youtube graphml error:', err))
          return(NULL)
        })
      }
      
      incProgress(1, detail = "Finished")
      
    }) # withConsoleRedirect
    
  }) # withProgress
  
  # enable button
  youtubeArgumentsOutput()
})

# download and view actions
callModule(collectDataButtons, "youtube", data = reactive({ youtube_rvalues$youtube_data }), file_prefix = "youtube")

callModule(collectGraphButtons, "youtube", graph_data = reactive({ youtube_rvalues$youtube_graphml }), 
           graph_wt_data = reactive({ youtube_rvalues$youtubeWT_graphml }), file_prefix = "youtube")

youtube_view_rvalues <- callModule(collectViewGraphButtons, "youtube", 
                                   graph_data = reactive({ youtube_rvalues$youtube_graphml }), 
                                   graph_wt_data = reactive({ youtube_rvalues$youtubeWT_graphml }))

observeEvent(youtube_view_rvalues$data, {
  setGraphView(data = isolate(youtube_view_rvalues$data), 
               desc = paste0("Youtube actor network for videos: ", paste0(youtube_video_id_list, collapse = ', '), 
                             sep = ""),
               type = "twitter",
               name = "",
               seed = sample(g_random_number_range[1]:g_random_number_range[2], 1))
}, ignoreInit = TRUE)

observeEvent(input$clear_youtube_console, {
  resetConsole("youtube_console")
})

#### output ----------------------------------------------------------------------------------------------------------- #

# render youtube collection arguments
output$youtube_arguments_output <- renderText({
  input$youtube_api_key_input
  input$youtube_add_video_id_button
  input$youtube_remove_video_id_button
  input$youtube_max_comments_input
  
  # do not update arguments text on input field or list changes
  isolate({ input$youtube_video_id_input
            input$youtube_video_id_list_output })
  
  # get youtube collection arguments output
  youtubeArgumentsOutput()
})

# render youtube data table
output$dt_youtube_data <- DT::renderDataTable({
  datatableYoutubeData()
})

observeEvent(input$select_all_youtube_dt_columns, {
  updateCheckboxGroupInput(session, "show_youtube_cols", label = NULL,
                           choices = isolate(youtube_rvalues$data_cols),
                           selected = isolate(youtube_rvalues$data_cols),
                           inline = TRUE)
})

observeEvent(input$clear_all_youtube_dt_columns, {
  updateCheckboxGroupInput(session, "show_youtube_cols", label = NULL,
                           choices = isolate(youtube_rvalues$data_cols),
                           selected = character(0),
                           inline = TRUE)
})

observeEvent(input$reset_youtube_dt_columns, {
  updateCheckboxGroupInput(session, "show_youtube_cols", label = NULL,
                           choices = isolate(youtube_rvalues$data_cols),
                           selected = c("Comment", "User", "PublishTime"),
                           inline = TRUE)
})

output$youtube_data_cols_ui <- renderUI({
  data <- youtube_rvalues$data_cols
  
  if (is.null(data)) {
    return(NULL)
  }
  
  conditionalPanel(condition = 'input.expand_show_youtube_cols',
                   div(actionButton("select_all_youtube_dt_columns", "Select all"), 
                       actionButton("clear_all_youtube_dt_columns", "Clear all"),
                       actionButton("reset_youtube_dt_columns", "Reset")),
                   checkboxGroupInput("show_youtube_cols", label = NULL,
                                      choices = youtube_rvalues$data_cols,
                                      selected = c("Comment", "User", "PublishTime"),
                                      inline = TRUE, width = '98%')
  )
})

#### reactives -------------------------------------------------------------------------------------------------------- #

setYoutubeAPIKey <- reactive({
  youtube_api_key <<- trimws(input$youtube_api_key_input)
  
  return(youtube_api_key)
})

# add to the list of youtube video ids to collect on
videoListAdd <- reactive({
  if (is.null(input$youtube_video_id_input) || trimws(input$youtube_video_id_input) == "") {
    return(NULL)
  }
  
  video_id <- getYoutubeVideoId(input$youtube_video_id_input)
  if (is.null(video_id)) {
    return(NULL)
  }
  
  # video_id <- paste0("id:", video_id)
  # only add if not already in list
  if (!(trimws(video_id) %in% youtube_video_id_list)) {
    youtube_video_id_list <<- append(youtube_video_id_list, trimws(video_id))
  }
  
  return(youtube_video_id_list)
})

# remove from the list of youtube video ids to collect on
videoListRemove <- reactive({
  if (is.null(input$youtube_video_id_list_output) || trimws(input$youtube_video_id_list_output) == "") {
    return(NULL)
  }
  
  youtube_video_id_list <<- youtube_video_id_list[!(youtube_video_id_list %in% input$youtube_video_id_list_output)]
  
  return(youtube_video_id_list)
})

datatableYoutubeData <- reactive({
  data <- youtube_rvalues$youtube_data
  
  if (is.null(data)) {
    return(NULL)
  }
  
  if (!is.null(input$show_youtube_cols)) {
    if (length(input$show_youtube_cols) > 0) {
      data <- dplyr::select(youtube_rvalues$youtube_data, input$show_youtube_cols)
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
  
  if (!is.null(youtube_rvalues$youtube_data)) {
    col_defs <- NULL
    if (input$dt_youtube_truncate_text_check == TRUE) {
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

# format youtube collection arguments output
youtubeArgumentsOutput <- function() {
  output <- c()
  key_flag <- video_id_flag <- count_flag <- FALSE
  
  if (!is.null(youtube_api_key) && nchar(youtube_api_key) > 1) {
    key_flag <- TRUE
    output <- append(output, trimws(paste0("api key: ", strtrim(youtube_api_key, 6), "...", sep = "")))
  }
  
  if (!is.null(youtube_video_id_list) && length(youtube_video_id_list) > 0) {
    video_id_flag <- TRUE
    output <- append(output, paste0("videos: ", trimws(paste0(youtube_video_id_list, collapse = ', '))))
  }
  
  if (!is.null(youtube_max_comments) && youtube_max_comments > 0) {
    count_flag <- TRUE
    output <- append(output, paste0("max comments: ", youtube_max_comments))
  }
  
  
  if (key_flag && video_id_flag && count_flag) {
    shinyjs::enable("youtube_collect_button")
  } else {
    shinyjs::disable("youtube_collect_button")
  }
  
  paste0(output, collapse = '\n')
}

