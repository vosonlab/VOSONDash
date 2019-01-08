#' VOSON Dashboard youtubeServer
#'
#' Collects Youtube video comments and creates an actor network using the vosonSML package.
#'

#### values ----------------------------------------------------------------------------------------------------------- #

youtube_rvalues <- reactiveValues()
youtube_rvalues$youtube_data <- NULL      # dataframe returned by vosonSML collection
youtube_rvalues$youtube_graphml <- NULL   # graphml object returned from collection
youtube_rvalues$youtubeWT_graphml <- NULL

youtube_api_key <- NULL        # youtube api key
youtube_video_id_list <- c()   # list of youtube video ids to collect on

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

# youtube collection button pushed
observeEvent(input$youtube_collect_button, {
  
  # disable button so it is not pushed again
  shinyjs::disable("youtube_collect_button")
  
  withProgress(message = 'Collecting comments', value = 0.5, {
    
    withConsoleRedirect("youtube_console", {
      #withCallingHandlers({
      shinyjs::html(id = "youtube_console", html = "")
      
      youtube_video_id_list <- sapply(youtube_video_id_list, 
                                      function(x) gsub("^v=", "", x, ignore.case = TRUE, perl = TRUE))
      
      # collect youtube data and print any output to console
      tryCatch({
        youtube_rvalues$youtube_data <<- collectYoutubeData(youtube_api_key, youtube_video_id_list)
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

# enable youtube download data button when there is youtube data
observeEvent(youtube_rvalues$youtube_data, {
  if (!is.null(youtube_rvalues$youtube_data) && nrow(youtube_rvalues$youtube_data) > 0) {
    shinyjs::enable("download_youtube_data_button")
  } else {
    shinyjs::disable("download_youtube_data_button")
  }
})

# enable youtube download graphml button when there is youtube graphml data
observeEvent(youtube_rvalues$youtube_graphml, {
  if (!is.null(youtube_rvalues$youtube_graphml)) {
    shinyjs::enable("download_youtube_graph_button")
    shinyjs::enable("download_youtube_graphWT_button")
    shinyjs::enable("view_youtube_graph_button")
    shinyjs::enable("view_youtube_graphWT_button")
  } else {
    shinyjs::disable("download_youtube_graph_button")
    shinyjs::disable("download_youtube_graphWT_button")
    shinyjs::disable("view_youtube_graph_button")
    shinyjs::disable("view_youtube_graphWT_button")
  }
})

observeEvent(input$view_youtube_graph_button, {
  
  if (!is.null(isolate(youtube_rvalues$youtube_graphml))) {
    # clear graph file data
    shinyjs::reset("graphml_data_file")
    
    # set graph data
    ng_rvalues$graph_data <<- isolate(youtube_rvalues$youtube_graphml)
    
    video_id_list_text <- paste0(youtube_video_id_list, collapse = ', ')
    
    ng_rvalues$graph_desc <<- paste0("Youtube actor network for videos: ", video_id_list_text, sep = "")
    ng_rvalues$graph_type <<- "youtube"
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

observeEvent(input$view_youtube_graphWT_button, {
  
  if (!is.null(isolate(youtube_rvalues$youtubeWT_graphml))) {
    # clear graph file data
    shinyjs::reset("graphml_data_file")
    
    # set graph data
    ng_rvalues$graph_data <<- isolate(youtube_rvalues$youtubeWT_graphml)
    
    video_id_list_text <- paste0(youtube_video_id_list, collapse = ', ')
    
    ng_rvalues$graph_desc <<- paste0("Youtube actor network for videos: ", video_id_list_text, sep = "")
    ng_rvalues$graph_type <<- "youtube"
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
#### output ----------------------------------------------------------------------------------------------------------- #

# render youtube collection arguments
output$youtube_arguments_output <- renderText({
  input$youtube_api_key_input
  input$youtube_add_video_id_button
  input$youtube_remove_video_id_button
  
  # do not update arguments text on input field or list changes
  isolate({input$youtube_video_id_input
    input$youtube_video_id_list_output})
  
  # get youtube collection arguments output
  youtubeArgumentsOutput()
})

# render youtube data table
output$dt_youtube_data <- DT::renderDataTable({
  datatableYoutubeData()
})

# set file name and content for youtube data download
output$download_youtube_data_button <- downloadHandler(
  filename = function() {
    systemTimeFilename("youtube-data", "csv")
  },
  
  content = function(file) {
    write.csv(youtube_rvalues$youtube_data, file)
  }
)

# set file name and content for youtube graphml download
output$download_youtube_graph_button <- downloadHandler(
  filename = function() {
    systemTimeFilename("youtube", "graphml")
  },
  
  content = function(file) {
    write_graph(youtube_rvalues$youtube_graphml, file, format=c("graphml"))
  }
)

# set file name and content for youtube graphml (with text) download
output$download_youtube_graphWT_button <- downloadHandler(
  filename = function() {
    systemTimeFilename("youtube-with-text", "graphml")
  },
  
  content = function(file) {
    write_graph(isolate(youtube_rvalues$youtubeWT_graphml), file, format=c("graphml"))
  }
)

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
  
  video_id <- paste0("v=", video_id)
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

# create data table from collected youtube data
datatableYoutubeData <- reactive({
  if (!is.null(youtube_rvalues$youtube_data)) {
    col_defs <- NULL
    if (input$dt_youtube_truncate_text_check == TRUE) {
      col_defs <- g_dt_col_defs
      col_defs[[1]]$targets <- c(1)
    }
    DT::datatable(youtube_rvalues$youtube_data, extensions = 'Buttons', 
                  options = list(lengthMenu = g_dt_length_menu, pageLength = g_dt_page_length, scrollX = TRUE,
                                 columnDefs = col_defs, dom = 'lBfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'print')), class = 'cell-border stripe compact hover')
  }
})

#### functions -------------------------------------------------------------------------------------------------------- #

# format youtube collection arguments output
youtubeArgumentsOutput <- function() {
  command_str <- ""
  command_str_2 <- ""
  
  collect_state <- 0
  
  if (!is.null(youtube_api_key) && nchar(youtube_api_key) > 1) {
    # command_str <- paste0("api key: ", youtube_api_key, "\n")
    command_str <- trimws(paste0("api key: ", strtrim(youtube_api_key, 6), "...", sep = ""))
    command_str <- paste0(command_str, "\n")
    collect_state <- 1
  }
  
  if (!is.null(youtube_video_id_list) && length(youtube_video_id_list) > 0) {
    command_str_2 <- paste0("videos: ", trimws(paste0(youtube_video_id_list, collapse = ', ')))
    
    if (collect_state == 1) {
      collect_state <- 2
    }
  }
  
  # if api key and video ids have been inputed enable collect button
  if (collect_state == 2) {
    shinyjs::enable("youtube_collect_button")
  } else {
    shinyjs::disable("youtube_collect_button")
  }
  
  paste0(command_str, command_str_2, sep='\n')
}
