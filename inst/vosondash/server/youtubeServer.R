#' VOSON Dashboard youtubeServer
#'
#' Collects Youtube video comments and creates an actor network using the vosonSML package.
#'

#### values ---------------------------------------------------------------------------------------------------------- #

yt_rv <- reactiveValues(
  yt_data = NULL,
  yt_network = NULL,
  yt_graphml = NULL,
  yt_wt_graphml = NULL,
  
  data_cols = NULL
)

youtube_api_key <- NULL        # youtube api key
youtube_video_id_list <- c()   # list of youtube video ids to collect on
youtube_max_comments <- 200

#### events ---------------------------------------------------------------------------------------------------------- #

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
      updateNumericInput(session, "youtube_max_comments_input", value = gbl_def_youtube_count)
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
      youtube_video_id_list <- sapply(youtube_video_id_list, 
                                      function(x) gsub("^v=", "", x, ignore.case = TRUE, perl = TRUE))
      
      # collect youtube data and print any output to console
      tryCatch({
        yt_rv$yt_data <- collectYoutubeData(youtube_api_key, youtube_video_id_list, youtube_max_comments)
        
        yt_rv$data_cols <- names(yt_rv$yt_data)
      }, error = function(err) {
        incProgress(1, detail = "Error")
        cat(paste('youtube collection error:', err))
        return(NULL)
      })
      
      incProgress(1, detail = "Finished")
      updateTabItems(session, "youtube_control_tabset", selected = "Create Network")
      
    }) # withConsoleRedirect
    
  }) # withProgress
  
  # enable button
  youtubeArgumentsOutput()
  
  delay(gbl_scroll_delay, js$scroll_console("youtube_console"))
})

observeEvent(yt_rv$yt_data, {
  if (!is.null(yt_rv$yt_data) && nrow(yt_rv$yt_data)) {
    shinyjs::enable("youtube_create_button")
  } else {
    shinyjs::disable("youtube_create_button")
  }
})

observeEvent(input$youtube_create_button, {
  net_type <- input$youtube_network_type_select
  add_text <- input$youtube_network_text
  network <- NULL
  
  shinyjs::disable("youtube_create_button")
  
  withProgress(message = 'Creating network', value = 0.5, {
    
    withConsoleRedirect("youtube_console", {
      if (net_type == "activity") {
        network <- vosonSML::Create(isolate(yt_rv$yt_data), "activity", verbose = TRUE)
        if (add_text) { 
          network <- vosonSML::AddText(network, isolate(yt_rv$yt_data), verbose = TRUE)
        }
      } else if (net_type == "actor") {
        network <- vosonSML::Create(isolate(yt_rv$yt_data), "actor", verbose = TRUE)
        if (add_text) {
              # vosonSML changed param name
              network <- vosonSML::AddText(network, isolate(yt_rv$yt_data), 
                                           repliesFromText = input$youtube_network_replies_from_text, verbose = TRUE)

        }
        if (input$youtube_network_video_data) { 
          creds <- vosonSML::Authenticate("youtube", apiKey = youtube_api_key)
          network <- vosonSML::AddVideoData(network, youtubeAuth = creds,
                                            actorSubOnly = input$youtube_network_video_subs, verbose = TRUE)
        }
      }
      if (!is.null(network)) { 
        yt_rv$yt_network <- network
        yt_rv$yt_graphml <- vosonSML::Graph(network)
      }
    }) # withConsoleRedirect
      
    incProgress(1, detail = "Finished")
  }) # withProgress
  
  shinyjs::enable("youtube_create_button")
  
  delay(gbl_scroll_delay, js$scroll_console("youtube_console"))
})

# download and view actions
callModule(collectDataButtons, "youtube", data = reactive({ yt_rv$yt_data }), file_prefix = "youtube")

callModule(collectNetworkButtons, "youtube", network = reactive({ yt_rv$yt_network }), file_prefix = "youtube")

callModule(collectGraphButtons_, "youtube", graph_data = reactive({ yt_rv$yt_graphml }), file_prefix = "youtube")

youtube_view_rvalues <- callModule(collectViewGraphButtons, "youtube", graph_data = reactive({ yt_rv$yt_graphml }))  

observeEvent(youtube_view_rvalues$data, {
  setGraphView(data = isolate(youtube_view_rvalues$data), 
               desc = paste0("Youtube network for videos: ", paste0(youtube_video_id_list, collapse = ', '), 
                             sep = ""),
               type = "youtube",
               name = "",
               seed = sample(gbl_rng_range[1]:gbl_rng_range[2], 1))
  updateCheckboxInput(session, "expand_demo_data_check", value = FALSE)
}, ignoreInit = TRUE)

observeEvent(input$clear_youtube_console, {
  resetConsole("youtube_console")
})

#### output ---------------------------------------------------------------------------------------------------------- #

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
                           choices = isolate(yt_rv$data_cols),
                           selected = isolate(yt_rv$data_cols),
                           inline = TRUE)
})

observeEvent(input$clear_all_youtube_dt_columns, {
  updateCheckboxGroupInput(session, "show_youtube_cols", label = NULL,
                           choices = isolate(yt_rv$data_cols),
                           selected = character(0),
                           inline = TRUE)
})

dt_yt_cols <- function() {
  return(c("Comment", "AuthorDisplayName", "VideoID", "PublishedAt"))
}

observeEvent(input$reset_youtube_dt_columns, {
  updateCheckboxGroupInput(session, "show_youtube_cols", label = NULL,
                           choices = isolate(yt_rv$data_cols),
                           selected = dt_yt_cols(),
                           inline = TRUE)
})

output$youtube_data_cols_ui <- renderUI({
  data <- yt_rv$data_cols
  
  if (is.null(data)) { return(NULL) }
  
  conditionalPanel(condition = 'input.expand_show_youtube_cols',
                   div(actionButton("select_all_youtube_dt_columns", "Select all"), 
                       actionButton("clear_all_youtube_dt_columns", "Clear all"),
                       actionButton("reset_youtube_dt_columns", "Reset")),
                   checkboxGroupInput("show_youtube_cols", label = NULL,
                                      choices = yt_rv$data_cols,
                                      selected = dt_yt_cols(),
                                      inline = TRUE, width = '98%'))
})

#### reactives ------------------------------------------------------------------------------------------------------- #

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
  if (is.null(video_id)) { return(NULL) }
  
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
  data <- yt_rv$yt_data
  
  if (is.null(data)) { return(NULL) }
  
  cls_lst <- class(data)
  class(data) <- cls_lst[!cls_lst %in% c("datasource", "youtube")]
  
  if (!is.null(input$show_youtube_cols)) {
    if (length(input$show_youtube_cols) > 0) {
      # data <- dplyr::select(yt_rv$yt_data, input$show_youtube_cols)
      data <- dplyr::select(data, input$show_youtube_cols)
    } else { return(NULL) }
  } else { return(NULL) }
  
  if (nrow(data) < 1) { return(NULL) }

  col_classes <- sapply(data, class)
  for (i in seq(1, length(col_classes))) {
    if ("list" %in% col_classes[i]) {
      var <- names(col_classes)[i]
      data[var] <- lapply(data[var], function(x) sapply(x, paste, collapse = ", ", character(1L)))
    }
  }
  
  if (!is.null(yt_rv$yt_data)) {
    col_defs <- NULL
    if (input$dt_youtube_truncate_text_check == TRUE) {
      col_defs <- gbl_dt_col_defs
      col_defs[[1]]$targets = "_all"
    }
    DT::datatable(data, extensions = 'Buttons', filter = "top",
                  options = list(lengthMenu = gbl_dt_menu_len, pageLength = gbl_dt_page_len, scrollX = TRUE,
                                 columnDefs = col_defs, dom = 'lBfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'print')),
                  class = 'cell-border stripe compact hover')
  }
})

#### functions ------------------------------------------------------------------------------------------------------- #

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
  
  if (!VOSONDash::isNullOrEmpty(youtube_max_comments) && is.numeric(youtube_max_comments)) {
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
