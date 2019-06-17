#' VOSON Dashboard Collection Module
#'
#' Shiny module functions for collection sections.
#'

#### ui ---------------------------------------------------------------------------------------------------------------

# consoleUI <- function(id) {
#   ns <- NS(id)
#   
#   tagList(
#     verbatimTextOutput(ns("args_output")),
#     pre(id = ns("console"), style = "height: 300px; overflow-y: scroll")
#   )
# }

collectDataButtonsUI <- function(id) {
  ns <- NS(id)
  
  tagList(shinyjs::disabled(downloadButton(ns("dl_data"), label = "Data", title = "Download Raw Data File")))
}

collectGraphButtonsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::disabled(downloadButton(ns("dl_graph"), label = "Graphml", title = "Download Network Graphml File")),
    shinyjs::disabled(downloadButton(ns("dl_graph_wt"), label = "Graphml (+text)", 
                                     title = "Download Network Graphml File with Text"))
  )
}

collectViewGraphButtonsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::disabled(actionButton(ns("view_graph"), label = "Graph", title = "View Network Graph", icon("eye"))),
    shinyjs::disabled(actionButton(ns("view_graph_wt"), label = "Graph (+text)", 
                      title = "View Network Graph with Text", icon("eye")))
  )
}

#### server -----------------------------------------------------------------------------------------------------------

# withConsoleRedirectMod <- function(input, output, session, value) {
#   input_text <- capture.output(results <- value, type = "output")
#   
#   id <- session$ns("console")
#          
#   if (length(input_text) > 0) {
#     output <- gsub("\n{2,}", "\n", input_text)
#     insertUI(paste0("#", id), where = "beforeEnd",
#              ui = div(id = paste0("_", id), paste0(output, "\n", collapse = ""))
#     )
#   }
#   return(results)
# }
# 
# resetConsoleMod <- function(input, output, session, remove_ui = TRUE) {
#   id <- session$ns("console")
#   
#   if (remove_ui) {
#     removeUI(selector = paste0("div#_", id), multiple = TRUE)  
#   }
#   vosonsml_version <- getVosonSMLVersion()
#   if (!is.null(vosonsml_version)) {
#     vosonsml_version <- paste0("vosonSML v", vosonsml_version)  
#   } else {
#     vosonsml_version <- "vosonSML unknown"
#   }  
#   reset_message <- paste0(vosonsml_version, " - ", Sys.time(), "\n")
#   
#   insertUI(paste0("#", id), where = "beforeEnd",
#            ui = div(id = paste0("_", id), paste0(reset_message, "\n", collapse = ""))
#   )
# }

withConsoleRedirect <- function(id, value) {
  input_text <- capture.output(results <- value, type = "output")

  if (length(input_text) > 0) {
    output <- gsub("\n{2,}", "\n", input_text)
    insertUI(paste0("#", id), where = "beforeEnd",
             ui = div(id = paste0("_", id), paste0(output, "\n", collapse = ""))
    )
  }
  return(results)
}

addToConsole <- function(id, value) {
  insertUI(paste0("#", id), where = "beforeEnd",
           ui = div(id = paste0("_", id), paste0(value, "\n", collapse = ""))
  )
}

resetConsole <- function(id, remove_ui = TRUE) {
  if (remove_ui) {
    removeUI(selector = paste0("div#_", id), multiple = TRUE)
  }
  vosonsml_version <- getVosonSMLVersion()
  if (!is.null(vosonsml_version)) {
    vosonsml_version <- paste0("vosonSML v", vosonsml_version)
  } else {
    vosonsml_version <- "vosonSML unknown"
  }
  reset_message <- paste0(vosonsml_version, " - ", Sys.time(), "\n")
  addToConsole(id, reset_message)
}

collectDataButtons <- function(input, output, session, data, file_prefix = "") {
  output$dl_data <- downloadHandler(
    filename = function() {
      systemTimeFilename(paste0(ifelse(file_prefix == "", "", paste0(file_prefix, "-")), "data"), "rds")
    },

    content = function(file) {
      saveRDS(collectData(), file)
    }
  )
  
  collectData <- reactive({
    data()
  })
  
  observeEvent(data(), {
    if (!is.null(data()) && nrow(data()) > 0) {
      shinyjs::enable("dl_data")  
    } else {
      shinyjs::disable("dl_data")
    }
  })  
}

collectGraphButtons <- function(input, output, session, graph_data, graph_wt_data, file_prefix = "") {
  output$dl_graph <- downloadHandler(
    filename = function() {
      systemTimeFilename(ifelse(file_prefix == "", "graph", file_prefix), "graphml")
    },
    
    content = function(file) {
      write_graph(collectGraphData(), file, format = c("graphml"))
    }
  )
  
  output$dl_graph_wt <- downloadHandler(
    filename = function() {
      systemTimeFilename(ifelse(file_prefix == "", "graph-wt", paste0(file_prefix, "-wt")), "graphml")
    },
    
    content = function(file) {
      write_graph(collectGraphWTData(), file, format = c("graphml"))
    }
  )
  
  collectGraphData <- reactive({
    g <- graph_data()
  })
  
  collectGraphWTData <- reactive({
    g <- graph_wt_data()
  })
  
  observeEvent(graph_data(), {
    if (!is.null(graph_data())) {
      shinyjs::enable("dl_graph")
      shinyjs::enable("view_graph")
    } else {
      shinyjs::disable("dl_graph")
      shinyjs::disable("view_graph")
    }
  })
  
  observeEvent(graph_wt_data(), {
    if (!is.null(graph_wt_data())) {
      shinyjs::enable("dl_graph_wt")
      shinyjs::enable("view_graph_wt")
    } else {
      shinyjs::disable("dl_graph_wt")
      shinyjs::disable("view_graph_wt")
    }
  })  
}

collectViewGraphButtons <- function(input, output, session, graph_data, graph_wt_data) {
  view_rvalues <- reactiveValues(data = NULL)
  
  observeEvent(input$view_graph, {
    view_rvalues$data <<- graph_data()
  })
  
  observeEvent(input$view_graph_wt, {
    view_rvalues$data <<- graph_wt_data()
  })
  
  return(view_rvalues)
}
