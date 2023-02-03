#' VOSON Dashboard Collection Module
#'
#' Shiny module functions for collection sections.
#'

#### ui ---------------------------------------------------------------------------------------------------------------

collectDataButtonsUI <- function(id) {
  ns <- NS(id)
  
  tagList(shinyjs::disabled(downloadButton(ns("dl_data"), label = "Data", title = "Download Raw Data File")))
}

collectNetworkButtonsUI <- function(id) {
  ns <- NS(id)
  
  tagList(shinyjs::disabled(downloadButton(ns("dl_network"), label = "Network", title = "Download Network Data File")))
}

collectGraphButtonsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::disabled(downloadButton(ns("dl_graph"), label = "Graphml", title = "Download Network Graphml File"))
  )
}

collectViewGraphButtonsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::disabled(actionButton(ns("view_graph"), label = "Graph", title = "View Network Graph", icon("eye")))
  )
}

#### server -----------------------------------------------------------------------------------------------------------

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
    if (!is.null(data())) { # && nrow(data()) > 0
      shinyjs::enable("dl_data")  
    } else {
      shinyjs::disable("dl_data")
    }
  })  
}

collectNetworkButtons <- function(input, output, session, network, file_prefix = "") {
  output$dl_network <- downloadHandler(
    filename = function() {
      systemTimeFilename(paste0(ifelse(file_prefix == "", "", paste0(file_prefix, "-")), "network"), "rds")
    },
    
    content = function(file) {
      saveRDS(collectNetwork(), file)
    }
  )
  
  collectNetwork <- reactive({
    network()
  })
  
  observeEvent(network(), {
    if (!is.null(network()) && nrow(network()$nodes) > 0) {
      shinyjs::enable("dl_network")  
    } else {
      shinyjs::disable("dl_network")
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

collectGraphButtons_ <- function(input, output, session, graph_data, file_prefix = "") {
  output$dl_graph <- downloadHandler(
    filename = function() {
      systemTimeFilename(ifelse(file_prefix == "", "graph", file_prefix), "graphml")
    },
    
    content = function(file) {
      write_graph(collectGraphData(), file, format = c("graphml"))
    }
  )
  
  collectGraphData <- reactive({
    g <- graph_data()
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

# collectViewGraphButtons_ <- function(input, output, session, graph_data) {
#   view_rvalues <- reactiveValues(data = NULL)
#   
#   observeEvent(input$view_graph, {
#     view_rvalues$data <<- graph_data()
#   })
#   
#   return(view_rvalues)
# }
