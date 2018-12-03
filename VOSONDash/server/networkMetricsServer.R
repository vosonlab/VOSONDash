#' VOSON Dashboard networkMetricsServer
#'
#' Network metrics and line plots.
#'

#### values -----------------------------------------------------------------------------------------------------------
#### events -----------------------------------------------------------------------------------------------------------
#### output -----------------------------------------------------------------------------------------------------------

output$network_metrics_details_output <- renderText({
  networkMetricsDetailsOutput()
})

output$componentDistPlot <- renderPlot({
  componentDistPlotData()
})

output$degreeDistPlot <- renderPlot({
  degreeDistPlotData()
})

output$indegreeDistPlot <- renderPlot({
  indegreeDistPlotData()
})

output$outdegreeDistPlot <- renderPlot({
  outdegreeDistPlotData()
})

#### reactives --------------------------------------------------------------------------------------------------------

componentDistPlotData <- reactive({
  g <- graphFilters()
  
  if (is.null(g)) {
    return(emptyMetricsPlotMessage("No graph data."))
    # return(NULL)
  }
  
  cc <- components(g, mode = input$graph_component_type_select)
  plot(table(cc$csize), type = "b", xlab = paste0("Size of component (", input$graph_component_type_select, ")"), ylab = "N")
}) 

degreeDistPlotData <- reactive({
  g <- graphFilters()
  
  if (is.null(g)) {
    return(emptyMetricsPlotMessage("No graph data."))
    # return(NULL)
  }
  
  if (is.directed(g)) {
    emptyMetricsPlotMessage("Not defined for undirected network.")
  } else {
    plot(table(degree(g)), type = "b", xlab = "Degree", ylab = "N")
  }
})

indegreeDistPlotData <- reactive({
  g <- graphFilters()
  
  if (is.null(g)) {
    return(emptyMetricsPlotMessage("No graph data."))
    # return(NULL)
  }
  
  if (is.directed(g)){
    plot(table(degree(g, mode="in")), type = "b", xlab = "Indegree", ylab = "N")
  } else {
    emptyMetricsPlotMessage("Not defined for undirected network.")
  }
}) 

outdegreeDistPlotData <- reactive({
  g <- graphFilters()
  
  if (is.null(g)) {
    return(emptyMetricsPlotMessage("No graph data."))
    # return(NULL)
  }
  
  if (is.directed(g)){
    plot(table(degree(g, mode="out")), type="b", xlab="Outdegree", ylab="N")
  } else {
    emptyMetricsPlotMessage("Not defined for undirected network.")
  }
})

networkMetricsDetailsOutput <- reactive({
  g <- graphFilters()
  
  output <- c()
  
  if (!is.null(g)) {
    graph_clusters <- components(g, mode = input$graph_component_type_select)
    
    output <- append(output, paste0("Number of nodes (network size): ", vcount(g)))
    output <- append(output, paste0("Number of edges: ", ecount(g)))
    output <- append(output, paste0("Number of components (", input$graph_component_type_select, "): ", graph_clusters$no))
    output <- append(output, paste0("Number of isolates: ", length(which(degree(g)==0))))
    output <- append(output, paste0("Density: ", sprintf("%.3f", graph.density(g))))
    output <- append(output, paste0("Average geodesic distance: ", sprintf("%.3f", mean_distance(g))))
    output <- append(output, "")
    output <- append(output, paste0("(Global) clustering coefficient: ", sprintf("%.3f", transitivity(g))))
    output <- append(output, "  Proportion of connected triples that close to form triangles")
    output <- append(output, paste0("Reciprocity - 1: ", sprintf("%.3f", reciprocity(g, mode="default"))))
    output <- append(output, "  Ratio of number of dyads with reciprocated (mutual) edges to number of dyads with single edge")
    output <- append(output, paste0("Reciprocity - 2: ", sprintf("%.3f", reciprocity(g, mode="ratio"))))
    output <- append(output, "  Ratio of total number of reciprocated edges to total number of edges")
    output <- append(output, "")
    if (is.directed(g)){
      output <- append(output, paste0("Indegree centralization: ", sprintf("%.3f", centr_degree(g, mode="in")$centralization)))
      output <- append(output, paste0("Outdegree centralization: ", sprintf("%.3f", centr_degree(g, mode="out")$centralization)))
    }else{
      output <- append(output, paste0("Degree centralization: ", sprintf("%.3f", centr_degree(g)$centralization)))
    }
    output <- append(output, paste0("Betweenness centralization: ", sprintf("%.3f", centr_betw(g)$centralization)))
    output <- append(output, paste0("Closeness centralization: ", sprintf("%.3f", suppressWarnings(centr_clo(g)$centralization))))
    
  } else {
    output <- append(output, paste0("No graph data."))
  }
  
  paste0(output, collapse = '\n')
})

#### functions --------------------------------------------------------------------------------------------------------

# return empty plot with message
emptyMetricsPlotMessage <- function(message) {
  return({ plot(1:10, 1:10, type = "n", axes = F, xlab = "", ylab = "")
    text(5, 5, message, cex = 1.2) })
}