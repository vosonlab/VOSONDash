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
    return(VOSONDash::emptyPlotMessage("No graph data."))
  }
  
  cc <- components(g, mode = input$graph_component_type_select)
  plot(table(cc$csize), type = "b", xlab = paste0("Size of component (", input$graph_component_type_select, ")"), 
       ylab = "N")
}) 

degreeDistPlotData <- reactive({
  g <- graphFilters()
  
  if (is.null(g)) {
    return(VOSONDash::emptyPlotMessage("No graph data."))
  }
  
  if (is.directed(g)) {
    VOSONDash::emptyPlotMessage("Not defined for undirected network.")
  } else {
    plot(table(degree(g)), type = "b", xlab = "Degree", ylab = "N")
  }
})

indegreeDistPlotData <- reactive({
  g <- graphFilters()
  
  if (is.null(g)) {
    return(VOSONDash::emptyPlotMessage("No graph data."))
  }
  
  if (is.directed(g)){
    plot(table(degree(g, mode="in")), type = "b", xlab = "Indegree", ylab = "N")
  } else {
    VOSONDash::emptyPlotMessage("Not defined for undirected network.")
  }
}) 

outdegreeDistPlotData <- reactive({
  g <- graphFilters()
  
  if (is.null(g)) {
    return(VOSONDash::emptyPlotMessage("No graph data."))
  }
  
  if (is.directed(g)){
    plot(table(degree(g, mode="out")), type="b", xlab="Outdegree", ylab="N")
  } else {
    VOSONDash::emptyPlotMessage("Not defined for undirected network.")
  }
})

networkMetricsDetailsOutput <- reactive({
  g <- graphFilters()
  
  output <- c()
  
  if (!is.null(g)) {
    graph_clusters <- components(g, mode = input$graph_component_type_select)
    
    output <- append(output, c(
      paste("Number of nodes (network size):", vcount(g)),
      paste("Number of edges:", ecount(g)),
      paste0("Number of components (", input$graph_component_type_select, "): ", graph_clusters$no),
      paste("Number of isolates:", length(which(degree(g) == 0))),
      paste("Density:", sprintf("%.3f", graph.density(g))),
      paste("Average geodesic distance:", sprintf("%.3f", mean_distance(g))), "",
      paste("(Global) clustering coefficient:", sprintf("%.3f", transitivity(g))),
      "  Proportion of connected triples that close to form triangles",
      paste("Reciprocity - 1:", sprintf("%.3f", reciprocity(g, mode = "default"))),
      "  Ratio of number of dyads with reciprocated (mutual) edges to number of dyads with single edge",
      paste("Reciprocity - 2:", sprintf("%.3f", reciprocity(g, mode = "ratio"))),
      "  Ratio of total number of reciprocated edges to total number of edges", ""
    ))
    
    if (is.directed(g)){
      output <- append(output, c(
        paste("Indegree centralization:", sprintf("%.3f", centr_degree(g, mode = "in")$centralization)),
        paste("Outdegree centralization:", sprintf("%.3f", centr_degree(g, mode = "out")$centralization))
      ))
    }else{
      output <- append(output, paste("Degree centralization:", sprintf("%.3f", centr_degree(g)$centralization)))
    }
    
    output <- append(output, c(
      paste("Betweenness centralization:", sprintf("%.3f", centr_betw(g)$centralization)),
      paste("Closeness centralization:", sprintf("%.3f", suppressWarnings(centr_clo(g)$centralization)))
    ))
    
  } else {
    output <- append(output, "No graph data.")
  }
  
  paste0(output, collapse = '\n')
})
