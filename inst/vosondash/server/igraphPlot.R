# create graph data for a standard network plot
standardPlotData <- reactive({
  g <- graphFilters()
  
  if (is.null(g)) { return(emptyPlotMessage("No graph data.")) }
  if (vcount(g) <= 0) { return(emptyPlotMessage("No vertices to plot.")) }
  
  # reactive dependencies
  isolate({
    # already dependencies of graphFilters
    categorical_attributes <- ng_rv$graph_cats
    selected_categorical_attribute <- input$graph_cat_select
  })
  
  selected_rows <- input$dt_vertices_rows_selected
  selected_edge_rows <- input$dt_edges_rows_selected
  chosen_layout <- input$graph_layout_select
  graph_seed <- ng_rv$graph_seed
  graph_spread <- input$graph_spread_slider  
  node_degree_type <- input$graph_node_size_degree_select
  node_size_multiplier <- input$graph_node_size_slider  
  
  if (is.null(V(g)$label)) {
    V(g)$label <- V(g)$name
  }
  
  df <- data.frame(label = V(g)$label,
                   name = V(g)$name, 
                   degree = V(g)$Degree, 
                   indegree = V(g)$Indegree, 
                   outdegree = V(g)$Outdegree, 
                   betweenness = V(g)$Betweenness, 
                   closeness = V(g)$Closeness,
                   stringsAsFactors = FALSE)
  row.names(df) <- V(g)$id
  graph_vertices <- df
  
  # set default vertex color
  V(g)$color <- as.character(gbl_plot_def_vertex_color)
  
  # vertex colours (only if cat attr selected)
  if (length(categorical_attributes) > 0) { # only if have categorical attributes
    
    if (nchar(selected_categorical_attribute) && selected_categorical_attribute != "All") {
      
      categories <- categorical_attributes[[selected_categorical_attribute]]
      df <- data.frame('cat' = categories)
      if (nrow(df) > 0) {
        df$color <- gbl_plot_palette()[1:nrow(df)]
        
        va <- paste0('vosonCA_', selected_categorical_attribute)
        V(g)$color <- df$color[match(vertex_attr(g, va), df$cat)]
      }
    }
  }
  
  selected_row_names <- c()
  if (length(selected_rows) > 0) {
    selected_row_names <- row.names(graph_vertices)[c(selected_rows)]
  }
  
  plot_parameters <- list(g, vertex.frame.color = "gray", edge.arrow.size = 0.4)
  
  # set vertex color for vertices selected in graph data table
  plot_parameters[['vertex.color']] <- ifelse(V(g)$id %in% selected_row_names, gbl_plot_sel_vertex_color, V(g)$color)
  plot_parameters[['vertex.label.font']] <- ifelse(V(g)$id %in% selected_row_names, 2, 1)
  
  base_vertex_size <- 4
  base_label_size <- 0.8
  label_dist <- 0.6
  label_degree <- -(pi)/2  
  norm_multi <- 3

  igraph_vsize <- function(x) {
    base_vertex_size + (((norm_values(x) + 0.1) * norm_multi) * node_size_multiplier)
  }

  plot_parameters[['vertex.size']] <- switch(node_degree_type,
                                             "Degree" = igraph_vsize(V(g)$Degree),
                                             "Indegree" = igraph_vsize(V(g)$Indegree),
                                             "Outdegree" = igraph_vsize(V(g)$Outdegree),
                                             "Betweenness" = igraph_vsize(V(g)$Betweenness),
                                             "Closeness" = igraph_vsize(V(g)$Closeness),
                                             "None" = (base_vertex_size + 0.1) * node_size_multiplier)
  
  # avoid unknown font warnings on windows by setting TT font
  if (.Platform$OS.type != "unix") {
    windowsFonts(Arial = windowsFont("TT Arial"))
  }
  
  plot_parameters['vertex.label.family'] <- "Arial"
  
  plot_parameters['vertex.label.cex'] <- base_label_size
  plot_parameters['vertex.label.dist'] <- label_dist
  plot_parameters['vertex.label.degree'] <- label_degree
  
  labels <- FALSE
  if (!(is.null(vertex_attr(g, "label")))) {
    labels <- TRUE
  }
    
  if (input$graph_names_check == FALSE) {
    if (labels) {
      plot_parameters[['vertex.label']] <- ifelse(V(g)$id %in% selected_row_names, 
                                                  ifelse(nchar(V(g)$label) > 0, V(g)$label, "-"), NA)
    } else {
      plot_parameters[['vertex.label']] <- ifelse(V(g)$id %in% selected_row_names, 
                                                  ifelse(nchar(V(g)$name) > 0, V(g)$name, "-"), NA)
    }
  } else {
    if (labels) {
      plot_parameters[['vertex.label']] <- ifelse(nchar(V(g)$label) > 0, V(g)$label, "-")
    } else {
      plot_parameters[['vertex.label']] <- ifelse(nchar(V(g)$name) > 0, V(g)$name, "-")
    }
  }
  
  # plot_parameters[['vertex.label.color']] = ifelse(V(g)$id %in% selected_row_names, gbl_plot_sel_vertex_color, 
  #                                                  gbl_plot_def_label_color)
  plot_parameters[['vertex.label.color']] <- gbl_plot_def_label_color
  
  plot_parameters[['vertex.label.cex']] <- switch(node_degree_type,
                                            "Degree" = (norm_values(V(g)$Degree)) + base_label_size,
                                            "Indegree" = (norm_values(V(g)$Indegree)) + base_label_size,
                                            "Outdegree" = (norm_values(V(g)$Outdegree)) + base_label_size,
                                            "Betweenness" = (norm_values(V(g)$Betweenness)) + base_label_size,
                                            "Closeness" = (norm_values(V(g)$Closeness)) + base_label_size,
                                            "None" = base_label_size)
  
  # must be set before graph layout
  if (!is.null(graph_seed)) {
    set.seed(graph_seed)
  }
  
  graph_layout <- switch(chosen_layout,
                         "Auto" = layout_nicely(g, dim = 2),
                         "FR" = layout_with_fr(g, dim = 2, niter = 500),
                         "KK" = layout_with_kk(g, dim = 2),
                         "DH" = layout_with_dh(g),
                         "LGL" = layout_with_lgl(g),
                         "Graphopt" = layout_with_graphopt(g),
                         "DrL" = layout_with_drl(g),
                         "GEM" = layout_with_gem(g),
                         "MDS" = layout_with_mds(g),
                         # "Tree" = layout_as_tree(g),
                         "Grid" = layout_on_grid(g),
                         "Sphere" = layout_on_sphere(g),
                         "Circle" = layout_in_circle(g),
                         "Star" = layout_as_star(g),
                         "Random" = layout_randomly(g),
                         layout_nicely(g, dim = 2)
  )
  
  graph_layout <- norm_coords(graph_layout, ymin = -1, ymax = 1, xmin = -1, xmax = 1)
  plot_parameters['rescale'] <- FALSE

  plot_parameters[['layout']] <- graph_layout * graph_spread
  
  par(mar = rep(0, 4))
  do.call(plot.igraph, plot_parameters)
})
