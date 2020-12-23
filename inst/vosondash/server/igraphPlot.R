# create graph data for a standard network plot
igraphData <- reactive({
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
  node_degree_type <- input$graph_node_size_select
  node_size_multiplier <- input$graph_node_size_slider  
  
  node_index_check <- input$node_index_check
  
  # ------------------
  # save and restore graphics parameters
  saved_par <- par(no.readonly = TRUE)  
  
  # avoid unknown font warnings on windows by setting TT font
  saved_win_font <- NULL
  if (.Platform$OS.type == "windows") {
    saved_win_font <- windowsFonts()$Arial
    windowsFonts(Arial = windowsFont("TT Arial"))
  }
  
  on.exit({
    par(saved_par)
    if (!is.null(saved_win_font)) {
      windowsFonts(Arial = windowsFont(saved_win_font))
    }
  })
  # ------------------
  
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
  
  # set vertex color
  v_color_in_data <- FALSE
  if ("color" %in% vertex_attr_names(g)) { v_color_in_data <- TRUE }
  
  if (input$use_vertex_colors_check == FALSE) { # added checkbox
    V(g)$color <- as.character(gbl_plot_def_vertex_color)  
  } else {
    if (!v_color_in_data) {
      V(g)$color <- as.character(gbl_plot_def_vertex_color)
    }
  }
  
  # vertex colours (only if cat attr selected)
  if (length(categorical_attributes) > 0) { # only if have categorical attributes
    
    if (nchar(selected_categorical_attribute) && selected_categorical_attribute != "All") {
      
      categories <- categorical_attributes[[selected_categorical_attribute]]
      df <- data.frame('cat' = categories)
      if (nrow(df) > 0) {
        
        if (input$use_vertex_colors_check == FALSE || !v_color_in_data) { # added checkbox
          df$color <- gbl_plot_palette()[1:nrow(df)]
          va <- paste0('vosonCA_', selected_categorical_attribute)
          V(g)$color <- df$color[match(vertex_attr(g, va), df$cat)]  
        }
      }
    }
  }
  
  selected_row_names <- c()
  if (length(selected_rows) > 0) {
    selected_row_names <- row.names(graph_vertices)[c(selected_rows)]
  }
  
  plot_parameters <- list(g, edge.arrow.size = 0.4)
  
  # set vertex color for vertices selected in graph data table
  plot_parameters[['vertex.color']] <- ifelse(V(g)$id %in% selected_row_names, gbl_plot_sel_vertex_color, V(g)$color)
  plot_parameters[['vertex.frame.color']] = ifelse(V(g)$id %in% selected_row_names, "#000000", "gray")
  plot_parameters[['vertex.label.font']] <- ifelse(V(g)$id %in% selected_row_names, 2, 1)
  
  base_vertex_size <- 4
  base_label_size <- 0.8
  label_dist <- 0.6
  label_degree <- -(pi)/2  
  norm_multi <- 3

  igraph_vsize <- function(x) {
    base_vertex_size + (((norm_values(x) + 0.1) * norm_multi) * node_size_multiplier)
  }
  
  # --- start id labels
  if (node_index_check) {
    plot_parameters[['vertex.label']] <- sub("n", "", V(g)$id) # V(g)$id
    plot_parameters[['vertex.label.color']] <- "#000000"
    base_vertex_size <- 7
  }
  # --- end id labels  

  plot_parameters[['vertex.size']] <- switch(node_degree_type,
                                             "Degree" = igraph_vsize(V(g)$Degree),
                                             "Indegree" = igraph_vsize(V(g)$Indegree),
                                             "Outdegree" = igraph_vsize(V(g)$Outdegree),
                                             "Betweenness" = igraph_vsize(V(g)$Betweenness),
                                             "Closeness" = igraph_vsize(V(g)$Closeness),
                                             "None" = (base_vertex_size + 0.1) * node_size_multiplier)
  
  if (.Platform$OS.type != "windows" & 
      ("Arial Unicode MS" %in% VOSONDash::getSystemFontFamilies()) &
      input$macos_font_check) {
    plot_parameters['vertex.label.family'] <- "Arial Unicode MS"
  } else {
    plot_parameters['vertex.label.family'] <- "Arial"
  }
  
  # --- start labels
  if (node_index_check == FALSE) {
    if (input$node_labels_check == FALSE) {
      plot_parameters[['vertex.label']] <- NA
    }
    
    plot_parameters['vertex.label.cex'] <- base_label_size
    plot_parameters['vertex.label.dist'] <- label_dist
    plot_parameters['vertex.label.degree'] <- label_degree
  
    labels <- ifelse(!is.null(vertex_attr(g, "label")), TRUE, FALSE)
  
    if (input$node_labels_check == FALSE) {
      if (input$node_sel_labels_check == TRUE) {
        if (labels) {
          plot_parameters[['vertex.label']] <- ifelse(V(g)$id %in% selected_row_names, 
                                                      ifelse(nchar(V(g)$label) > 0, V(g)$label, "-"), NA)
        } else {
          plot_parameters[['vertex.label']] <- ifelse(V(g)$id %in% selected_row_names, 
                                                      ifelse(nchar(V(g)$name) > 0, V(g)$name, "-"), NA)
        }
      }
    } else {
      if (labels) {
        plot_parameters[['vertex.label']] <- ifelse(nchar(V(g)$label) > 0, V(g)$label, "-")
      } else {
        plot_parameters[['vertex.label']] <- ifelse(nchar(V(g)$name) > 0, V(g)$name, "-")
      }
    }
  
    plot_parameters[['vertex.label.color']] = ifelse(V(g)$id %in% selected_row_names, gbl_sel_label_col, 
                                                     gbl_plot_def_label_color)

    plot_parameters[['vertex.label.cex']] <- switch(node_degree_type,
                                              "Degree" = (norm_values(V(g)$Degree)) + base_label_size,
                                              "Indegree" = (norm_values(V(g)$Indegree)) + base_label_size,
                                              "Outdegree" = (norm_values(V(g)$Outdegree)) + base_label_size,
                                              "Betweenness" = (norm_values(V(g)$Betweenness)) + base_label_size,
                                              "Closeness" = (norm_values(V(g)$Closeness)) + base_label_size,
                                              "None" = base_label_size)
  }
  # --- end labels
  
  # must be set before graph layout
  if (!is.null(graph_seed)) {
    set.seed(graph_seed)
  }
  
  graph_layout <- switch(chosen_layout,
                         "Auto" = layout_nicely(g, dim = 2),
                         "FR" = layout_with_fr(g, dim = 2, niter = input$graph_niter),
                         "KK" = layout_with_kk(g, dim = 2),
                         "DH" = layout_with_dh(g),
                         "LGL" = layout_with_lgl(g),
                         "DrL" = layout_with_drl(g),
                         "GEM" = layout_with_gem(g),
                         "MDS" = layout_with_mds(g),
                         "Tree" = layout_as_tree(g, circular = TRUE),
                         "Grid" = layout_on_grid(g),
                         "Sphere" = layout_on_sphere(g),
                         "Circle" = layout_in_circle(g),
                         "Star" = layout_as_star(g),
                         "Random" = layout_randomly(g),
                         layout_nicely(g, dim = 2)
  )
  
  if (chosen_layout == "Graphopt") {
    graph_layout <- layout_with_graphopt(g, niter = input$graph_niter, 
                                         charge = input$graph_charge,
                                         mass = input$graph_mass,
                                         spring.length = input$graph_spr_len,
                                         spring.constant = input$graph_spr_const)
  }
  
  graph_layout <- igraph::norm_coords(graph_layout, ymin = -1, ymax = 1, xmin = -1, xmax = 1)
  plot_parameters['rescale'] <- FALSE

  plot_parameters[['layout']] <- graph_layout * graph_spread
  
  par(mar = rep(0, 4))
  do.call(plot.igraph, plot_parameters)
})
