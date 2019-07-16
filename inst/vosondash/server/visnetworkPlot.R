visNetworkData <- reactive({
  verts <- dt_vertices_df()
  edges <- dt_edges_df()
  
  if (is.null(verts) | is.null(edges)) { return(NULL) }
  if (nrow(verts) < 1) { return(NULL) }
  
  chosen_layout <- input$graph_layout_select
  graph_layout <- switch(chosen_layout,
                         "Auto" = "layout_nicely",
                         "Fruchterman-Reingold" = "layout_with_fr",
                         "Kamada-Kawai" = "layout_with_kk",
                         "Davidson-Harel" = "layout_with_dh",
                         "Large Graph Layout" = "layout_with_lgl",
                         "Grid" = "layout_on_grid",
                         "Sphere" = "layout_on_sphere",
                         "Circle" = "layout_in_circle",
                         "Star" = "layout_as_star",
                         "Random" = "layout_randomly",
                         "layout_nicely")
  
  graph_seed <- ng_rv$graph_seed
  
  # node size
  
  node_degree_type <- input$graph_node_size_degree_select
  node_size_multiplier <- input$graph_node_size_slider  
  
  base_vertex_size <- 8
  
  if (node_degree_type == "None") {
    if (node_size_multiplier > 1) {
      verts$size <- base_vertex_size + (node_size_multiplier / 4)
    } else {
      verts$size <- base_vertex_size
    }
  } else {
    # todo: needs to calculate average values to better adjust scale
    verts$size <- switch(node_degree_type,
                         "Degree" = (verts$degree / 4 * node_size_multiplier) + base_vertex_size,
                         "Indegree" = (verts$indegree / 2 * node_size_multiplier) + base_vertex_size,
                         "Outdegree" = (verts$outdegree / 2 * node_size_multiplier) + base_vertex_size,
                         "Betweenness" = (verts$betweenness / 100 * node_size_multiplier) + base_vertex_size,
                         "Closeness" = (verts$closeness * 100 * node_size_multiplier) + base_vertex_size
    )
  }
  
  # category colors
  
  isolate({
    # already dependencies of graphFilters
    categorical_attributes <- ng_rv$graph_cats
    selected_categorical_attribute <- input$graph_cat_select
  })
  
  if (nrow(verts) > 0) {
    verts$color.background <- as.character(gbl_plot_def_vertex_color)
    verts$font.color <- gbl_plot_def_label_color
    verts$id <- verts$name
  }
  
  # vertex colours (only if cat attr selected)
  if (length(categorical_attributes) > 0) { # only if have categorical attributes
    
    if (nchar(selected_categorical_attribute) && selected_categorical_attribute != "All") {
      
      categories <- categorical_attributes[[selected_categorical_attribute]]
      df <- data.frame('cat' = categories)
      if (nrow(df) > 0) {
        df$color <- gbl_plot_palette()[1:nrow(df)]
        verts$color.background <- df$color[match(verts[[selected_categorical_attribute]], df$cat)]
      }
    }
  }
  
  if (length(input$dt_vertices_rows_selected) > 0) {
    selected_row_names <- row.names(verts)[c(input$dt_vertices_rows_selected)]
    verts$color.background[row.names(verts) %in% selected_row_names] <- gbl_plot_sel_vertex_color
    verts$font.color[row.names(verts) %in% selected_row_names] <- gbl_plot_sel_vertex_color
  }
  
  edges <- edges %>% group_by(to, from) %>%
    summarise(width = n()) %>% 
    ungroup()
  
  category_selection <- NULL
  if (!is.null(ng_rv$graph_cat_selected) && (!(ng_rv$graph_cat_selected %in% c("All", "")))) {
    category_selection <- list(variable = ng_rv$graph_cat_selected, multiple = TRUE)
  }
  
  visNetwork::visNetwork(verts, edges, main = NULL) %>% # height = "500px"
    visIgraphLayout(layout = graph_layout, 
                    randomSeed = graph_seed) %>%
    
    visNetwork::visEdges(arrows = 'to',
                         # smooth = list(enabled = TRUE, type = "continuous", roundness = 0.1)
                         color = list(color = "#b0b0b0")) %>% # arrows = 'to, from'
    
    visOptions(collapse = TRUE, 
               highlightNearest = list(enabled = TRUE, hover = TRUE),
               selectedBy = category_selection,
               nodesIdSelection = TRUE,
               height = ng_rv$plot_height) # ng_rv$plot_height
  
  # visInteraction(navigationButtons = TRUE)
})
