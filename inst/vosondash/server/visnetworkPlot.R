visNetworkData <- reactive({
  verts <- dt_vertices_df()
  edges <- dt_edges_df()
  
  if (is.null(verts) | is.null(edges)) { return(NULL) }
  if (nrow(verts) < 1) { return(NULL) }
  
  isolate({
    # already dependencies of graphFilters / verts / edges
    categorical_attributes <- ng_rv$graph_cats
    selected_categorical_attribute <- input$graph_cat_select
    gcs <- ng_rv$graph_cat_selected
  })
  
  verts_rows_selected <- input$dt_vertices_rows_selected
  chosen_layout <- input$graph_layout_select
  graph_seed <- ng_rv$graph_seed
  node_degree_type <- input$graph_node_size_degree_select
  node_size_multiplier <- input$graph_node_size_slider
  plot_height <- ng_rv$plot_height
  
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
  
  verts$font.size <- 24
  base_vertex_size <- 20
  norm_multi <- 5
  
  vis_vsize <- function(x) {
    base_vertex_size + (((norm_values(x) + 0.1) * norm_multi) * node_size_multiplier)
  }
  
  verts$size <- switch(node_degree_type,
                       "Degree" = vis_vsize(verts$degree),
                       "Indegree" = vis_vsize(verts$indegree),
                       "Outdegree" = vis_vsize(verts$outdegree),
                       "Betweenness" = vis_vsize(verts$betweenness),
                       "Closeness" = vis_vsize(verts$closeness),
                       "None" = (base_vertex_size + 0.1) * node_size_multiplier)

  if (nrow(verts) > 0) {
    verts$color.background <- as.character(gbl_plot_def_vertex_color)
    verts$font.color <- gbl_plot_def_label_color
    verts$id <- verts$name
  }
  
  if (input$graph_names_check == FALSE) {
    verts$label <- "" 
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
  
  if (length(verts_rows_selected) > 0) {
    selected_row_names <- row.names(verts)[c(verts_rows_selected)]
    verts$color.background[row.names(verts) %in% selected_row_names] <- gbl_plot_sel_vertex_color
    verts$font.color[row.names(verts) %in% selected_row_names] <- gbl_plot_sel_vertex_color
  }
  
  edges <- edges %>% group_by(to, from) %>%
    summarise(width = n()) %>% 
    ungroup()
  
  category_selection <- NULL
  if (!is.null(gcs) && (!(gcs %in% c("All", "")))) {
    category_selection <- list(variable = gcs, multiple = TRUE)
  }
  
  visNetwork::visNetwork(verts, edges, main = NULL) %>%
    visIgraphLayout(layout = graph_layout, 
                    randomSeed = graph_seed) %>%
    
    visNetwork::visEdges(arrows = 'to',
                         color = list(color = "#b0b0b0")) %>%
    
    visOptions(collapse = TRUE, 
               highlightNearest = list(enabled = TRUE, hover = TRUE),
               selectedBy = category_selection,
               nodesIdSelection = TRUE,
               height = plot_height)
})
