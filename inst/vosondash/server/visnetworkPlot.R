visNetworkData <- reactive({
  verts <- graphNodes()
  edges <- graphEdges()
  
  if (is.null(verts) | is.null(edges)) { return(NULL) }
  if (nrow(verts) < 1) { return(NULL) }
  
  isolate({
    # already dependencies of graphFilters, verts, edges
    categorical_attributes <- ng_rv$graph_cats
    selected_categorical_attribute <- input$graph_cat_select
    gcs <- ng_rv$graph_cat_selected
  })
  
  verts_rows_selected <- input$dt_vertices_rows_selected
  chosen_layout <- input$graph_layout_select
  graph_seed <- ng_rv$graph_seed
  node_degree_type <- input$graph_node_size_select
  node_size_multiplier <- input$graph_node_size_slider
  plot_height <- ng_rv$plot_height
  
  use_v_colors <- input$use_vertex_colors_check
  node_index_check <- input$node_index_check
  
  graph_layout <- switch(chosen_layout,
                         "Auto" = "layout_nicely",
                         "FR" = "layout_with_fr",   # Fruchterman-Reingold
                         "KK" = "layout_with_kk",   # Kamada-Kawai
                         "DH" = "layout_with_dh",   # Davidson-Harel
                         "LGL" = "layout_with_lgl", # Large Graph Layout
                         "Graphopt" = "layout_with_graphopt",
                         "DrL" = "layout_with_drl",
                         "GEM" = "layout_with_gem",
                         "MDS" = "layout_with_mds",
                         "Tree" = "layout_as_tree",
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

  v_color_in_data <- FALSE
  if ("color" %in% names(verts)) { v_color_in_data <- TRUE }
  
  if (nrow(verts) > 0) {
    verts$color.background <- as.character(gbl_plot_def_vertex_color)

    if (use_v_colors & v_color_in_data) { # added checkbox
      verts$color.background <- verts$color
    }
    
    verts$font.color <- gbl_plot_def_label_color
    # verts$id <- row.names(verts)
    verts$id <- verts$name
  }
  
  # vertex colours (only if cat attr selected)
  if (length(categorical_attributes) > 0) { # only if have categorical attributes
    
    if (nchar(selected_categorical_attribute) && selected_categorical_attribute != "All") {
      
      categories <- categorical_attributes[[selected_categorical_attribute]]
      df <- data.frame('cat' = categories)
      if (nrow(df) > 0) {
        df$color <- gbl_plot_palette()[1:nrow(df)]
        if (use_v_colors == FALSE || !v_color_in_data) { # added checkbox
          verts$color.background <- df$color[match(verts[[selected_categorical_attribute]], df$cat)]
        }
      }
    }
  }
  
  verts$sel_label <- NA
  if (length(verts_rows_selected) > 0) {
    sel_dt_row_names <- row.names(verts)[c(verts_rows_selected)] # get df row names for verts in dt selection
    sel_subset <- row.names(verts) %in% sel_dt_row_names
    
    verts$color.background[sel_subset] <- gbl_plot_sel_vertex_color
    verts$font.color[sel_subset] <- gbl_sel_label_col
    
    verts$sel_label[sel_subset] <- verts$label[sel_subset]
  }
  
  if (node_index_check) {
    verts$title <- verts$label
    verts$label <- sub("n", "", row.names(verts))
    verts$shape <- "circle"
  } else {
    verts$shape <- "dot"
    if (input$node_labels_check == FALSE) {
      if (input$node_sel_labels_check == TRUE) {
        verts <- dplyr::mutate(verts, label = ifelse(!is.na(.data$sel_label), .data$sel_label, ""),
                               sel_label = NULL)
      } else {
        verts$label <- ""  
      }
    } else {
      # verts <- dplyr::mutate(verts, label = ifelse(is.na(.data$sel_label), .data$label, .data$sel_label),
      #                        sel_label = NULL)
      verts <- dplyr::mutate(verts, label = ifelse(is.na(.data$label), .data$name, .data$label))
    }
    verts$title <- row.names(verts)
  }
  
  if (!"width" %in% names(edges)) {
    if ("weight" %in% names(edges)) {
      edges$width <- edges$weight
    } else {
      medge <- isolate(input$graph_multi_edge_check)
      if (medge == FALSE) {
        edges <- edges %>%
          group_by(to, from) %>%
          summarise(width = n(), .groups = "drop") %>% 
          ungroup()
      }
    }
  }
  
  # edges <- edges %>%
  #   group_by(to, from) %>%
  #   summarise(width = n()) %>% 
  #   ungroup()
  # .groups = "drop"
  
  category_selection <- NULL
  if (!is.null(gcs) && (!(gcs %in% c("All", "")))) {
    category_selection <- list(variable = gcs, multiple = TRUE)
  }
  
  if ("color" %in% names(verts)) { verts <- dplyr::select(verts, -color) }

  # vis_net <- visNetworkProxy("visNetworkPlot") %>% visUpdateNodes(verts)
  vis_net <- visNetwork::visNetwork(verts, edges, main = NULL)
  
  l_params <- list(vis_net, layout = graph_layout, randomSeed = graph_seed)
  if (chosen_layout %in% c("FR", "Graphopt")) { l_params['niter'] <- input$graph_niter }
  if (chosen_layout == "Graphopt") {
    l_params['charge'] = input$graph_charge
    l_params['mass'] = input$graph_mass
    l_params['spring.length'] = input$graph_spr_len
    l_params['spring.constant'] = input$graph_spr_const    
  }
  vis_net <- do.call(visIgraphLayout, l_params)
  
  vis_net <- vis_net %>%
    visOptions(collapse = TRUE, 
               highlightNearest = list(enabled = TRUE, hover = TRUE),
               selectedBy = category_selection,
               nodesIdSelection = TRUE,
               height = plot_height) %>%
    visInteraction(multiselect = TRUE) %>%
    visEvents(click = "function(v) { 
                // if (v.event.srcEvent.ctrlKey) {
                //   Shiny.onInputChange('vis_nbh_node_select', v.nodes);
                // } else {
                  Shiny.onInputChange('vis_node_select', v.nodes);
                // }
                }")
  
  if (ng_rv$graph_dir) { 
    vis_net <- vis_net %>% visEdges(arrows = "to", color = list(color = "#b0b0b0"))
  } else {
    vis_net <- vis_net %>% visEdges(color = list(color = "#b0b0b0"))
  }
  
  vis_net
})
