#' VOSON Dashboard networkGraphsServer
#'
#' Network data, measures, filters and graph visualisations.
#'

#### values ----------------------------------------------------------------------------------------------------------- #

ng_rvalues <- reactiveValues(
  data = NULL,                  # vosonsml df
  graph_data = NULL,            # graphml object
  graph_seed = NULL,
  
  graph_desc = "",
  graph_name = "",
  graph_type = "",
  
  graph_CA = c(),               # list of categories in the data
  graph_CA_selected = "",        # list of chosen attributes for selected category
  
  plot_height = g_plot_height
)

# list of user selected graph vertices to prune
prune_flag <- FALSE
pruning_rvalues <- reactiveValues(
  prune_verts = c()
)

# proxy for vertices data table used for row manipulation
dt_vertices_proxy = dataTableProxy('dt_vertices')
# dt_edges_proxy = dataTableProxy('dt_edges')

#### events ----------------------------------------------------------------------------------------------------------- #

output$test_graph_summary <- renderUI({
  tagList(
    div(
      div(
          div(selectInput("plot_height", label = NULL, 
                          choices = c("300px" = 300, "400px" = 400, "500px" = 500, "600px" = 600, "700px" = 700, "800px" = 800, "900px" = 900, "1000px" = 1000), 
                          multiple = FALSE, selectize = FALSE, selected = ng_rvalues$plot_height), 
              style = "width:100%;", align = "right"),
          
          HTML(graphSummaryOutput()),
            style = "position:absolute; z-index:1; top:60px; right:40px; font-size:0.97em;"),
    style = "position:relative; z-index:0;")
  )
})

output$test_vis_graph <- renderUI({
  tabBox(width = 12, title = span(icon("share-alt", class = "social_green"), "Network Graphs"), 
         selected = input$selected_graph_tab, id = "selected_graph_tab",
         tabPanel("igraph", plotOutput("standardPlot", width = "100%", height = "auto"), # 500px
                  value = "Plot"),
         tabPanel("visNetwork", visNetworkOutput("visNetworkPlot", width = "100%",
                                                 height = paste0(ng_rvalues$plot_height, "px")), value = "visNetwork") # ,
         # tabPanel("D3 Force", forceNetworkOutput("force", width = "100%", height = "500px")),
         # tabPanel("D3 Simple", simpleNetworkOutput("simple", width = "100%", height = "500px"))
  )
})

observeEvent(input$plot_height, {
  ng_rvalues$plot_height <- input$plot_height
}, ignoreInit = TRUE)

# disable network metrics and assortativity tabs when app loads
addCssClass(selector = "a[data-value = 'network_metrics_tab']", class = "inactive_menu_link")
addCssClass(selector = "a[data-value = 'assortativity_tab']", class = "inactive_menu_link")

# do once at startup
check_demo_files <- TRUE
observeEvent(check_demo_files, {
  tryCatch({
    demo_files_list <- list.files(path = system.file("extdata", "", package = "VOSONDash", mustWork = TRUE),
                                  pattern = "\\.graphml$")
    
    if (length(demo_files_list) > 0) {
      updateSelectInput(session, "demo_data_select", label = NULL, choices = demo_files_list,
                        selected = NULL)
      shinyjs::enable("demo_data_select")
      shinyjs::enable("demo_data_select_button")
    }    
  }, error = function(err) {
    # cat(paste("error loading demo files:", err))
  }, warning = function(w) {
    # cat(paste("warning loading demo files:", w))
  })
}, once = TRUE)

observeEvent(input$demo_data_select_button, {
  load_file <- system.file("extdata", input$demo_data_select, package = "VOSONDash")

  if (load_file != "") {
    file_desc <- "Description not found."
    tryCatch({
      file_desc <- paste(readLines(paste0(load_file, ".txt")), collapse = "<br>")
    }, error = function(err) {
      # cat(paste("error loading demo files:", err))
    }, warning = function(w) {
      # cat(paste("warning loading demo files:", w))
    })
    
    tryCatch({
      data <- igraph::read_graph(load_file, format = c('graphml'))
      
      type <- ifelse("type" %in% graph_attr_names(data), 
                                       graph_attr(data, "type"), "")
      
      setGraphView(data = data, 
                   desc = file_desc,
                   type = type,
                   name = input$demo_data_select,
                   seed = sample(g_random_number_range[1]:g_random_number_range[2], 1))
    }, error = function(err) {
      # cat(paste("error loading demo files:", err))
    }, warning = function(w) {
      # cat(paste("warning loading demo files:", w))
    })
  }
})

# enable network metrics tab when graph data loaded
observeEvent(ng_rvalues$graph_data, {
  if (!is.null(ng_rvalues$graph_data)) {
    removeCssClass(selector = "a[data-value = 'network_metrics_tab']", class = "inactive_menu_link")
  }
  
  attr_v <- vertex_attr_names(ng_rvalues$graph_data)
  if (!("id" %in% attr_v)) {
    V(ng_rvalues$graph_data)$id <- paste0("n", as.numeric(V(ng_rvalues$graph_data))-1)
  }

  if ("label" %in% attr_v) {
    V(ng_rvalues$graph_data)$label <- ifelse(nchar(V(ng_rvalues$graph_data)$label) > 0, 
                                             V(ng_rvalues$graph_data)$label, "-")
  } else {
    V(ng_rvalues$graph_data)$label <- ifelse(nchar(V(ng_rvalues$graph_data)$name) > 0, 
                                             V(ng_rvalues$graph_data)$name, "-")
  }
})

# enable assortativity tab when category selected
observeEvent(ng_rvalues$graph_CA_selected, {
  if (ng_rvalues$graph_CA_selected %in% c("", "All")) {
    addCssClass(selector = "a[data-value = 'assortativity_tab']", class = "inactive_menu_link")
  } else {
    removeCssClass(selector = "a[data-value = 'assortativity_tab']", class = "inactive_menu_link")
  }
})

# update component slider when graph component or category changed
observeEvent({ input$graph_component_type_select
               input$graph_catAttr_attr_select
               input$reset_on_change_check }, {
  
  g <- ng_rvalues$graph_data
                 
  if (input$reset_on_change_check == TRUE) {
    g <- applyPruneFilter(g, pruning_rvalues$prune_verts)
    g <- applyCategoricalFilters(g, input$graph_catAttr_select, input$graph_catAttr_attr_select)
  }

  updateComponentSlider(g, input$graph_component_type_select)
}, ignoreInit = TRUE)
  
# selected category updates select box with its attribute values
observeEvent(input$graph_catAttr_select, {
  ng_rvalues$graph_CA_selected <<- input$graph_catAttr_select
  
  if (!is.null(ng_rvalues$graph_data)) {
    attr_choices <- c("All")
    
    if (input$graph_catAttr_select != "All") {
      attr_choices <- append(attr_choices, ng_rvalues$graph_CA[[input$graph_catAttr_select]])
    }
    
    # update list of values in select box
    updateSelectInput(session, "graph_catAttr_attr_select",
                      choices = attr_choices, selected = "All")
    
    # enable select box control
    shinyjs::enable("graph_catAttr_attr_select")
  }
})

# set graph controls on graph tab changes
observeEvent(input$selected_graph_tab, {
  setGraphTabControls()
})

# graphml file uploaded
observeEvent(input$graphml_data_file, {
  filedata()
  
  # get a random number to seed graphs
  ng_rvalues$graph_seed <<- sample(g_random_number_range[1]:g_random_number_range[2], 1)
  
  # reset controls and filters
  setGraphTabControls()
  setGraphFilterControls()
})

# generate a new random seed
observeEvent(input$graph_reseed_button, {
  ng_rvalues$graph_seed <<- sample(g_random_number_range[1]:g_random_number_range[2], 1)
})

# reset graph spread when a new layout is selected
observeEvent(input$graph_layout_select, {
  shinyjs::reset("graph_spread_slider")
})

#### graph vertex pruning ####

# add selected data table rows to pruned vertices list
observeEvent(input$prune_selected_rows_button, {
  if (length(input$dt_vertices_rows_selected) > 0) { prune_flag <<- TRUE }
  
  pruneListAddNames()
  
  # update prune list select box
  prune_list <- isolate(pruning_rvalues$prune_verts)
  if (is.null(prune_list)) { 
    prune_list <- character(0) 
  } else {
    temp <- list()
    for (i in prune_list) {
      n_value <- V(isolate(ng_rvalues$graph_data))[which(V(isolate(ng_rvalues$graph_data))$id == i)]$label # name
      temp[paste0(i, " - ", n_value)] <- i
    }
    prune_list <- temp
  }
  updateSelectInput(session, "pruned_vertices_select", choices = prune_list)
})

# add unselected data table rows to pruned vertices list
observeEvent(input$prune_unselected_rows_button, {
  if (length(input$dt_vertices_rows_selected) > 0) { prune_flag <<- TRUE }
  
  pruneListAddOtherNames()
  
  # update prune list select box
  prune_list <- isolate(pruning_rvalues$prune_verts)
  if (is.null(prune_list)) { 
    prune_list <- character(0) 
  } else {
    temp <- list()
    for (i in prune_list) {
      n_value <- V(isolate(ng_rvalues$graph_data))[which(V(isolate(ng_rvalues$graph_data))$id == i)]$name
      temp[paste0(i, " - ", n_value)] <- i
    }
    prune_list <- temp
  }
  updateSelectInput(session, "pruned_vertices_select", choices = prune_list)
})

# remove selected vertices from prune list
observeEvent(input$prune_return_button, {
  if (length(input$pruned_vertices_select) > 0) { prune_flag <<- TRUE }
  
  pruning_rvalues$prune_verts <<- pruning_rvalues$prune_verts[!(pruning_rvalues$prune_verts %in% 
                                                                  input$pruned_vertices_select)]
  
  # update prune list select box
  prune_list <- isolate(pruning_rvalues$prune_verts)
  if (is.null(prune_list)) { 
    prune_list <- character(0) 
  } else {
    temp <- list()
    for (i in prune_list) {
      n_value <- V(isolate(ng_rvalues$graph_data))[which(V(isolate(ng_rvalues$graph_data))$id == i)]$name
      temp[paste0(i, " - ", n_value)] <- i
    }
    prune_list <- temp
  }
  updateSelectInput(session, "pruned_vertices_select", choices = prune_list)
})

# reset prune list
observeEvent(input$prune_reset_button, {
  if (length(isolate(pruning_rvalues$prune_verts)) > 0) { prune_flag <<- TRUE }
  
  pruning_rvalues$prune_verts <<- c()
  
  updateSelectInput(session, "pruned_vertices_select", choices = character(0))
})

# deselect all data table selected rows
observeEvent(input$prune_deselect_rows_button, {
  dt_vertices_proxy %>% selectRows(NULL)
})

#### output ----------------------------------------------------------------------------------------------------------- #

output$graph_summary_output <- renderText({
  graphSummaryOutput()
})

output$graph_name <- renderText({
  output <- ifelse(nchar(ng_rvalues$graph_name), ng_rvalues$graph_name, "Not set")
  output <- paste("Name: ", output)
  
  if (nchar(ng_rvalues$graph_type)) {
    output <- paste0(output, "  (Type: ", ng_rvalues$graph_type, ")")
  }
  
  return(output)
})

output$graph_desc <- renderText({
  if (nchar(ng_rvalues$graph_desc)) {
    return(HTML(ng_rvalues$graph_desc))
  }
  
  return(HTML("No description."))
})

observeEvent(ng_rvalues$graph_desc, {
  if (!isNullOrEmpty(ng_rvalues$graph_desc)) {
    shinyjs::enable("expand_data_desc_check")
  } else {
    shinyjs::disable("expand_data_desc_check")
  }
})

# graph download button for d3 graphs
output$graph_download_button <- downloadHandler(
  filename = function() { saveGraphFileName() },
  
  content = function(file) {
    if (input$selected_graph_tab == "visNetwork") {
      saveGraphFileData() %>% visSave(file, selfcontained = TRUE, background = "white")
    } else {
      saveGraphFileData() %>% saveNetwork(file, selfcontained = TRUE) 
    }
  }
)

# analysis graphml download button
output$analysis_graphml_download_button <- downloadHandler(
  filename = function() { systemTimeFilename("analysis-graph", "graphml") },
  
  content = function(file) {
    # print(graphFilters())
    write_graph(graphFilters(), file, format = c("graphml"))
  }
)

# graph vertices data table
output$dt_vertices <- DT::renderDataTable({
  data <- dt_vertices_df()
  
  # truncate text in column cells
  col_defs <- NULL
  if (input$graph_dt_v_truncate_text_check == TRUE) {
    col_defs <- g_dt_col_defs
    col_defs[[1]]$targets <- "_all"
  }
  
  if (!is.null(data)) {
    dt <- DT::datatable(data, extensions = 'Buttons', filter = "top",
                        options = list(lengthMenu = g_dt_length_menu, pageLength = g_dt_page_length, scrollX = TRUE,
                        columnDefs = col_defs, dom = 'lBfrtip', buttons = c('copy', 'csv', 'excel', 'print')),
                        class = 'cell-border stripe compact hover')

    # format betweeness and closeness values to display 3 decimal places
    if (nrow(data) > 0) {
      dt %>% DT::formatRound(columns = c('betweenness', 'closeness'), digits = 3)
    } else { dt }
  }
})

# graph edges data table
output$dt_edges <- DT::renderDataTable({
  data <- dt_edges_df()
  
  # truncate text in column cells
  col_defs <- NULL
  if (input$graph_dt_e_truncate_text_check == TRUE) {
    col_defs <- g_dt_col_defs
    col_defs[[1]]$targets <- "_all"
  }
  
  if (!is.null(data)) {
    DT::datatable(data, extensions = 'Buttons', filter = "top",
                  options = list(lengthMenu = g_dt_length_menu, pageLength = g_dt_page_length, scrollX = TRUE,
                  columnDefs = col_defs, dom = 'lBfrtip',
                  buttons = c('copy', 'csv', 'excel', 'print')), class = 'cell-border stripe compact hover')
  }
})

# standard network plot
output$standardPlot <- renderPlot({
  standardPlotData()
}, height = function() { as.numeric(ng_rvalues$plot_height) })

# d3 force network graph
output$force <- renderForceNetwork({
  forceNetworkData()
})

# d3 simple network graph
output$simple <- renderSimpleNetwork({
  simpleNetworkData()
})

output$visNetworkPlot <- renderVisNetwork({
  data <- visNetworkData() 
  if (!is.null(data)) {
    data %>% 
    visEvents(select = 
    "function(nodes) {
      Shiny.onInputChange('visnetwork_vertex_selection', nodes.nodes);
    }")
  }
})

# observeEvent(input$visnetwork_vertex_selection, {
#   cat(paste0("selected: ", input$visnetwork_vertex_selection, "\n"))
# })

#### reactives -------------------------------------------------------------------------------------------------------- #

# set file data when a file is uploaded
filedata <- reactive({
  infile <- input$graphml_data_file
  
  if (is.null(infile)) {
    return(NULL)
  }
  
  # reads file as graphml and fails gracefully
  tryCatch({
    ng_rvalues$graph_data <<- igraph::read_graph(infile$datapath, format = c('graphml'))
    
    ng_rvalues$graph_name <<- infile$name
    ng_rvalues$graph_type <<- ifelse("type" %in% graph_attr_names(ng_rvalues$graph_data), 
                                     graph_attr(ng_rvalues$graph_data, "type"), "")
    ng_rvalues$graph_desc <<- "Network loaded from file."
    
    createGraphCategoryList()
    
  }, error = function(err) {
    return(NULL)
  })
})

# apply filters except categorical to graph data and return modified graph
graphFiltersNoCategorical <- reactive({
  g <- NULL

  if (!is.null(ng_rvalues$graph_data)) {
    g <- ng_rvalues$graph_data
    g <- applyPruneFilter(g, pruning_rvalues$prune_verts)
    # isolate as graph_component_type_select has event
    g <- applyComponentFilter(g, isolate(input$graph_component_type_select), input$graph_component_slider)
    g <- applyGraphFilters(g, input$graph_isolates_check, input$graph_multi_edge_check, input$graph_loops_edge_check)
    g <- addAdditionalMeasures(g)
  }

  return(g)
})

# apply all filters to graph data and return modified graph
graphFilters <- reactive({
  g <- NULL

  # during a plot this is triggered 3 times - need to fix at some stage
  
  if (!is.null(ng_rvalues$graph_data)) {
    g <- ng_rvalues$graph_data
    g <- applyPruneFilter(g, pruning_rvalues$prune_verts)
    g <- applyCategoricalFilters(g, input$graph_catAttr_select, input$graph_catAttr_attr_select)
    # isolate as graph_component_type_select has event
    g <- applyComponentFilter(g, isolate(input$graph_component_type_select), input$graph_component_slider)    
    g <- applyGraphFilters(g, input$graph_isolates_check, input$graph_multi_edge_check, input$graph_loops_edge_check)
    g <- addAdditionalMeasures(g)
  }
  
  return(g)
})

# create a list of categories from voson category field names in data
createGraphCategoryList <- reactive({
  g <- ng_rvalues$graph_data
  
  # metadata on categorical attributes
  # note: only vertex attributes
  ng_rvalues$graph_CA <<- list()
  
  attr_v <- vertex_attr_names(g)
  attr_v <- attr_v[grep("^vosonCA", attr_v, perl = T)]
  
  if (length(attr_v)) {
    for (i in attr_v) {
      ng_rvalues$graph_CA[[sapply(strsplit(i, "_"), `[`, 2)]] <<- sort(unique(vertex_attr(g, i)))
    }
  }
})

# only runs on file upload or when collection view graph option selected
setGraphFilterControls <- reactive({
  g <- ng_rvalues$graph_data
  
  # reset pruned list
  pruning_rvalues$prune_verts <<- c()
  updateSelectInput(session, "pruned_vertices_select", choices = character(0))
  
  # clear text analysis plot list
  ta_rvalues$plot_list_data <<- NULL
  
  if (is.null(g)) {
    # disable controls if no data
    disableGraphFilterControls()
    disableTextAnalysisControls()
    
    return(NULL)
  }
  
  if (vcount(g) > 0) {
    # reset and enable graph filter controls
    resetEnableGraphFilterControls()
    
    shinyjs::enable("analysis_graphml_download_button")
    shinyjs::enable("graph_reseed_button")
    shinyjs::enable("graph_component_slider")

    updateComponentSlider(g, isolate(input$graph_component_type_select))
    
    # update the categorical attribute select box
    if (!is.null(ng_rvalues$graph_CA) && length(ng_rvalues$graph_CA) > 0) {
      shinyjs::reset("graph_catAttr_select")
      shinyjs::enable("graph_catAttr_select")
      
      category_choices <- c("All")
      category_choices <- append(category_choices, names(ng_rvalues$graph_CA))
      
      updateSelectInput(session, "graph_catAttr_select", choices = category_choices, selected = "All")
      
      shinyjs::reset("reset_on_change_check")
      shinyjs::enable("reset_on_change_check")
      
    } else {
      shinyjs::reset("graph_catAttr_select")
      shinyjs::disable("graph_catAttr_select")
      
      shinyjs::reset("graph_catAttr_attr_select")
      shinyjs::disable("graph_catAttr_attr_select")   
      
      shinyjs::reset("reset_on_change_check")
      shinyjs::disable("reset_on_change_check")      
    }
    
    # text analysis controls
    if (hasVosonTextData(g)) {
      ta_rvalues$has_text <<- TRUE
      
      # reset and enable text analysis controls
      resetEnableTextAnalysisControls()
      
      # enable or disable text analysis twitter controls
      if (!is.null(get.graph.attribute(g, "type")) && get.graph.attribute(g, "type") == "twitter") {
        shinyjs::enable("text_analysis_twitter_hashtags_check")
        shinyjs::enable("text_analysis_twitter_usernames_check")
      } else {
        shinyjs::disable("text_analysis_twitter_hashtags_check")
        shinyjs::disable("text_analysis_twitter_usernames_check")
      }  
    } else {
      ta_rvalues$has_text <<- FALSE
      
      # disable controls if no text data
      disableTextAnalysisControls()
    }
  }
})

# graph tab specific controls
setGraphTabControls <- reactive({
  g <- ng_rvalues$graph_data
  
  # disable controls
  if (is.null(g)) {
    shinyjs::disable("graph_download_button")
    shinyjs::disable("graph_reseed_button")
    
    return(NULL)
  }
  
  # enable or disable controls based on network graph tab
  switch(input$selected_graph_tab,
         "Plot" = { enablePlotControls() },
         "D3 Simple" = { enableD3Controls() },
         "D3 Force" = { enableD3Controls() },
         "visNetwork" = { enableVisNetworkControls() })
})

# create d3 network from igraph
d3data <- reactive({
  g <- graphFilters()
  
  if (is.null(g) || vcount(g) <= 0) { return(NULL) }
  
  wc <- cluster_walktrap(g)
  members <- membership(wc)
  igraph_to_networkD3(g, group = members)
})

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
                         
  graph_seed <- ng_rvalues$graph_seed

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
    categorical_attributes <- ng_rvalues$graph_CA
    selected_categorical_attribute <- input$graph_catAttr_select
  })
  
  # set default vertex color
  if (nrow(verts) > 0) {
    verts$color.background <- as.character(g_plot_default_vertex_color)
  }
  
  # vertex colours (only if cat attr selected)
  if (length(categorical_attributes) > 0) { # only if have categorical attributes
    
    if (nchar(selected_categorical_attribute) && selected_categorical_attribute != "All") {
      
      categories <- categorical_attributes[[selected_categorical_attribute]]
      df <- data.frame('cat' = categories)
      if (nrow(df) > 0) {
        df$color <- g_plot_palette()[1:nrow(df)]
        verts$color.background <- df$color[match(verts[[selected_categorical_attribute]], df$cat)]
      }
    }
  }
  
  verts$id <- verts$name
  
  verts$font.color <- "#000000"
  if (length(input$dt_vertices_rows_selected) > 0) {
    selected_row_names <- row.names(verts)[c(input$dt_vertices_rows_selected)]
    verts$color.background[row.names(verts) %in% selected_row_names] <- g_plot_selected_vertex_color
    verts$font.color[row.names(verts) %in% selected_row_names] <- g_plot_selected_vertex_color
  }

  edges <- edges %>% group_by(to, from) %>%
    summarise(width = n()) %>% 
    ungroup()
  
  visNetwork::visNetwork(verts, edges, main = NULL) %>% # height = "500px"
    visIgraphLayout(layout = graph_layout, randomSeed = graph_seed) %>%
    visNetwork::visEdges(arrows = 'to',
                         # smooth = list(enabled = TRUE, type = "continuous", roundness = 0.1)
                         color = list(color = "#b0b0b0")) %>% # arrows = 'to, from'
    visOptions(collapse = TRUE, highlightNearest = list(enabled = TRUE, hover = TRUE),
               nodesIdSelection = TRUE, height = ng_rvalues$plot_height) #%>%
    # visInteraction(navigationButtons = TRUE)
})

# network graph save file name based on selected network graph tab
saveGraphFileName <- reactive({
  switch(input$selected_graph_tab,
         "D3 Simple" = systemTimeFilename("d3simple-graph", "html"),
         "D3 Force" = systemTimeFilename("d3force-graph", "html"),
         "visNetwork" = systemTimeFilename("visNetwork-graph", "html"))
})

# network graph data based on selected network graph tab
saveGraphFileData <- reactive({
  data <- switch(input$selected_graph_tab,
         "D3 Simple" = simpleNetworkData(),
         "D3 Force" = forceNetworkData(),
         "visNetwork" = visNetworkData())
  
  if (input$selected_graph_tab == "visNetwork") {
    data$height <- "800px"
    data$sizingPolicy$defaultWidth <- "100%"
    
    data$sizingPolicy$browser$fill <- TRUE
    data$sizingPolicy$viewer$suppress <- TRUE
    data$sizingPolicy$knitr$figure <- FALSE    
  }
  
  data
})

# add selected data table row name values to pruned vertices list
pruneListAddNames <- reactive({
  dt_vertices <- isolate(dt_vertices_df())
  dt_selected_rows <- input$dt_vertices_rows_selected
  prune_list <- pruning_rvalues$prune_verts
  
  selected_rows <- row.names(dt_vertices)[c(dt_selected_rows)]
  # selected_rows <- dt_vertices$name[c(dt_selected_rows)]
  
  # add name if not already in list
  lapply(selected_rows, function(x) {
                          if (!x %in% pruning_rvalues$prune_verts)
                            pruning_rvalues$prune_verts <<- append(pruning_rvalues$prune_verts, x)})
})

# add deselected data table row name values to pruned vertices list
pruneListAddOtherNames <- reactive({
  dt_vertices <- isolate(dt_vertices_df())
  dt_selected_rows <- input$dt_vertices_rows_selected
  prune_list <- pruning_rvalues$prune_verts
  
  # does not let user prune all data this way requires two or more selected rows
  if (length(dt_selected_rows) > 1) {
    selected_rows <- row.names(dt_vertices)[c(dt_selected_rows)]
    
    # names of vertices not selected
    sdf <- subset(dt_vertices, !(row.names(dt_vertices) %in% selected_rows))
    selected_rows <- row.names(sdf)
    
    # add name if not already in list
    lapply(selected_rows, function(x) {
                            if (!x %in% pruning_rvalues$prune_verts) 
                              pruning_rvalues$prune_verts <<- append(pruning_rvalues$prune_verts, x)}) 
  }
})

# graph vertices data as dataframe
dt_vertices_df <- reactive({
  g <- graphFilters()
  
  if (is.null(g)) { return(NULL) }
  
  df_parameters <- list()
  
  df_parameters[['name']] <- V(g)$name
  if (!(is.null(vertex_attr(g, "label")))) {
    df_parameters[['label']] <- V(g)$label
  }  
  df_parameters[['degree']] <- V(g)$Degree
  df_parameters[['indegree']] <- V(g)$Indegree
  df_parameters[['outdegree']] <- V(g)$Outdegree
  df_parameters[['betweenness']] <- V(g)$Betweenness
  df_parameters[['closeness']] <- V(g)$Closeness
  
  attr_v <- vertex_attr_names(g)
  voson_txt_attrs <- attr_v[grep("^vosonTxt", attr_v, perl = T)]
  if (length(voson_txt_attrs)) {
    attr <- voson_txt_attrs[1]
    df_txt_attr <- gsub("vosonTxt_", "", attr, perl = TRUE)
    df_parameters[[df_txt_attr]] <- vertex_attr(g, attr, index = V(g))
  }
  
  voson_cat_attrs <- attr_v[grep("^vosonCA", attr_v, perl = T)]
  if (length(voson_cat_attrs) > 0) {
    for (i in 1:length(voson_cat_attrs)) {
      attr <- voson_cat_attrs[i]
      df_txt_attr <- gsub("vosonCA_", "", attr, perl = TRUE)
      df_parameters[[df_txt_attr]] <- vertex_attr(g, attr, index = V(g))
    }  
  }

  df <- do.call(data.frame, df_parameters)
  
  row.names(df) <- V(g)$id
  
  return(df)
})

# graph edges data as dataframe
dt_edges_df <- reactive({
  g <- graphFilters()
  
  if (is.null(g)) { return(NULL) }
  
  igraph::as_data_frame(g, what = c("edges"))
  # data.frame(name = V(g)$name)
})

# create graph data for a standard network plot
standardPlotData <- reactive({
  g <- graphFilters()

  if (is.null(g)) { return(emptyGraphPlotMessage("No graph data.")) }
  if (vcount(g) <= 0) { return(emptyGraphPlotMessage("No vertices to plot.")) }
  
  # reactive dependencies
  isolate({
    # already dependencies of graphFilters
    categorical_attributes <- ng_rvalues$graph_CA
    selected_categorical_attribute <- input$graph_catAttr_select
  })
  selected_rows <- input$dt_vertices_rows_selected
  # graph_vertices <- as_data_frame(g, what = c("vertices"))
  
  selected_edge_rows <- input$dt_edges_rows_selected
  
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
  
  node_degree_type <- input$graph_node_size_degree_select
  node_size_multiplier <- input$graph_node_size_slider  
  chosen_layout <- input$graph_layout_select
  graph_seed <- ng_rvalues$graph_seed
  graph_spread <- input$graph_spread_slider
  
  # set default vertex color
  V(g)$color <- as.character(g_plot_default_vertex_color)
  
  # vertex colours (only if cat attr selected)
  if (length(categorical_attributes) > 0) { # only if have categorical attributes
    
    if (nchar(selected_categorical_attribute) && selected_categorical_attribute != "All") {
      
      categories <- categorical_attributes[[selected_categorical_attribute]]
      df <- data.frame('cat' = categories)
      if (nrow(df) > 0) {
        df$color <- g_plot_palette()[1:nrow(df)]
        
        va <- paste0('vosonCA_', selected_categorical_attribute)
        V(g)$color <- df$color[match(vertex_attr(g, va), df$cat)]
      }
    }
  }
  
  selected_row_names <- c()
  if (length(selected_rows) > 0) {
    selected_row_names <- row.names(graph_vertices)[c(selected_rows)]
  }
  
  # if (length(selected_edge_rows) > 0) {
  #   E(g)$color g_plot_selected_vertex_color
  # }
  
  plot_parameters <- list(g, vertex.frame.color = "gray", edge.arrow.size = 0.4)
  
  # set vertex color for vertices selected in graph data table
  plot_parameters[['vertex.color']] <- ifelse(V(g)$id %in% selected_row_names, g_plot_selected_vertex_color, V(g)$color)
  plot_parameters[['vertex.label.font']] <- ifelse(V(g)$id %in% selected_row_names, 2, 1)
  # plot_parameters[['vertex.label.cex']] = ifelse(V(g)$id %in% selected_row_names, 4, 1)
  
  # E(g)$weight <- 1
  # g <- simplify(g, edge.attr.comb = list(weight = "sum"), 
  #               remove.loops = FALSE, # !input$graph_loops_edge_check,
  #               remove.multiple = FALSE) # !input$graph_multi_edge_check)
  # plot_parameters[['edge.width']] <- E(g)$weight
  
  base_vertex_size <- 4
  
  if (node_degree_type == "None") {
    if (node_size_multiplier > 1) {
      plot_parameters['vertex.size'] <- base_vertex_size + (node_size_multiplier / 4)
    } else {
      plot_parameters['vertex.size'] <- base_vertex_size
    }
  } else {
    # todo: needs to calculate average values to better adjust scale
    plot_parameters[['vertex.size']] <- switch(node_degree_type,
                                   "Degree" = (V(g)$Degree / 4 * node_size_multiplier) + base_vertex_size,
                                   "Indegree" = (V(g)$Indegree / 2 * node_size_multiplier) + base_vertex_size,
                                   "Outdegree" = (V(g)$Outdegree / 2 * node_size_multiplier) + base_vertex_size,
                                   "Betweenness" = (V(g)$Betweenness / 100 * node_size_multiplier) + base_vertex_size,
                                   "Closeness" = (V(g)$Closeness * 100 * node_size_multiplier) + base_vertex_size
    )
  }
  
  # avoid unknown font warnings on windows by setting TT font
  if (.Platform$OS.type != "unix") {
    windowsFonts(Arial = windowsFont("TT Arial"))
  }
  
  plot_parameters['vertex.label.family'] <- "Arial"
  plot_parameters['vertex.label.cex'] <- 0.9
  plot_parameters['vertex.label.dist'] <- 1.4

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
  plot_parameters[['vertex.label.color']] = ifelse(V(g)$id %in% selected_row_names, g_plot_selected_vertex_color, 
                                                   g_plot_default_label_color)

  # reproduce same graph with same seed
  # must be set before graph layout
  if (!is.null(graph_seed)) {
    set.seed(graph_seed)
  }
  
  graph_layout <- switch(chosen_layout,
                         "Auto" = layout_nicely(g, dim = 2),
                         "Fruchterman-Reingold" = layout_with_fr(g, dim = 2, niter = 1000),
                         "Kamada-Kawai" = layout_with_kk(g, dim = 2),
                         "Davidson-Harel" = layout_with_dh(g),
                         "Large Graph Layout" = layout_with_lgl(g),
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
  do.call(plot, plot_parameters)
  
})

# d3 simple network graph
simpleNetworkData <- reactive({
  network_d3 <- d3data()
  
  if (is.null(network_d3)) { return(NULL) }
  
  dfr <- network_d3$links
  dfr = dfr - 1
  
  simpleNetwork(dfr, Source = "source", Target = "target",
                fontSize = 10, fontFamily = "arial", opacity = 1)
})

# d3 force network graph
forceNetworkData <- reactive({
  network_d3 <- d3data()
  
  if (is.null(network_d3)) { return(NULL) }
  
  dfr <- network_d3$links
  #dfr = dfr-1
  
  forceNetwork(Links = dfr,
               Nodes =network_d3$nodes, NodeID = "name",
               Group ="group", Source = "source", Target = "target",
               fontSize = 16, fontFamily = "arial",
               bounded = TRUE, zoom = TRUE, opacity = 1)
})

# graph summary
graphSummaryOutput <- reactive({
  g <- graphFilters()
  
  output <- c()
  
  if (!is.null(g)) {
    graph_clusters <- components(g, mode = input$graph_component_type_select)
    
    output <- append(output, paste0("Components (", input$graph_component_type_select, "): ", graph_clusters$no))
    
    min_value <- max_value <- 0
    if (graph_clusters$no > 0) {
      # suppress no non-missing arguments to min; returning Inf warning
      min_value <- suppressWarnings(min(graph_clusters$csize))
      max_value <- suppressWarnings(max(graph_clusters$csize))
    }
    
    if (graph_clusters$no == 1) {
      output <- append(output, paste0("Size: ", min_value, sep = ""))
    } else {
      output <- append(output, paste0("Size min: ", min_value, " max: ", max_value, sep = ""))
    }
    
    output <- append(output, "")
    
    output <- append(output, paste0("Nodes: ", vcount(g)))
    output <- append(output, paste0("Edges: ", ecount(g)))
    
    isolate_count <- sum(degree(g) == 0)
    output <- append(output, paste0("Isolates: ", isolate_count))
  }else {
    output <- append(output, paste0(""))
  }
  
  paste0(output, collapse = '<br>') # \n
})

#### functions -------------------------------------------------------------------------------------------------------- #

# set graph manually
setGraphView <- function(data, desc = "", type = "", name = "", seed = 1) {
  shinyjs::reset("graphml_data_file")
  
  ng_rvalues$graph_data <<- data
  ng_rvalues$graph_desc <<- desc
  ng_rvalues$graph_type <<- type
  ng_rvalues$graph_name <<- name
  ng_rvalues$graph_seed <<- seed
  ng_rvalues$graph_CA <<- c()
  ng_rvalues$graph_CA_selected <<- ""
  
  createGraphCategoryList()
  setGraphFilterControls()
  #createGraphCategoryList()
  updateTabItems(session, "sidebar_menu", selected = "network_graphs_tab")
}

# return empty plot with message
emptyGraphPlotMessage <- function(message) {
  return({ plot(1:10, 1:10, type = "n", axes = F, xlab = "", ylab = "")
    text(5, 5, message, cex = 1.2) })
}

# filter out vertices not in selected voson categories from graph object
applyCategoricalFilters <- function(g, selected_category, selected_category_attr) {
  # filter out all vertices that are not in category attribute
  if (selected_category != "All") {
    selected_category_attr <- selected_category_attr[selected_category_attr != "All"]
    
    if (length(selected_category_attr) > 0) {
      vattr <- paste0('vosonCA_', selected_category)
      g <- delete.vertices(g, V(g)[!(vertex_attr(g, vattr) %in% selected_category_attr)])
    }
  }
  
  return(g)
}

updateComponentSlider <- function(g, component_type) {
  if (!is.null(g)) {
    graph_clusters <- components(g, mode = component_type)
    
    # suppress no non-missing arguments to min; returning Inf warning
    min_cluster_size <- suppressWarnings(min(graph_clusters$csize))
    max_cluster_size <- suppressWarnings(max(graph_clusters$csize))
    
    # likely causes a double render when graph has a component max size greater than the initial slider max set in ui
    updateSliderInput(session, inputId = "graph_component_slider", min = min_cluster_size,
                      max = max_cluster_size, value = c(min_cluster_size, max_cluster_size))
  }
}

applyComponentFilter <- function(g, component_type, component_range) {
  graph_clusters <- components(g, mode = component_type)
  
  min_cluster_size <- suppressWarnings(min(graph_clusters$csize)) # suppress no non-missing arguments to min;
  max_cluster_size <- suppressWarnings(max(graph_clusters$csize)) # returning Inf warning
  
  component_slider_min_value <- component_range[1]
  component_slider_max_value <- component_range[2]
  
  filter_nodes_under <- NULL
  filter_nodes_over <- NULL
  rm_nodes <- c()
  
  # remove vertices not part of components in component size range
  if (component_slider_min_value > min_cluster_size) {
    filter_nodes_under <- names(which(table(graph_clusters$membership) < component_slider_min_value))
    
    if (length(filter_nodes_under) > 0) {
      rm_nodes <- sapply(filter_nodes_under, function(x) append(rm_nodes, names(which(graph_clusters$membership == x))))
    }
  }
  
  if (component_slider_max_value < max_cluster_size) {
    filter_nodes_over <- names(which(table(graph_clusters$membership) > component_slider_max_value))
    
    if (length(filter_nodes_over) > 0) {
      rm_nodes <- sapply(filter_nodes_over, function(x) append(rm_nodes, names(which(graph_clusters$membership == x))))
    }
  }
  
  if (length(rm_nodes) > 0) {
    rm_nodes <- unlist(rm_nodes)
    g <- delete.vertices(g, rm_nodes)
  }
  
  return(g)
}

# filter out vertices and edges from graph object
applyGraphFilters <- function(g, isolates, multi_edge, loops_edge) {
  # remove multiple edges and self loops
  if (multi_edge == FALSE || loops_edge == FALSE) {
    remove_multiple <- ifelse(multi_edge == FALSE, TRUE, FALSE)
    remove_loops <- ifelse(loops_edge == FALSE, TRUE, FALSE)
    g <- simplify(g, remove.multiple = remove_multiple, remove.loops = remove_loops)
  }
  
  # remove isolates
  if (isolates == FALSE) {
    g <- delete.vertices(g, degree(g) == 0)
  }
  
  return(g)  
}

# filter out list of vertices from graph object
applyPruneFilter <- function(g, selected_prune_verts) {
  if (length(selected_prune_verts) > 0) {
    verts <- which(V(g)$id %in% selected_prune_verts)
    g <- delete.vertices(g, verts) # selected_prune_verts
  }
  
  # toggle flag even if did not prune
  if (prune_flag == TRUE) {
    prune_flag <<- FALSE
  }
  
  return(g)
}

# add additional calculated data to graph object
# if column already exists in the data it will be overwritten
addAdditionalMeasures <- function(g) {
  # add degree
  V(g)$Degree <- degree(g, mode = "total")
  if (is.directed(g)) {
    V(g)$Indegree <- degree(g, mode = "in")
    V(g)$Outdegree <- degree(g, mode = "out")
  } else {
    V(g)$Indegree <- V(g)$Outdegree <- 0
  }
  
  # add centrality
  if (vcount(g) > 1) {
    V(g)$Betweenness <- as.numeric(sprintf("%.3f", betweenness(g)))
    V(g)$Closeness <- as.numeric(sprintf("%.3f", suppressWarnings(closeness(g)))) # suppress disconnected graph warnings    
  } else {
    V(g)$Betweenness <- V(g)$Closeness <- 0
  }

  return(g)
}

#### batch reset, enable and disable graph contols ####

# need to modularize
disableGraphFilterControls <- function() {
  ui_controls <- c("graph_isolates_check",
                   "graph_multi_edge_check",
                   "graph_loops_edge_check",
                   "graph_names_check",
                   "graph_catAttr_select",
                   "graph_catAttr_attr_select",
                   "graph_node_size_degree_select", 
                   "analysis_graphml_download_button",
                   "graph_reseed_button",
                   "graph_layout_select", 
                   "graph_spread_slider",
                   "graph_component_type_select",
                   "graph_component_slider")
  
  sapply(ui_controls, function(x) { shinyjs::disable(x) })  
}

resetEnableGraphFilterControls <- function() {
  ui_controls <- c("graph_isolates_check", 
                   "graph_multi_edge_check", 
                   "graph_loops_edge_check", 
                   "graph_names_check", 
                   "graph_node_size_degree_select", 
                   "graph_catAttr_attr_select", 
                   "graph_layout_select", 
                   "graph_spread_slider",
                   "graph_component_type_select")
  
  sapply(ui_controls, function(x) { shinyjs::reset(x)
                                    shinyjs::enable(x) })
}

disableTextAnalysisControls <- function() {
  ui_controls <- c("text_analysis_stopwords_check",
                   "text_analysis_user_stopwords_input", 
                   "text_analysis_user_stopwords_check",
                   "text_analysis_twitter_hashtags_check", 
                   "text_analysis_twitter_usernames_check",
                   "text_analysis_stem_check",
                   "text_analysis_wf_top_count",
                   "text_analysis_wf_min_word_freq",
                   "text_analysis_wc_min_word_freq",
                   "text_analysis_wc_max_word_count",
                   "text_analysis_cc_max_word_count")
  
  sapply(ui_controls, function(x) { shinyjs::disable(x) })
}

resetEnableTextAnalysisControls <- function() {
  ui_controls <- c("text_analysis_stopwords_check",
                   "text_analysis_user_stopwords_input", 
                   "text_analysis_user_stopwords_check",
                   "text_analysis_twitter_hashtags_check",
                   "text_analysis_twitter_usernames_check", 
                   "text_analysis_stem_check",
                   "text_analysis_wf_top_count",
                   "text_analysis_wf_min_word_freq",
                   "text_analysis_wc_min_word_freq",
                   "text_analysis_wc_max_word_count",
                   "text_analysis_cc_max_word_count")
  
  sapply(ui_controls, function(x) { shinyjs::reset(x)
                                    shinyjs::enable(x) })
}

enablePlotControls <- function() {
  shinyjs::disable("graph_download_button")
  
  # added "graph_multi_edge_check", "graph_loops_edge_check" for bug switching back to plot from visnetwork
  # and multi, loops checkbox remaining disabled
  ui_controls <- c("graph_names_check",
                   "graph_reseed_button",
                   "graph_layout_select",
                   "graph_node_size_degree_select",
                   "graph_node_size_slider",
                   "graph_spread_slider",
                   "graph_multi_edge_check",
                   "graph_loops_edge_check")
  
  sapply(ui_controls, function(x) { shinyjs::enable(x) })
}

enableD3Controls <- function() {
  shinyjs::enable("graph_download_button")
  
  ui_controls <- c("graph_names_check",
                   "graph_reseed_button",
                   "graph_layout_select",
                   "graph_node_size_degree_select",
                   "graph_node_size_slider",
                   "graph_spread_slider")
  
  sapply(ui_controls, function(x) { shinyjs::disable(x) })
}

enableVisNetworkControls <- function() {
  shinyjs::enable("graph_download_button")
  
  ui_controls <- c("graph_names_check", 
                   "graph_multi_edge_check", 
                   "graph_loops_edge_check", 
                   "graph_spread_slider")
  
  sapply(ui_controls, function(x) { shinyjs::disable(x) })
}
