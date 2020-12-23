#' VOSON Dashboard networkGraphsServer
#'
#' Network data, measures, filters and graph visualisations.
#'

#### values ---------------------------------------------------------------------------------------------------------- #

ng_rv <- reactiveValues(
  data = NULL,       # vosonsml df
  graph_data = NULL, # igraph graph object
  graph_seed = NULL, # plot seed value
  
  graph_desc = "",   # some graph attributes
  graph_name = "",
  graph_type = "",
  
  graph_dir = TRUE, # directed
  
  graph_cats = c(),        # list of categories in the data # graph_CA
  graph_cat_selected = "", # selected category # graph_CA_selected
  
  plot_height = gbl_plot_height,
  
  prune_verts = c()
)

dt_prev_sel <- reactiveValues(
  nodes = c()  
)

# proxy for vertices data table used for row manipulation
dt_vertices_proxy <- dataTableProxy('dt_vertices')

# disable network metrics and assortativity tabs when app loads
addCssClass(selector = "a[data-value = 'network_metrics_tab']", class = "inactive_menu_link")
addCssClass(selector = "a[data-value = 'assortativity_tab']", class = "inactive_menu_link")

source("server/controls.R", local = TRUE)

#### events ---------------------------------------------------------------------------------------------------------- #

# set reactive value plot height when height input changes
observeEvent(input$plot_height, {
  ng_rv$plot_height <- input$plot_height
}, ignoreInit = TRUE)

# create list of demo files found in extdata - do once at startup
check_demo_files <- TRUE
observeEvent(check_demo_files, {
  tryCatch({
    demo_files_list <- list.files(path = system.file("extdata", "", package = "VOSONDash", mustWork = TRUE),
                                  pattern = "\\.graphml$")
      
    if (length(demo_files_list) > 0) {
      demo_files_list <- lapply(demo_files_list, function(x) gsub("\\.graphml$", "", x, ignore.case = TRUE))
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

# load demo data button event
observeEvent(input$demo_data_select_button, {
  load_file <- system.file("extdata", paste0(input$demo_data_select, ".graphml"), package = "VOSONDash")

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
      type <- ifelse("type" %in% graph_attr_names(data), graph_attr(data, "type"), "")
      
      setGraphView(data = data,
                   desc = file_desc,
                   type = type,
                   name = input$demo_data_select,
                   seed = sample(gbl_rng_range[1]:gbl_rng_range[2], 1))
    }, error = function(err) {
      # cat(paste("error loading demo files:", err))
    }, warning = function(w) {
      # cat(paste("warning loading demo files:", w))
    })
  }
})

# when graphml data loaded or changed
observeEvent(ng_rv$graph_data, {
  if (!is.null(ng_rv$graph_data)) {
    
    # add vertex ids and labels if not present
    attr_v <- vertex_attr_names(ng_rv$graph_data)
    if (!("id" %in% attr_v)) {
      V(ng_rv$graph_data)$id <- paste0("n", as.numeric(V(ng_rv$graph_data))-1) # n0, n1 ..
    }
    
    if ("label" %in% attr_v) {
      # replace empty string labels
      V(ng_rv$graph_data)$label <- ifelse(nchar(V(ng_rv$graph_data)$label) > 0, 
                                               V(ng_rv$graph_data)$label, "-")
    } else {
      # if no labels set label to vertex name
      V(ng_rv$graph_data)$label <- ifelse(nchar(V(ng_rv$graph_data)$name) > 0,
                                               V(ng_rv$graph_data)$name, "-")
    }
    
    # set directed
    isolate({ ng_rv$graph_dir <- igraph::is_directed(ng_rv$graph_data) })
    
    # enable network metrics tab
    removeCssClass(selector = "a[data-value = 'network_metrics_tab']", class = "inactive_menu_link")
  }
})

# enable assortativity tab when a category other than "all" selected
observeEvent(ng_rv$graph_cat_selected, {
  if (ng_rv$graph_cat_selected %in% c("", "All")) {
    addCssClass(selector = "a[data-value = 'assortativity_tab']", class = "inactive_menu_link")
  } else {
    removeCssClass(selector = "a[data-value = 'assortativity_tab']", class = "inactive_menu_link")
  }
})

# ** check this is not redundant
# update component slider when graph component or category changed
observeEvent({ input$graph_component_type_select
               input$graph_sub_cats_select
               ng_rv$prune_verts
               input$reset_on_change_check }, {
  
  g <- ng_rv$graph_data
  g <- applyPruneFilterSrv(g, ng_rv$prune_verts)
  
  if (input$reset_on_change_check == TRUE) {
    g <- applyCategoricalFilters(g, input$graph_cat_select, input$graph_sub_cats_select)
  }

  updateComponentSlider(g, input$graph_component_type_select)
}, ignoreInit = TRUE)
  
# selected category updates select box with its attribute values
observeEvent(input$graph_cat_select, {
  ng_rv$graph_cat_selected <- input$graph_cat_select
  
  if (!is.null(ng_rv$graph_data)) {
    attr_choices <- c("All")
    
    if (input$graph_cat_select != "All") {
      attr_choices <- append(attr_choices, ng_rv$graph_cats[[input$graph_cat_select]])
    }
    
    # update list of values in select box
    updateSelectInput(session, "graph_sub_cats_select", choices = attr_choices, selected = "All")
    
    # enable select box control
    shinyjs::enable("graph_sub_cats_select")
  }
})

# set graph controls on graph tab changes
observeEvent(input$selected_graph_tab, {
  setGraphTabControls()
})

# graphml file uploaded
observeEvent(input$graphml_data_file, {
  filedata()
  
  # set a random number to seed plots
  ng_rv$graph_seed <- sample(gbl_rng_range[1]:gbl_rng_range[2], 1)
  
  # reset controls and filters
  setGraphTabControls()
  setGraphFilterControls()
})

# generate a new random seed on reseed button event
observeEvent(input$graph_reseed_button, {
  ng_rv$graph_seed <- sample(gbl_rng_range[1]:gbl_rng_range[2], 1)
})

observeEvent(input$node_index_check, {
  if (input$node_index_check) {
    updateCheckboxInput(session, "node_labels_check", value = FALSE)
  }
})

observeEvent(input$node_labels_check, {
  if (input$node_labels_check) {
    updateCheckboxInput(session, "node_index_check", value = FALSE)
  }  
})

observeEvent(ng_rv$graph_seed, {
  html("seed", ng_rv$graph_seed)
})

#### graph vertex pruning ####

# add selected data table rows to pruned vertices list
observeEvent(input$prune_selected_rows_button, {
  # this updates prune list and triggers graph redraw
  pruneListAddNames()
  
  # update prune list select box
  prune_list <- isolate(ng_rv$prune_verts)
  if (is.null(prune_list)) { 
    prune_list <- character(0) 
  } else {
    temp <- list()
    for (i in prune_list) {
      n_value <- V(isolate(ng_rv$graph_data))[which(V(isolate(ng_rv$graph_data))$id == i)]$label # name
      temp[paste0(i, " - ", n_value)] <- i
    }
    prune_list <- temp
  }
  updateSelectInput(session, "pruned_vertices_select", choices = prune_list)
})

# add unselected data table rows to pruned vertices list
observeEvent({ input$prune_unselected_rows_button
               input$nbh_prune_unselected }, {
  pruneListAddOtherNames()
  
  # update prune list select box
  prune_list <- isolate(ng_rv$prune_verts)
  if (is.null(prune_list)) { 
    prune_list <- character(0) 
  } else {
    temp <- list()
    for (i in prune_list) {
      n_value <- V(isolate(ng_rv$graph_data))[which(V(isolate(ng_rv$graph_data))$id == i)]$name
      temp[paste0(i, " - ", n_value)] <- i
    }
    prune_list <- temp
  }
  updateSelectInput(session, "pruned_vertices_select", choices = prune_list)
  # DT::selectRows(dt_vertices_proxy, rownames(graphNodes()))
})

# remove selected vertices from prune list
observeEvent(input$prune_return_button, {
  ng_rv$prune_verts <- ng_rv$prune_verts[!(ng_rv$prune_verts %in% input$pruned_vertices_select)]
  
  # update prune list select box
  prune_list <- isolate(ng_rv$prune_verts)
  if (is.null(prune_list)) { 
    prune_list <- character(0) 
  } else {
    temp <- list()
    for (i in prune_list) {
      n_value <- V(isolate(ng_rv$graph_data))[which(V(isolate(ng_rv$graph_data))$id == i)]$name
      temp[paste0(i, " - ", n_value)] <- i
    }
    prune_list <- temp
  }
  updateSelectInput(session, "pruned_vertices_select", choices = prune_list)
})

# reset prune list
observeEvent({ input$prune_reset_button 
               input$nbh_reset_button }, {
  ng_rv$prune_verts <- c()
  
  updateSelectInput(session, "pruned_vertices_select", choices = character(0))
  
  # added to address bug with disappearing plot on pruning
  updateComponentSlider(ng_rv$graph_data, input$graph_component_type_select)  
})

# deselect all data table selected rows
observeEvent({ input$prune_deselect_rows_button 
               input$nbh_deselct_button }, { DT::selectRows(dt_vertices_proxy, NULL) })

# nodes clicked event in visnetwork
observeEvent(input$vis_node_select, {
  dt_vertices <- isolate(graphNodes())
  
  selected_rows <- row.names(dt_vertices)[c(input$dt_vertices_rows_selected)] # selected in dt
  plot_sel_nodes <- row.names(dt_vertices)[dt_vertices$name %in% input$vis_node_select] # selected in plot
  
  deselect_nodes <- plot_sel_nodes[plot_sel_nodes %in% selected_rows] # deselect if already selected in dt
  all_selected <- union(selected_rows, plot_sel_nodes)
  
  sel <- all_selected[!all_selected %in% deselect_nodes]
  sel <- which(rownames(dt_vertices) %in% sel) # require indices not row names
  
  DT::selectRows(dt_vertices_proxy, sel)
})

# observeEvent(input$vis_nbh_node_select, {
#   g <- graphFilters()
#   dt_vertices <- graphNodes()
#   plot_sel_nodes <- row.names(dt_vertices)[dt_vertices$name %in% input$vis_nbh_node_select]
#   DT::selectRows(dt_vertices_proxy, which(rownames(dt_vertices) %in% plot_sel_nodes))
#   sel_rows <- row.names(dt_vertices)[c(input$dt_vertices_rows_selected)]
#   dt_prev_sel$nodes <- sel_rows
#   shinyjs::enable("nbh_undo_button")
#   sel_row_names <- V(g)[V(g)$id %in% sel_rows]$name
#   order <- 1
#   g_ego <- make_ego_graph(g, order = order, nodes = sel_row_names, mode = "all", mindist = 0)
#   ids <- unlist(sapply(g_ego, function(x) V(x)$id))
#   sel <- which(rownames(dt_vertices) %in% ids)
#   DT::selectRows(dt_vertices_proxy, sel)  
# })

observeEvent(input$nbh_select_button, {
  if (length(input$dt_vertices_rows_selected) < 1) { return() }
  g <- graphFilters()
  dt_vertices <- graphNodes()
  sel_rows <- row.names(dt_vertices)[c(input$dt_vertices_rows_selected)]
  
  dt_prev_sel$nodes <- sel_rows
  shinyjs::enable("nbh_undo_button")
  
  sel_row_names <- V(g)[V(g)$id %in% sel_rows]$name
  
  order <- input$nbh_order_select
  g_ego <- make_ego_graph(g, order = order, nodes = sel_row_names, mode = "all", mindist = 0)
  ids <- unlist(sapply(g_ego, function(x) V(x)$id))
  sel <- which(rownames(dt_vertices) %in% ids)
  
  DT::selectRows(dt_vertices_proxy, sel)
})

observeEvent(input$nbh_undo_button, {
  if (length(dt_prev_sel$nodes) > 0) {
    DT::selectRows(dt_vertices_proxy, NULL)
    sel <- which(rownames(graphNodes()) %in% dt_prev_sel$nodes)
    DT::selectRows(dt_vertices_proxy, sel)
    
    dt_prev_sel$nodes <- c()
    shinyjs::disable("nbh_undo_button")
  }
})

# reset node size slider when changed to none
observeEvent(input$graph_node_size_select, {
  if (input$graph_node_size_select == "None") { shinyjs::reset("graph_node_size_slider") }
})

# on change layout event
observeEvent(input$graph_layout_select, {
  shinyjs::reset("graph_spread_slider") # reset graph spread when a new layout is selected
  
  if (input$graph_layout_select == "Graphopt") { 
    shinyjs::reset("graph_charge")
    shinyjs::reset("graph_mass")
    shinyjs::reset("graph_spr_len")
    shinyjs::reset("graph_spr_const")
  }
})

#### output ---------------------------------------------------------------------------------------------------------- #

output$plot_height_ui <- renderUI({
  tagList(div(div(
    div(selectInput("plot_height", label = NULL, 
                    choices = c("300px" = 300, "400px" = 400, "500px" = 500, "600px" = 600, "700px" = 700, 
                                "800px" = 800, "900px" = 900, "1000px" = 1000), 
                    multiple = FALSE, selectize = FALSE, selected = ng_rv$plot_height), 
        style = "width:100%;", align = "right"),
    style = "position:absolute; z-index:1; top:60px; right:40px; font-size:0.97em;"),
    style = "position:relative; z-index:0;"))
})

output$graph_summary_ui <- renderUI({
  tagList(div(div(
    HTML(graphSummaryOutput()),
    style = paste0("position:absolute; z-index:1; top:", (as.numeric(ng_rv$plot_height)-5), 
                   "px; left:18px; font-size:0.97em;")),
    style = "position:relative; z-index:0;"))
})

output$graph_legend_ui <- renderUI({
  tagList(div(div(
    HTML(graphLegendOutput()),
    style = paste0("position:absolute; z-index:1; top:", 85, # (as.numeric(ng_rv$plot_height)-5), 
                   "px; left:18px; font-size:0.97em;")),
    style = "position:relative; z-index:0;"))
})

output$vis_plot_ui <- renderUI({
  tabBox(width = 12, title = span(icon("share-alt", class = "social_green"), "Network Graphs"), 
         selected = input$selected_graph_tab, id = "selected_graph_tab",
         tabPanel("igraph", plotOutput("igraphPlot", width = "100%", height = "auto"), value = "Plot"),
         tabPanel("visNetwork", visNetworkOutput("visNetworkPlot", width = "100%",
                                                 height = paste0(ng_rv$plot_height, "px")), value = "visNetwork")
  )
})

output$component_summary_ui <- renderText({
  graphComponentSummary()
})

# output$graph_summary_output <- renderText({
#   graphSummaryOutput()
# })

output$graph_name <- renderText({
  output <- ifelse(nchar(ng_rv$graph_name), ng_rv$graph_name, "Not set")
  output <- paste("Name: ", output)
  
  if (nchar(ng_rv$graph_type)) {
    output <- paste0(output, "  (Type: ", ng_rv$graph_type, ")")
  }
  
  return(output)
})

output$graph_desc <- renderText({
  if (nchar(ng_rv$graph_desc)) {
    return(HTML(ng_rv$graph_desc))
  }
  
  return(HTML("No description."))
})

observeEvent(ng_rv$graph_desc, {
  if (!isNullOrEmpty(ng_rv$graph_desc)) {
    shinyjs::enable("expand_data_desc_check")
  } else {
    shinyjs::disable("expand_data_desc_check")
  }
})

# graph download buttons
output$graph_download_button <- downloadHandler(
  filename = function() { saveGraphFileName() },
  
  content = function(file) {
    if (input$selected_graph_tab == "visNetwork") {
      visSave(saveGraphFileData(), file, selfcontained = TRUE, background = "white")
    } else {
      saveNetwork(saveGraphFileData(), file, selfcontained = TRUE) 
    }
  }
)

# analysis graphml download button
output$analysis_graphml_download_button <- downloadHandler(
  filename = function() { systemTimeFilename("analysis-graph", "graphml") },
  content = function(file) { write_graph(graphFilters(), file, format = c("graphml")) }
)

# graph vertices data table
output$dt_vertices <- DT::renderDataTable({
  data <- graphNodes()
  
  # truncate text in column cells
  col_defs <- NULL
  if (input$graph_dt_v_truncate_text_check == TRUE) {
    col_defs <- gbl_dt_col_defs
    col_defs[[1]]$targets <- "_all"
  }
  
  if (!is.null(data)) {
    dt <- DT::datatable(data, extensions = 'Buttons', filter = "top",
                        options = list(lengthMenu = gbl_dt_menu_len, pageLength = gbl_dt_page_len, scrollX = TRUE,
                        columnDefs = col_defs, dom = 'lBfrtip', buttons = c('copy', 'csv', 'excel', 'print')),
                        class = 'cell-border stripe compact hover')

    # format betweeness and closeness values to display 3 decimal places
    if (nrow(data) > 0) {
      DT::formatRound(dt, columns = c('betweenness', 'closeness'), digits = 3)
    } else { dt }
  }
})

# graph edges data table
output$dt_edges <- DT::renderDataTable({
  data <- graphEdges()
  
  # truncate text in column cells
  col_defs <- NULL
  if (input$graph_dt_e_truncate_text_check == TRUE) {
    col_defs <- gbl_dt_col_defs
    col_defs[[1]]$targets <- "_all"
  }
  
  if (!is.null(data)) {
    DT::datatable(data, extensions = 'Buttons', filter = "top", selection = "none", # rows not selectable
                  options = list(lengthMenu = gbl_dt_menu_len, pageLength = gbl_dt_page_len, scrollX = TRUE,
                  columnDefs = col_defs, dom = 'lBfrtip',
                  buttons = c('copy', 'csv', 'excel', 'print')), class = 'cell-border stripe compact hover')
  }
})

output$igraphPlot <- renderPlot({
  igraphData()
}, height = function() { as.numeric(ng_rv$plot_height) })

output$visNetworkPlot <- renderVisNetwork({
  visNetworkData()
})

#### reactives -------------------------------------------------------------------------------------------------------- #

source("server/igraphPlot.R", local = TRUE)
source("server/visnetworkPlot.R", local = TRUE)

# set file data when a file is uploaded
filedata <- reactive({
  infile <- input$graphml_data_file
  
  if (is.null(infile)) { return(NULL) }
  
  # reads file as graphml and fails gracefully
  tryCatch({
    ng_rv$graph_data <- igraph::read_graph(infile$datapath, format = c('graphml'))
    ng_rv$graph_name <- infile$name
    ng_rv$graph_type <- ifelse("type" %in% graph_attr_names(ng_rv$graph_data), 
                                     graph_attr(ng_rv$graph_data, "type"), "")
    ng_rv$graph_desc <- "Network loaded from file."
    
    createGraphCategoryList()
    
    updateCheckboxInput(session, "expand_demo_data_check", value = FALSE)
  }, error = function(err) { return(NULL) })
})

# apply all filters to graph data and return modified graph
graphFilters <- reactive({
  g <- NULL

  if (!is.null(ng_rv$graph_data)) {
    g <- ng_rv$graph_data
    
    # ----
    # add vertex ids and labels if not present
    attr_v <- vertex_attr_names(g)
    if (!("id" %in% attr_v)) {
      V(g)$id <- paste0("n", as.numeric(V(g))-1) # n0, n1 ..
    }
    
    if ("label" %in% attr_v) {
      # replace empty string labels
      V(g)$label <- ifelse(nchar(V(g)$label) > 0, V(g)$label, "-")
    } else {
      # if no labels set label to vertex name
      V(g)$label <- ifelse(nchar(V(g)$name) > 0, V(g)$name, "-")
    }    
    # ----
    
    g <- applyPruneFilterSrv(g, ng_rv$prune_verts)
    g <- applyCategoricalFilters(g, input$graph_cat_select, input$graph_sub_cats_select)
    # isolate as graph_component_type_select has event
    g <- applyComponentFilter(g, isolate(input$graph_component_type_select), input$graph_component_slider)    
    g <- applyGraphFilters(g, input$graph_isolates_check, input$graph_multi_edge_check, 
                                      input$graph_loops_edge_check)
    g <- addAdditionalMeasures(g)
    
    # enable network metrics tab
    removeCssClass(selector = "a[data-value = 'network_metrics_tab']", class = "inactive_menu_link")
  }
  
  return(g)
})

# create a list of categories from voson vertex category field names in data
createGraphCategoryList <- reactive({
  ng_rv$graph_cats <- getVertexCategories(ng_rv$graph_data)  
})

# only runs on file upload or when collection view graph option selected
setGraphFilterControls <- reactive({
  g <- ng_rv$graph_data
  
  # reset pruned list
  ng_rv$prune_verts <- c()
  updateSelectInput(session, "pruned_vertices_select", choices = character(0))
  
  # clear text analysis plot list
  ta_rv$plot_data_list <- NULL
  
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
    
    dt_prev_sel$nodes <- c()
    shinyjs::reset("nbh_undo_button")
    
    # update the categorical attribute select box
    if (!is.null(ng_rv$graph_cats) && length(ng_rv$graph_cats) > 0) {
      shinyjs::reset("graph_cat_select")
      shinyjs::enable("graph_cat_select")
      
      category_choices <- c("All")
      category_choices <- append(category_choices, names(ng_rv$graph_cats))
      
      updateSelectInput(session, "graph_cat_select", choices = category_choices, selected = "All")
      
      shinyjs::reset("reset_on_change_check")
      shinyjs::enable("reset_on_change_check")
      
    } else {
      shinyjs::reset("graph_cat_select")
      shinyjs::disable("graph_cat_select")
      
      shinyjs::reset("graph_sub_cats_select")
      shinyjs::disable("graph_sub_cats_select")   
      
      shinyjs::reset("reset_on_change_check")
      shinyjs::disable("reset_on_change_check")      
    }
    
    # text analysis controls
    if (hasVosonTextData(g)) {
      ta_rv$has_text <- TRUE
      
      # reset and enable text analysis controls
      resetEnableTextAnalysisControls()
      
      # enable or disable text analysis twitter controls
      if (!is.null(get.graph.attribute(g, "type")) && get.graph.attribute(g, "type") == "twitter") {
        shinyjs::enable("ta_twitter_hashtags_check")
        shinyjs::enable("ta_twitter_usernames_check")
      } else {
        shinyjs::disable("ta_twitter_hashtags_check")
        shinyjs::disable("ta_twitter_usernames_check")
      }  
    } else {
      ta_rv$has_text <- FALSE
      
      # disable controls if no text data
      disableTextAnalysisControls()
    }
  }
})

# graph tab specific controls
setGraphTabControls <- reactive({
  g <- ng_rv$graph_data
  
  # disable controls
  if (is.null(g)) {
    shinyjs::disable("graph_download_button")
    shinyjs::disable("graph_reseed_button")
    
    return(NULL)
  }
  
  # enable or disable controls based on network graph tab
  switch(input$selected_graph_tab,
         "Plot" = { enablePlotControls() },
         "visNetwork" = { enableVisNetworkControls() })
})

# network graph save file name based on selected network graph tab
saveGraphFileName <- reactive({
  switch(input$selected_graph_tab,
         "visNetwork" = systemTimeFilename("visNetwork-graph", "html"))
})

# network graph data based on selected network graph tab
saveGraphFileData <- reactive({
  data <- switch(input$selected_graph_tab,
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
  dt_vertices <- isolate(graphNodes())
  dt_selected_rows <- input$dt_vertices_rows_selected
  prune_list <- ng_rv$prune_verts
  
  selected_rows <- row.names(dt_vertices)[c(dt_selected_rows)]
  
  # add name if not already in list
  lapply(selected_rows, function(x) {
                          if (!x %in% ng_rv$prune_verts)
                            ng_rv$prune_verts <<- append(ng_rv$prune_verts, x)})
})

# add deselected data table row name values to pruned vertices list
pruneListAddOtherNames <- reactive({
  dt_vertices <- isolate(graphNodes())
  dt_selected_rows <- input$dt_vertices_rows_selected
  prune_list <- ng_rv$prune_verts
  
  # does not let user prune all data this way requires two or more selected rows
  if (length(dt_selected_rows) > 1) {
    selected_rows <- row.names(dt_vertices)[c(dt_selected_rows)]
    
    # names of vertices not selected
    sdf <- subset(dt_vertices, !(row.names(dt_vertices) %in% selected_rows))
    selected_rows <- row.names(sdf)
    
    # add name if not already in list
    lapply(selected_rows, function(x) {
                            if (!x %in% ng_rv$prune_verts) 
                              ng_rv$prune_verts <<- append(ng_rv$prune_verts, x)}) 
  }
})

# graph nodes as dataframe
graphNodes <- reactive({
  g <- graphFilters()
  
  if (is.null(g)) { return(NULL) }

  df_parameters <- list()
  
  df_parameters[['name']] <- V(g)$name
  if (!(is.null(vertex_attr(g, "label")))) { df_parameters[['label']] <- V(g)$label }
  if ("color" %in% vertex_attr_names(g)) { df_parameters[['color']] <- V(g)$color }
  df_parameters[['degree']] <- V(g)$Degree
  df_parameters[['indegree']] <- V(g)$Indegree
  df_parameters[['outdegree']] <- V(g)$Outdegree
  df_parameters[['betweenness']] <- V(g)$Betweenness
  df_parameters[['closeness']] <- V(g)$Closeness
  
  attr_v <- vertex_attr_names(g)
  voson_txt_attrs <- attr_v[grep(voson_txt_prefix, attr_v, perl = T)]
  if (length(voson_txt_attrs)) {
    attr <- voson_txt_attrs[1]
    df_txt_attr <- gsub(voson_txt_prefix, "", attr, perl = TRUE)
    df_parameters[[df_txt_attr]] <- vertex_attr(g, attr, index = V(g))
  }
  
  voson_cat_attrs <- attr_v[grep(voson_cat_prefix, attr_v, perl = T)]
  if (length(voson_cat_attrs) > 0) {
    for (i in 1:length(voson_cat_attrs)) {
      attr <- voson_cat_attrs[i]
      df_txt_attr <- gsub(voson_cat_prefix, "", attr, perl = TRUE) # vosonCA_
      df_parameters[[df_txt_attr]] <- vertex_attr(g, attr, index = V(g))
    }  
  }

  df_parameters['stringsAsFactors'] <- FALSE
  df <- do.call(data.frame, df_parameters)
  
  row.names(df) <- V(g)$id
  
  return(df)
})

# graph edges as dataframe
graphEdges <- reactive({
  g <- graphFilters()
  
  if (is.null(g)) { return(NULL) }
  
  igraph::as_data_frame(g, what = c("edges"))
})

# graph summary
graphSummaryOutput <- reactive({
  g <- graphFilters()
  
  output <- c()
  
  if (!is.null(g)) {
    output <- append(output, paste0("Nodes: ", vcount(g)))
    output <- append(output, paste0("Edges: ", ecount(g)))
    
    isolate_count <- sum(degree(g) == 0)
    output <- append(output, paste0("Isolates: ", isolate_count))
  }else {
    output <- append(output, paste0(""))
  }
  
  paste0(output, collapse = '<br>') # \n
})

graphLegendOutput <- reactive({
  if (input$graph_legend_check == FALSE) { return("") }
  g <- graphFilters()
  output <- c()
  
  if (!is.null(g)) {
    isolate({
      categorical_attributes <- ng_rv$graph_cats
      selected_categorical_attribute <- input$graph_cat_select
    })
    
    output <- append(output, paste0(""))
    
    if (length(categorical_attributes) > 0) {
      if (nchar(selected_categorical_attribute) && selected_categorical_attribute != "All") {
        categories <- categorical_attributes[[selected_categorical_attribute]]
        df <- data.frame('cat' = categories)
        if (nrow(df) > 0) {
          if (!("color" %in% vertex_attr_names(g) & input$use_vertex_colors_check == TRUE)) {
            output <- append(output, paste0("<table><tbody><tr><td colspan='3'>",
                                            selected_categorical_attribute, "</td></tr>"))
            df$color <- gbl_plot_palette()[1:nrow(df)]
            for (row in 1:nrow(df)) {
              output <- append(output,
                paste0("<tr><td style='vertical-align:middle'>",
                  "<span style='height:12px; width:12px; border-radius:50%; display:inline-block;",
                  "background-color:", df[row, 2], ";'></span></td>",
                  "<td>&nbsp;</td><td style='vertical-align:middle'>", df[row, 1], "</td></tr>"))
            }
            output <- append(output, "</tbody></table>")
          }
        }
      }
    }
    
  } else {
    output <- append(output, paste0(""))
  }
  
  output
})

graphComponentSummary <- reactive({
  g <- graphFilters()
  
  output <- c()
  
  if (!is.null(g)) {
    graph_clusters <- components(g, mode = isolate(input$graph_component_type_select))
    
    output <- append(output, paste0("Components (", isolate(input$graph_component_type_select), "): ", 
                                    graph_clusters$no))
    
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
  }else {
    output <- append(output, paste0(""))
  }
  
  paste0(output, collapse = '\n')
})

#### functions ------------------------------------------------------------------------------------------------------- #

# set graph manually
setGraphView <- function(data, desc = "", type = "", name = "", seed = 1) {
  shinyjs::reset("graphml_data_file")
  
  ng_rv$graph_data <- data
  ng_rv$graph_desc <- desc
  ng_rv$graph_type <- type
  ng_rv$graph_name <- name
  ng_rv$graph_seed <- seed
  ng_rv$graph_cats <- c()
  ng_rv$graph_cat_selected <- ""
  
  createGraphCategoryList()
  setGraphFilterControls()
  
  updateTabItems(session, "sidebar_menu", selected = "network_graphs_tab")
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

# filter out list of vertices from graph object
applyPruneFilterSrv <- function(g, selected_prune_verts) {
  if (length(selected_prune_verts) > 0) {
    verts <- which(V(g)$id %in% selected_prune_verts)
    g <- delete.vertices(g, verts) # selected_prune_verts
  }
  return(g)
}

# normalize continuous values
norm_values <- function(x) {
  # all values the same
  if (var(x) == 0) { return(rep(0.1, length(x))) }
  
  min_x <- min(x)
  diff_x <- max(x) - min_x
  s <- sapply(x, function(y) { (y - min_x) / diff_x })
}
