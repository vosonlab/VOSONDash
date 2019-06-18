#### network graphs tab  ---------------------------------------------------------------------------------------------- #
tabItem(tabName = "network_graphs_tab",
        fluidRow(
          column(width = 3, offset = 0,
                 fluidRow(
                   # graph controls
                   sidebarPanel(width = 12, class = "custom_well_for_controls",
                                fileInput('graphml_data_file', 'Choose graphml file', accept = c('.graphml'))
                   ),
                   sidebarPanel(width = 12, class = "custom_well_for_controls",
                                div("Graph Filters", style = "font-weight: bold;", class = "div_inline"),
                                div(disabled(actionButton("graph_reseed_button", label = icon("refresh"), style = "padding: 2px 8px;")), style = "float:right; margin-top:5px;"),
                                disabled(checkboxInput("graph_names_check", "Node Names", FALSE)),
                                div(disabled(checkboxInput("graph_multi_edge_check", "Multiple Edges", TRUE)), class = "div_inline", style = "margin-right:8px; margin-top:0px;"),
                                div(disabled(checkboxInput("graph_loops_edge_check", "Loops", TRUE)), class = "div_inline", style = "margin-right:8px; margin-top:0px;"),
                                div(disabled(checkboxInput("graph_isolates_check", "Isolates", TRUE)), class = "div_inline"),
                                fluidRow(
                                  column(width = 6,
                                         disabled(selectInput("graph_layout_select", label = "Graph Layout", choices = c("Auto", "Fruchterman-Reingold", "Kamada-Kawai", "Davidson-Harel",
                                                                                                                         "Large Graph Layout", "Grid", "Sphere", "Circle", "Star", "Random"),
                                                              selectize = TRUE, selected = "Auto"))
                                  ),
                                  column(width = 6,
                                         disabled(sliderInput("graph_spread_slider", "Spread", min = 0.5, max = 2.5, step = 0.1, value = c(1), ticks = FALSE))
                                  )
                                ),
                                
                                fluidRow(
                                  column(width = 6,
                                         div("Node Size", style = "font-weight: bold;", class = "custom_node_size_div"),
                                         disabled(selectInput("graph_node_size_degree_select", label = NULL, choices = c("None", "Degree", "Indegree", "Outdegree", "Betweenness", "Closeness"),
                                                              multiple = FALSE, selectize = TRUE))
                                  ),
                                  column(width = 6,
                                         disabled(sliderInput("graph_node_size_slider", label = "Multiplier", min = 0, max = 10, step = 0.1, value = c(1), ticks = FALSE, animate = FALSE))
                                  )
                                ),
                                
                                checkboxInput('expand_categorical_filter_check', div("Categorical Filter", style = "font-weight: bold;"), FALSE),
                                conditionalPanel(condition = 'input.expand_categorical_filter_check',
                                                 fluidRow(
                                                   column(width = 6,
                                                          disabled(selectInput("graph_catAttr_select", div("Category", style = "font-weight: normal;"), choices = c("All"), multiple = FALSE)) # selectize = TRUE
                                                   ),
                                                   column(width = 6,
                                                          disabled(selectInput("graph_catAttr_attr_select", div("View", style = "font-weight: normal;"), choices = c("All"), multiple = TRUE, selected = "All", selectize = FALSE, size = 3))
                                                   )
                                                 )
                                ),
                                
                                #div("Components", style = "font-weight: bold;"),
                                checkboxInput('expand_component_filter_check', div("Component Filter", style = "font-weight: bold;"), FALSE),
                                conditionalPanel(condition = 'input.expand_component_filter_check',
                                                 disabled(checkboxInput('reset_on_change_check', div("Recalculate for category change", style = "font-weight: normal;"), TRUE)),
                                                 fluidRow(
                                                   column(width = 4,
                                                          shinyjs::disabled(selectInput("graph_component_type_select", div("Type", style = "font-weight: normal;"), choices = c("Weak", "Strong"), selected = "Weak", multiple = FALSE))
                                                   ),
                                                   column(width = 8,
                                                          disabled(sliderInput("graph_component_slider", div("Size", style = "font-weight: normal;"), min = 1, max = 500, value = c(1, 500), ticks = FALSE))
                                                   )
                                                 )
                                                 
                                )
                   ),
                   sidebarPanel(width = 12, class = "custom_well_for_controls",
                                div("Summary", style = "font-weight: bold;", style = "margin-bottom:5px;"),
                                verbatimTextOutput("graph_details_top_output", placeholder = TRUE)
                   )
                   
                 )
          ),
          
          column(width = 9, offset = 0,
                 fluidRow(
                   # graph type tabs
                   tabBox(width = 12, title = span(icon("share-alt", class = "social_green"), "Network Graphs"), 
                          selected = "Plot", id = "selected_graph_tab",
                          tabPanel("igraph", plotOutput("standardPlot", width = "100%", height = "500px"), 
                                   value = "Plot"),
                          tabPanel("visNetwork", visNetworkOutput("visNetworkPlot", width = "100%", 
                                                                       height = "500px"), value = "visNetwork"),
                          tabPanel("D3 Force", forceNetworkOutput("force", width = "100%", height = "500px")),
                          tabPanel("D3 Simple", simpleNetworkOutput("simple", width = "100%", height = "500px"))
                   ),
                   
                   # graph info and download buttons
                   sidebarPanel(id = "graph_info_well", width = 12, class = "custom_well_for_buttons",
                                fluidRow(
                                  div(textOutput("graphml_desc1_text"), textOutput("graphml_desc2_text"), 
                                      class = "div_inline"),
                                  div(disabled(downloadButton("graph_download_button", label = "Plot HTML",
                                                              title = "Download Plot as HTML File")), 
                                      style = "float:right; margin-right:10px;", class = "div_inline"),
                                  div(disabled(downloadButton("analysis_graphml_download_button", label = "Graphml", 
                                                              title = "Download Plot Graphml File")), 
                                      style = "float:right; margin-right:10px;", class = "div_inline")
                                )
                   )
                 ),
                 fluidRow(
                   column(width = 12,
                          verbatimTextOutput("graph_details_bottom_output")
                   )
                 )
          )
        ),
        
        fluidRow(
          # graph data table
          tabBox(width = 12, title = "Graph Data", selected = "Vertices",
                 id = "selected_dt_tab",
                 tabPanel("Vertices",
                          fluidRow(
                            div(checkboxInput("graph_dt_v_truncate_text_check", "Truncate text", TRUE), style = "margin-left:12px; margin-right:5px;", class = "div_inline")
                          ),
                          DT::dataTableOutput("dt_vertices"),
                          fluidRow(
                            column(width = 4, selectInput("pruned_vertices_select", "Pruned Nodes", choices = c(), multiple = TRUE, selectize = FALSE),
                                   div(actionButton("prune_return_button", "Un-prune Selected"), style = "margin-right:10px;", class = "div_inline"), 
                                   div(actionButton("prune_reset_button", "Reset"), class = "div_inline")),
                            column(width = 1, actionButton("prune_deselect_rows_button", "Deselect All"),
                                   actionButton("prune_selected_rows_button", "Prune Selected"),
                                   actionButton("prune_unselected_rows_button", "Prune Unselected"))
                          )),
                 tabPanel("Edges", 
                          fluidRow(
                            div(checkboxInput("graph_dt_e_truncate_text_check", "Truncate text", TRUE), style = "margin-left:12px; margin-right:5px;", class = "div_inline")
                          ),
                          DT::dataTableOutput("dt_edges"))
          )
        )
)
#### end network_graphs_tab