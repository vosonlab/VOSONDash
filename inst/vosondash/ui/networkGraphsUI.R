#### network graphs tab  ---------------------------------------------------------------------------------------------- #
tabItem(tabName = "network_graphs_tab",
        fluidRow(
          column(width = 3, offset = 0,
                 fluidRow(
                   # graph controls
                   sidebarPanel(width = 12, class = "custom_well_for_controls",
                                fileInput('graphml_data_file', 'Choose graphml file', accept = c('.graphml')),
                                checkboxInput('expand_demo_data_check', div("Package Datasets"), FALSE),
                                conditionalPanel(condition = 'input.expand_demo_data_check',
                                                 fluidRow(
                                                         column(width = 12,
                                                                div(shinyjs::disabled(selectInput("demo_data_select", 
                                                                                              label = NULL, 
                                                                                              choices = c("No Demo Dataset Files Found"), selected = NULL, multiple = FALSE))),
                                                                div(shinyjs::disabled(actionButton("demo_data_select_button", label = "Load graphml")))
                                                         )
                                                 )
                                                 
                                )
                   ),
                   sidebarPanel(width = 12, class = "custom_well_for_controls",
                                fluidRow(
                                        column(width = 12,
                                          div("Labels", style = "font-weight: bold;"),
                                          div(disabled(checkboxInput("node_index_check", "Index", FALSE)), class = "div_inline", style = "margin-right:8px; margin-top:0px;"),
                                          div(disabled(checkboxInput("node_labels_check", "Label Attribute", FALSE)), class = "div_inline"),
                                          div(disabled(checkboxInput("node_sel_labels_check", "Selected Nodes", TRUE)), class = "div_inline")
                                        )
                                ),
                                fluidRow(
                                        column(width = 12,
                                          div("Graph Filters", style = "font-weight: bold;"),
                                          div(disabled(checkboxInput("graph_multi_edge_check", "Multiple Edges", TRUE)), class = "div_inline", style = "margin-right:8px; margin-top:0px;"),
                                          div(disabled(checkboxInput("graph_loops_edge_check", "Loops", TRUE)), class = "div_inline", style = "margin-right:8px; margin-top:0px;"),
                                          div(disabled(checkboxInput("graph_isolates_check", "Isolates", TRUE)), class = "div_inline")
                                        )
                                ),
                                fluidRow(
                                  column(width = 4,
                                         div(tags$b("Graph Layout"), 
                                             vpopover(po_graph_layout()$title, po_graph_layout()$content), 
                                             style = "margin-bottom:5px;"),
                                         disabled(selectInput("graph_layout_select", label = NULL, choices = c("Auto", "FR", "KK", "DH", "LGL", "Graphopt", "DrL", "GEM",
                                                                                                               "MDS", "Grid", "Sphere", "Circle", "Star", "Random"),
                                                              selectize = TRUE, selected = "Auto"))
                                  ),
                                  column(width = 6,
                                         disabled(sliderInput("graph_spread_slider", "Spread", min = 0.25, max = 2.5, step = 0.1, value = c(1), ticks = FALSE))
                                  ),
                                  column(width = 2,
                                         div(div(id = "seed", "", class = "div_inline"), disabled(actionButton("graph_reseed_button", label = icon("refresh"), style = "padding:2px 8px;")), style = "float:right; margin-top:5px; font-size:0.98em;",
                                             vpopover(po_reseed_graph()$title, po_reseed_graph()$content))       
                                  )
                                ),
                                conditionalPanel(condition = 'input.graph_layout_select == "FR" | input.graph_layout_select == "Graphopt"',
                                                 fluidRow(column(width = 6, numericInput(inputId = "graph_niter", "Iterations (niter)", value = 500, min = 1, max = 1000000)))
                                ),
                                conditionalPanel(condition = 'input.graph_layout_select == "Graphopt"',
                                                 fluidRow(column(width = 6, numericInput(inputId = "graph_charge", "Charge", value = 0.001, min = 0.001, max = 1.0, step = 0.001)),
                                                          column(width = 6, numericInput(inputId = "graph_mass", "Mass", value = 30, min = 1, max = 1000))),
                                                 fluidRow(column(width = 6, numericInput(inputId = "graph_spr_len", "Spring Length", value = 0, min = 0, max = 1000)),
                                                          column(width = 6, numericInput(inputId = "graph_spr_const", "Constant", value = 1, min = 1, max = 1000)))
                                ),
                                fluidRow(
                                  column(width = 4,
                                         div("Node Size", style = "font-weight: bold;", class = "custom_node_size_div"),
                                         disabled(selectInput("graph_node_size_select", label = NULL, choices = c("None", "Degree", "Indegree", "Outdegree", "Betweenness", "Closeness"),
                                                              multiple = FALSE, selectize = TRUE))
                                  ),
                                  column(width = 6,
                                         disabled(sliderInput("graph_node_size_slider", label = "Multiplier", min = 0.1, max = 15, step = 0.1, value = c(1), ticks = FALSE, animate = FALSE))
                                  )
                                ),
                                
                                checkboxInput('use_vertex_colors_check', 
                                              div("Node colors from graphml", style = "margin-bottom:5px;")
                                              , TRUE),
                                
                                checkboxInput('expand_categorical_filter_check', 
                                              div(tags$b("Categorical Filter"), 
                                                  vpopover(po_cat_filter()$title, po_cat_filter()$content), 
                                                  style = "margin-bottom:5px;")
                                              , FALSE),
                                conditionalPanel(condition = 'input.expand_categorical_filter_check',
                                                 fluidRow(
                                                   column(width = 6,
                                                          disabled(selectInput("graph_cat_select", div("Category", style = "font-weight: normal;"), choices = c("All"), multiple = FALSE)), # selectize = TRUE graph_catAttr_select
                                                          checkboxInput("graph_legend_check", "Legend", TRUE)
                                                   ),
                                                   column(width = 6,
                                                          disabled(selectInput("graph_sub_cats_select", div("View", style = "font-weight: normal;"), choices = c("All"), multiple = TRUE, selected = "All", selectize = FALSE, size = 3))
                                                   )
                                                 )
                                ),
                                checkboxInput('expand_component_filter_check', div("Component Filter", style = "font-weight: bold;"), FALSE),
                                conditionalPanel(condition = 'input.expand_component_filter_check',
                                                 disabled(checkboxInput('reset_on_change_check', div("Recalculate on category change", style = "font-weight: normal;"), TRUE)),
                                                 fluidRow(
                                                   column(width = 4,
                                                          shinyjs::disabled(selectInput("graph_component_type_select", div("Type", style = "font-weight: normal;"), choices = c("Weak", "Strong"), selected = "Weak", multiple = FALSE))
                                                   ),
                                                   column(width = 8,
                                                          disabled(sliderInput("graph_component_slider", div("Size", style = "font-weight: normal;"), min = 1, max = 500, value = c(1, 500), ticks = FALSE))
                                                   )
                                                 ),
                                                 fluidRow(
                                                         column(width = 12,
                                                                verbatimTextOutput("component_summary_ui"))
                                                 )
                                                 
                                ),
                                checkboxInput('expand_nbh_check', div("Neighbourhood Select", style = "font-weight: bold;"), FALSE),
                                conditionalPanel(condition = 'input.expand_nbh_check',
                                                 fluidRow(
                                                         column(width = 4,
                                                                selectInput("nbh_order_select", div("Order", style = "font-weight: normal;"), choices = c(1:10), selected = 1, multiple = FALSE)
                                                         ),
                                                         column(width = 8,
                                                                fluidRow(actionButton("nbh_select_button", label = "Select Nodes"),
                                                                         actionButton("nbh_prune_unselected", label = icon("scissors"))
                                                                        ),
                                                                fluidRow(disabled(actionButton("nbh_undo_button", label = "Undo")),
                                                                         actionButton("nbh_deselct_button", "Deselect All"),
                                                                         actionButton("nbh_reset_button", "Reset")
                                                                         )
                                                        )
                                                 )
                                                 
                                ),                                
                                conditionalPanel(condition = js_is_mac,
                                                 disabled(checkboxInput("macos_font_check", "Arial Unicode MS", TRUE))
                                )
                                
                   )
                 )
          ),
          
          column(width = 9, offset = 0,
                 fluidRow(
                   # graph type tabs
                   uiOutput("vis_plot_ui"),
                   uiOutput("plot_height_ui"),
                   uiOutput("graph_summary_ui"),
                   uiOutput("graph_legend_ui"),
                   
                   # graph info and download buttons
                   sidebarPanel(id = "graph_info_well", width = 12, class = "custom_well_for_buttons",
                                fluidRow(
                                        div(shinyjs::disabled(checkboxInput('expand_data_desc_check', label = NULL, FALSE)), class = "div_inline"),
                                        div(textOutput("graph_name"), class = "div_inline", style = "margin-bottom:10px;"),
                                        div(disabled(downloadButton("analysis_graphml_download_button", label = "Graphml", 
                                                                    title = "Download Plot Graphml File")), 
                                            style = "float:right; margin-right:10px;", class = "div_inline"),
                                        div(disabled(downloadButton("graph_download_button", label = "Plot HTML",
                                                                    title = "Download Plot as HTML File")), 
                                            style = "float:right; margin-right:10px;", class = "div_inline")
                                ),
                                
                                fluidRow(        
                                        conditionalPanel(condition = 'input.expand_data_desc_check',
                                            div(htmlOutput("graph_desc"))
                                        )
                                )
                                
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