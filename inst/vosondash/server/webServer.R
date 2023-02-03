#' VOSON Dashboard webServer
#'
#' Collects hyperlinks and creates a network using the vosonSML package.
#'

#### values ---------------------------------------------------------------------------------------------------------- #

hyperlink_rv <- reactiveValues(
  hyperlink_seed_urls = NULL,
  hyperlink_data = NULL,
  hyperlink_network = NULL,
  hyperlink_graphml = NULL,
  data_cols = NULL
)

#### events ---------------------------------------------------------------------------------------------------------- #

observeEvent(input$hyperlink_add_url_button, {
  page <- input$hyperlink_url_input
  type <- input$hyperlink_crawl_type_select
  max_depth <- input$hyperlink_max_depth_input
  delay <- 1
  if (input$hyperlink_request_delay_robots_checkbox) delay <- NULL
  hyperlink_rv$hyperlink_seed_urls <- dplyr::bind_rows(
    hyperlink_rv$hyperlink_seed_urls,
    tibble::tibble(page = page, type = type, max_depth = max_depth, delay = delay)
  )
})

observeEvent(input$hyperlink_remove_url_button,{
  if (!is.null(input$hyperlink_seed_urls_table_rows_selected)) {
    hyperlink_rv$hyperlink_seed_urls <- hyperlink_rv$hyperlink_seed_urls[-as.numeric(input$hyperlink_seed_urls_table_rows_selected), ]
  }
  
  tbl_value <- hyperlink_rv$hyperlink_seed_urls
  if (!is.null(tbl_value)) {
    if (inherits(tbl_value, "data.frame") && nrow(tbl_value) < 1) {
      shinyjs::disable("hyperlink_create_button")
    }
  } else {
    shinyjs::disable("hyperlink_create_button") 
  }
})

observeEvent(hyperlink_rv$hyperlink_seed_urls,{
  tbl_value <- hyperlink_rv$hyperlink_seed_urls
  if (!is.null(tbl_value)) {
    if (inherits(tbl_value, "data.frame") && nrow(tbl_value)) {
      shinyjs::enable("hyperlink_collect_button")
    }
  }
  
  FALSE
})

# hyperlink collection button pushed
observeEvent(input$hyperlink_collect_button, {
  
  # disable button so it is not pushed again
  shinyjs::disable("hyperlink_collect_button")
  
  withProgress(message = 'Collecting hyperlinks', value = 0.5, {
    
    withConsoleRedirect("hyperlink_console", {
      tryCatch({
        hyperlink_rv$hyperlink_data <- vosonSML::collect_web_hyperlinks(pages = hyperlink_rv$hyperlink_seed_urls, verbose = TRUE)
        hyperlink_rv$data_cols <- names(hyperlink_rv$hyperlink_data)
      }, error = function(err) {
        incProgress(1, detail = "Error")
        cat(paste('hyperlink collection error:', err))
        return(NULL)
      })
      
      incProgress(1, detail = "Finished")
      updateTabItems(session, "hyperlink_control_tabset", selected = "Create Network")
    }) # withConsoleRedirect
    
  }) # withProgress
  
  shinyjs::enable("hyperlink_collect_button")
  delay(gbl_scroll_delay, js$scroll_console("hyperlink_console"))
})

observeEvent(hyperlink_rv$hyperlink_data, {
  if (!is.null(hyperlink_rv$hyperlink_data) && nrow(hyperlink_rv$hyperlink_data)) {
    shinyjs::enable("hyperlink_create_button")
  } else {
    shinyjs::disable("hyperlink_create_button")
  }
})

observeEvent(input$hyperlink_create_button, {
  net_type <- input$hyperlink_network_type_select
  network <- NULL
  
  shinyjs::disable("hyperlink_create_button")
  
  withProgress(message = 'Creating network', value = 0.5, {
    
    withConsoleRedirect("hyperlink_console", {
      if (net_type == "activity") {
        network <- vosonSML::Create(isolate(hyperlink_rv$hyperlink_data), "activity", verbose = TRUE)
      } else if (net_type == "actor") {
        network <- vosonSML::Create(isolate(hyperlink_rv$hyperlink_data), "actor", verbose = TRUE)
      }
      if (!is.null(network)) {
        hyperlink_rv$hyperlink_network <- network
        hyperlink_rv$hyperlink_graphml <- vosonSML::Graph(network) 
      }
    }) # withConsoleRedirect
  
    incProgress(1, detail = "Finished")
  }) # withProgress
  
  shinyjs::enable("hyperlink_create_button")
  
  delay(gbl_scroll_delay, js$scroll_console("hyperlink_console"))
})

# download and view actions
callModule(collectDataButtons, "hyperlink", data = reactive({ hyperlink_rv$hyperlink_data }), file_prefix = "hyperlink")

callModule(collectNetworkButtons, "hyperlink", network = reactive({ hyperlink_rv$hyperlink_network }), file_prefix = "hyperlink")


callModule(collectGraphButtons_, "hyperlink", graph_data = reactive({ hyperlink_rv$hyperlink_graphml }), file_prefix = "hyperlink")

hyperlink_view_rvalues <- callModule(collectViewGraphButtons, "hyperlink", graph_data = reactive({ hyperlink_rv$hyperlink_graphml }))

observeEvent(hyperlink_view_rvalues$data, {
  setGraphView(data = isolate(hyperlink_view_rvalues$data), 
               desc = paste0("Hyperlink network for seed pages: ", paste0(isolate(hyperlink_rv$hyperlink_seed_urls$page), collapse = ', '), sep = ""),
               type = "hyperlink",
               name = "",
               seed = sample(gbl_rng_range[1]:gbl_rng_range[2], 1))
  updateCheckboxInput(session, "expand_demo_data_check", value = FALSE)
}, ignoreInit = TRUE)

observeEvent(input$clear_hyperlink_console, {
  resetConsole("hyperlink_console")
})
#### output ---------------------------------------------------------------------------------------------------------- #

output$hyperlink_seed_urls_table <- DT::renderDT({
  DT::datatable(
    hyperlink_rv$hyperlink_seed_urls,
    rownames = FALSE,
    editable = TRUE,
    options = list(dom = 't')
  )
})

# render reddit data table
output$dt_hyperlink_data <- DT::renderDataTable({
  datatableHyperlinkData()
})

output$seed_table_toggle <- reactive({
  tbl_value <- hyperlink_rv$hyperlink_seed_urls
  if (is.null(tbl_value)) return(FALSE)
  if (inherits(tbl_value, "data.frame") && nrow(tbl_value)) return(TRUE)
  FALSE
})

outputOptions(output, "seed_table_toggle", suspendWhenHidden = FALSE)

observeEvent(input$select_all_hyperlink_dt_columns, {
  updateCheckboxGroupInput(session, "show_hyperlink_cols", label = NULL,
                           choices = isolate(hyperlink_rv$data_cols),
                           selected = isolate(hyperlink_rv$data_cols),
                           inline = TRUE)
})

observeEvent(input$clear_all_hyperlink_dt_columns, {
  updateCheckboxGroupInput(session, "show_hyperlink_cols", label = NULL,
                           choices = isolate(hyperlink_rv$data_cols),
                           selected = character(0),
                           inline = TRUE)
})

observeEvent(input$reset_hyperlink_dt_columns, {
  updateCheckboxGroupInput(session, "show_hyperlink_cols", label = NULL,
                           choices = isolate(hyperlink_rv$data_cols),
                           selected = c("url", "n", "page_err", "page", "depth", "max_depth", "seed", "type"),
                           inline = TRUE)
})

output$hyperlink_data_cols_ui <- renderUI({
  data <- hyperlink_rv$data_cols
  
  if (is.null(data)) { return(NULL) }
  
  conditionalPanel(condition = 'input.expand_show_hyperlink_cols',
                   div(actionButton("select_all_hyperlink_dt_columns", "Select all"), 
                       actionButton("clear_all_hyperlink_dt_columns", "Clear all"),
                       actionButton("reset_hyperlink_dt_columns", "Reset")),
                   checkboxGroupInput("show_hyperlink_cols", label = NULL,
                                      choices = hyperlink_rv$data_cols,
                                      selected = c("url", "n", "page_err", "page", "depth", "max_depth", "seed", "type"),
                                      inline = TRUE, width = '98%')
  )
})

#### reactives ------------------------------------------------------------------------------------------------------- #

datatableHyperlinkData <- reactive({
  data <- hyperlink_rv$hyperlink_data
  
  if (is.null(data)) { return(NULL) }
  
  cls_lst <- class(data)
  class(data) <- cls_lst[!cls_lst %in% c("datasource", "web")]
  
  if (!is.null(input$show_hyperlink_cols)) {
    if (length(input$show_hyperlink_cols) > 0) {
      data <- dplyr::select(data, input$show_hyperlink_cols)
    } else { return(NULL) }
  } else { return(NULL) }
  
  if (nrow(data) < 1) { return(NULL) }
  
  col_classes <- sapply(data, class)
  for (i in seq(1, length(col_classes))) {
    if ("list" %in% col_classes[i]) {
      var <- names(col_classes)[i]
      data[var] <- lapply(data[var], function(x) sapply(x, paste, collapse = ", ", character(1L)))
    }
  }
  
  if (!is.null(hyperlink_rv$hyperlink_data)) {
    col_defs <- NULL
    if (input$dt_hyperlink_truncate_text_check == TRUE) {
      col_defs <- gbl_dt_col_defs
      col_defs[[1]]$targets = "_all"
    }
    DT::datatable(data, extensions = 'Buttons', filter = "top",
                  options = list(lengthMenu = gbl_dt_menu_len, pageLength = gbl_dt_page_len, scrollX = TRUE,
                                 columnDefs = col_defs, dom = 'lBfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'print')),
                  class = 'cell-border stripe compact hover')
  }
})
