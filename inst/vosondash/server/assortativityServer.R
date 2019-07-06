#' VOSON Dashboard assortativityServer
#'
#' Measures of assortative mixing and homophily in network.
#'

#### values ----------------------------------------------------------------------------------------------------------- #

assort_rvalues <- reactiveValues(
  mixmat_message = NULL            # displays a message if problem with mixing matrix
)

#### events ----------------------------------------------------------------------------------------------------------- #

#### output ----------------------------------------------------------------------------------------------------------- #

output$assortativity_details_output <- renderText({
  assortativityPrelimOutput()
})

output$mixing_matrix <- DT::renderDataTable({
  DT::datatable(assortativityMMOutput(), options = list(paging = F, searching = F, bInfo = F, ordering = F))
})

output$assortativity_homophily_output <- renderText({
  homophilyOutput()
})

output$mixing_matrix_details_output <- renderText({
  assort_rvalues$mixmat_message
})

#### reactives -------------------------------------------------------------------------------------------------------- #

# returns selected categorical attribute output message
assortativityPrelimOutput <- reactive({
  g <- graphFilters()
  
  output <- c()
  
  if (!is.null(g)) {
    CA_sel <- ng_rv$graph_cat_selected
    if (nchar(CA_sel) && CA_sel != "All") {   # eventually will have cat attr selected by default...
      output <- append(output, paste0("Selected categorical attribute is: ", CA_sel))
      output <- append(output, "")
    }else{
      return(NULL)
      # output <- append(output, paste0("Categorical attribute not present, or not selected."))
    }
  }else{
    return(NULL)
    # output <- append(output, paste0("No data."))
  }
  
  paste0(output, collapse = '\n')
})

# creates and returns mixing matrix dataframe, or returns null and sets an output message
assortativityMMOutput <- reactive({
  g <- graphFilters()
  
  if (!is.null(g)) {
    CA_sel <- ng_rv$graph_cat_selected
    if (nchar(CA_sel) && CA_sel != "All") {  # eventually will have cat attr selected by default...
      assort_rvalues$mixmat_message <- NULL
      df <- VOSONDash::mixmat(g, paste0("vosonCA_", CA_sel), use.density = FALSE)
      return(df)
    } else {
      assort_rvalues$mixmat_message <- "Categorical attribute not present, or not selected."
      return(NULL)      
    }
  } else {
    assort_rvalues$mixmat_message <- "No Data."
    return(NULL)
  }
})

# returns output for homophily index calculations
homophilyOutput <- reactive({
  g <- graphFilters()
  
  output <- c()
  
  if (!is.null(g)) {
    CA_sel <- ng_rv$graph_cat_selected
    if (nchar(CA_sel) && CA_sel != "All") {   # eventually will have cat attr selected by default...
      # output <- append(output, paste0("Selected categorical attribute is: ", CA_sel))
      vattr <- paste0('vosonCA_', CA_sel)
      mm <- VOSONDash::mixmat(g, paste0("vosonCA_", CA_sel), use.density = FALSE)
      
      attr_list <- ng_rv$graph_cats[[CA_sel]]
      
      # if subset of attributes selected
      if (input$graph_sub_cats_select[1] != "All") {
        attr_list <- input$graph_sub_cats_select
      }
      
      for (i in attr_list) {
        output <- append(output, paste0("Category: ", i))
        w_i <- length(which(vertex_attr(g, vattr) == i)) / length(V(g))
        output <- append(output, paste0("  Population share: ", sprintf("%.3f", w_i)))
        if (w_i > 0) {
          H_i <- mm[i, i] / rowSums(mm)[i]
          output <- append(output, paste0("  Homogeneity index: ", sprintf("%.3f", H_i)))
          Hstar_i <- (H_i - w_i) / (1 - w_i)
          output <- append(output, paste0("  Homophily index: ", sprintf("%.3f", Hstar_i)))          
        }
        output <- append(output, "")
      }
      output <- append(output, "")
    }else{
      output <- append(output, paste0("Categorical attribute not present, or not selected."))
    }
  }else{
    output <- append(output, paste0("No data."))
  }
  
  paste0(output, collapse = '\n')
})
