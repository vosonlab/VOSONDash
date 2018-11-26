#### assortativity tab  ----------------------------------------------------------------------------------------------- #
tabItem(tabName = "assortativity_tab",
        fluidRow(
          box(
            width = 12,
            h2("Assortativity"),
            verbatimTextOutput("assortativity_details_output", placeholder = FALSE),
            p(""),
            h4("Mixing Matrix"),
            verbatimTextOutput("mixing_matrix_details_output", placeholder = FALSE),
            DT::dataTableOutput("mixing_matrix"),
            hr(),
            h4("Homophily Index"),
            verbatimTextOutput("assortativity_homophily_output", placeholder = FALSE)
          )
        )
)
#### end assortativity tab