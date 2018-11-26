#### network metric tab  ---------------------------------------------------------------------------------------------- #
tabItem(tabName="network_metrics_tab",
        fluidRow(
          box(
            width = 12,
            h2("Network Metrics"),
            verbatimTextOutput("network_metrics_details_output", placeholder = TRUE),
            hr(),
            h4("Component Distribution"),
            plotOutput("componentDistPlot"),
            hr(),
            h4("Degree Distribution"),
            plotOutput("degreeDistPlot"),
            hr(),
            h4("Indegree Distribution"),
            plotOutput("indegreeDistPlot"),
            hr(),
            h4("Outdegree Distribution"),
            plotOutput("outdegreeDistPlot")
          )
        )
)
#### end network_metrics_tab