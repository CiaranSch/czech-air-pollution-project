# Deployed to
# https://cschutten.shinyapps.io/project/

fluidPage(
    titlePanel("Analysis of air pollution levels"),
    sidebarLayout(
        sidebarPanel(
            selectizeInput("pollutant", 
                           "Pollutant", 
                           selected="PM2.5",
                           choices=pollutants,
                           multiple=FALSE
            ),
            selectizeInput("stations", 
                           "Stations (max 4)", 
                           choices="",
                           selected = "",
                           multiple=TRUE,
                           options = list(maxItems = 4)
            ),
            selectizeInput("aggregation", 
                         "Aggregation",
                         selected = "none",
                         choices=aggregations,
                         multiple=FALSE
            ),
            conditionalPanel(
                condition = "input.aggregation == 'none'",
                p("Please note the graph will take longer to load with no aggregation", style = "color:red")
            ),
            conditionalPanel(
                condition = "input.aggregation=='thresh_hours_per_day' || input.aggregation=='thresh_hours_per_year' || input.aggregation=='thresh_days_per_year'",
                numericInput("threshold", "Threshold", value=50, min=0, max=500, step=1)
            ),
            selectizeInput("xaxis", 
                           "X-Axis",
                           choices="",
                           selected="",
                           multiple=FALSE
            ),
            actionButton("calculate", "Calculate"),
            downloadButton("download", "Download table", icon="download")
            
        ),
        # The main panel is typically used for displaying R output
        mainPanel(
            h2("Pollution Plotted"),
            plotOutput("pollutionplot"),
            plotOutput("maplot"),
            DT::dataTableOutput("table")
        )
    )
)