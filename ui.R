library(shiny)

# Define UI for random distribution app ----
ui <- fluidPage(
    sidebarLayout(sidebarPanel(
        titlePanel("Test Automated Inspection System!"),
        
        span("Team Leader", style = "color:#4d3a7d"),br(),
        
        span("Sudhakshine.N", style = "color:#4d3a7d"),br(),br(),
        
        span("Team Members", style = "color:block"),br(),
        span("SAKTHIVEL K", style = "color:blue"),br(),
        span("SELVA SUNDAR S", style = "color:blue"),br(),
        span("SRINIVASAN R", style = "color:blue"),br(),
        span("SRI VIGNESH D", style = "color:blue"),br(),
        span("THILLAI PARATHASARATHY R", style = "color:blue"),br(),
        br(),
        br(),
        # Output: Tabset w/ plot, summary, and table 
        
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
        
        # Output: Tabset w/ plot, summary, and table ----
        tabsetPanel(type = "pills",
                    tabPanel("Jute Data", tableOutput("juteData")),
                    tabPanel("WD1", tableOutput("resultWD1")),
                    tabPanel("WD2", tableOutput("resultWD2")),
                    tabPanel("WD3", tableOutput("resultWD3")),
                    tabPanel("Graph", plotOutput("pieChart"))
        )
        
    )
    )
)

