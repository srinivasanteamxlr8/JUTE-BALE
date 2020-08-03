library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)

# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("GET IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="User Name", label = tagList(icon("user"), "Username")),
                   passwordInput("passwd", placeholder="Pass Word", label = tagList(icon("unlock-alt"), "Password")),
                   passwordInput("passwd", placeholder="Pass Word", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: white; background-color:#00FFFF;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     br(),
                     
                   ))
)

credentials = data.frame(
  username_id = c("srini","viki"),
  passod   = sapply(c("password@123","mother@123"),password_store),
  passod   = sapply(c("1234","mother@123"),password_store),
  permission  = c("basic", "advanced"), 
  stringsAsFactors = F
)

header <- dashboardHeader( title = "JUTE BALE DASHBOARD", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<-dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          } 
        } 
      }
    }    
  })
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #7FFFD4 !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      div(
        tags$div(
          tags$h2("Test Automated Inspection System!"), 
          tags$h3("Team Leader",style = "color:yellow"), 
          tags$h3("Sudhakshine.N", style = "color:orange"), 
          tags$h3("Team Members",style = "color:yellow"), 
          tags$h4("Sakthivel K", style = "color:white"), 
          tags$h4("Selva Sundar S", style = "color:white"), 
          tags$h4("Srinivasan R", style = "color:white"), 
          tags$h4("Sri Vignesh D", style = "color:white"), 
          tags$h4("Thillai Parthasarathy R", style = "color:white")
        )
      ) 
    }
  })
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      tabsetPanel(type = "pills",
                  tabPanel("Jute Data", tableOutput("juteData")),
                  tabPanel("WD1", tableOutput("resultStrong")),
                  tabPanel("WD2", tableOutput("resultGood")),
                  tabPanel("WD3", tableOutput("resultBad")),
                  tabPanel("Graph", plotOutput("pieChart"))
      )
    }
    else {
      loginpage
    }
  })
  
  
  output$results <-  DT::renderDataTable({
    datatable(iris, options = list(autoWidth = TRUE,
                                   searching = FALSE))
  })
  
  juteData <- read.csv("Data.csv", header = TRUE)
  
  resultStrong <- subset(juteData, juteData$WEIGHT.OF.THE.BALE..kg. > 150) 
  resultGood <- subset(juteData, juteData$MOISTURE.CONTENT... < 9)
  resultBad <- subset(juteData, juteData$MOISTURE.CONTENT... > 10)
  
  strongCount=nrow(resultStrong)
  goodCount=nrow(resultGood)
  badCount=nrow(resultBad)
  
  output$juteData <- renderTable({
    juteData
  })
  
  output$resultStrong <- renderTable({
    resultStrong
  })
  
  output$resultGood <- renderTable({
    resultGood
  })
  
  output$resultBad <- renderTable({
    resultBad
  })
  
  output$pieChart <- renderPlot({
    graphData <- c(strongCount,goodCount, badCount)
    pie(graphData, labels = c("Strong","Good","Bad"),main = "Graphical Result",col = c("green","yellow","red")) 
  })
  
  output$results <-  DT::renderDataTable({
    datatable(iris, options = list(autoWidth = TRUE,
                                   searching = FALSE))
  })
  
}

runApp(list(ui = ui, server = server), launch.browser = TRUE)