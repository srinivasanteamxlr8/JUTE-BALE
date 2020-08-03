library(shiny)
library(shinydashboard)
source("admin.R")

my_username <- c("test","admin")
my_password <- c("test","123")
get_role=function(user){
  
  if(user=="test") {
    
    return("TEST")
  }else{
    
    return("ADMIN")
  }
}

get_ui=function(role){
  itog=list()
  if(role=="TEST"){
    itog$title=test_title
    itog$main=test_main
    itog$side=test_side
    return(itog)
  }else{
    itog$title=admin_title
    itog$main=admin_main
    itog$side=admin_side
    return(itog)
  }
}


shinyServer(function(input, output,session) {
  
  USER <- reactiveValues(Logged = FALSE,role=NULL)
  
  ui1 <- function(){
    tagList(
      div(id = "login",
          wellPanel(textInput("userName", "Username"),
                    passwordInput("passwd", "Password"),
                    br(),actionButton("Login", "Log in")))
      ,tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -10px;margin-left: -150px;}")
    )}
  
  
  observe({ 
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Id.username <- which(my_username == Username)
          Id.password <- which(my_password == Password)
          if (length(Id.username) > 0 & length(Id.password) > 0) {
            if (Id.username == Id.password) {
              USER$Logged <- TRUE
              USER$role=get_role(Username)
              
            }
          } 
        }
      }
    }
  })
  observe({
    if (USER$Logged == FALSE) {
      
      output$page <- renderUI({
        box(
          div(class="outer",do.call(bootstrapPage,c("",ui1()))))
      })
    }
    if (USER$Logged == TRUE)    {
      output$page <- ui # ui from the first dashboard
    }
  })
})