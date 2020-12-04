library(maps)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(DT)
library(formattable)
library(dashboardthemes)
library(readxl)
library(RcppRoll)
library(plotly)
library(shinyBS)
library(shinyalert)
library(shinycustomloader)
library(geosphere)
library(waiter)
library(shinyjs)
library(grid)
library(jpeg)
library(sever)

#-----------------------------------------------------------------------------

load('complete_pred_tbl_2021.rda')

fightingteam <- complete_pred_tbl_2021 %>% 
  select(team1) %>% 
  unique()
# 
opponentteam <- complete_pred_tbl_2021 %>% 
  select(opponent) %>% 
  unique()



#-----------------------------------------------------------------------------

ui <- dashboardPage(
  
  #-----------------------------------------------------------------------------
  
  #header
  header <-  dashboardHeader(
    title = "2020-2021 NCAA BASKETBALL PREDICTION",
    titleWidth = 330
  ),
  
  #-----------------------------------------------------------------------------
  
  
  #sidebar
  sidebar <- dashboardSidebar(
    width = 330, #width of the left side bar
    
    sidebarMenu(
      menuItem('NCAA Men Basketball', tabName = "men", icon = icon("male")),
      menuItem("NCAA Women Basketball", tabName = "women", icon = icon("female"))
    )
  ),
  
  
  #-----------------------------------------------------------------------------
  
  #body
  body <- dashboardBody(
    
    #prevent error message
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    #this sets the width of dropdownBlock where twitter feed is embedded on the header part
    tags$head(tags$style(HTML('.navbar-custom-menu>.navbar-nav>li>.dropdown-menu {width:400px;}'))),
    
    #this code helps align the team logos in the picker input displayed on the left side bar
    tags$head(tags$style(".jhr{ display: inline;vertical-align: middle;padding-left: 10px;}")),
    
    #this code helps prevent Data Tables from altering default table font colors automatically 
    tags$style(HTML("
                    .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate {
                    color:lightblue;
                    }
                    
                    thead {
                    color:white;
                    }
                    
                    tbody {
                    color:white;
                    }
                    
                    ")),
    
    #this sets the customised look for slider inputs on the right side bar
    chooseSliderSkin("Simple"),
    
    #this sets the custom theme for the app, In our case we are using a dark grey theme
    shinyDashboardThemes(theme = "grey_dark"),
    
    
    tabItems(
      
      
      #edit the first tab
      tabItem(
        tabName = 'men',
        tabBox(title = "", id = "tab1", height = "100%", width = "100%",
               
               
               #first selection   
               fluidRow(
                 
                 column(
                   width = 6,
                   pickerInput(
                     inputId = 'fighting',
                     label = 'Select A Fighting Team', 
                     choices = fightingteam,
                     multiple = F)),
                 
               ),
               
               #second selection 
               fluidRow(
                 
                 column(
                   width = 6, 
                   uiOutput('date'))
                 
               ),
               
               #Winner 
               fluidRow(
                 
                 #Selected team data
                 column(width = 6, align="center",
                        uiOutput("team"),
                        htmlOutput("location_team"), 
                        uiOutput("team_points"),
                        tags$strong(htmlOutput("team_label")),
                        tags$br()
                 ),
                 
                 #Opponent's Data
                 column(width = 6, align="center", 
                        uiOutput("opponent"), 
                        htmlOutput("location_opponent"),
                        uiOutput("opponent_points"),
                        tags$strong(htmlOutput("opponent_label")),
                        tags$br()
                 )
                 
               )
               
        ) #tab box1
      ), #tab item 1
      
      
      
      #edit the second tab
      tabItem(tabName = 'women',
              h2('test')
      ) #tab item 2    
      
    ) #tab itemS
  )#body
)#ui




#-----------------------------------------------------------------------------


server <- shinyServer(function(input, output, session){
  
  
  #data processing 
  
  
  #-----------------------------------------------------------------------------
  
  #team selection 
  output$fighting <- renderText({
    paste('The selected fighting team is: ', input$fighting)
    
  })
  
  output$opponent <- renderText({
    input$submit
    isolate(paste('The selected opponent team is: ', input$opponent))
    
  })
  
  
  #-----------------------------------------------------------------------------
  
  
  #Code to enable users to select a date (to look at a specific game)
  output$date <- renderUI({
    
    req(input$fighting)
    
    choices <- complete_pred_tbl_2021 %>% 
      filter((team1 == input$fighting) | (opponent == input$fighting)) %>% 
      select(DateofGame, team1, opponent) %>% 
      arrange(DateofGame) %>%
      mutate(Date = as.character(DateofGame)) %>% 
      unique()
    
    pickerInput(
      inputId = 'date_filter',
      label = 'Select Game', 
      choices = choices$Date, 
      choicesOpt = list(style = rep(('color: black; background: deepskyblue3'),100), subtext = paste(choices$team1, choices$opponent, sep = " vs ")),
      multiple = F)
    
  })
  
  
  #the below code filters the data set that we will be using for this dashboard
  
  city <- reactive({
    
    validate(
      need(input$fighting, ""),
      need(input$date_filter, "")
    )
    
    complete_pred_tbl_2021 %>% 
      
      filter((team1 == input$fighting) | (opponent == input$fighting)) %>% 
      
      select(DateofGame, Location, Loc, team1, opponent, Ranking, Opp_Ranking, win_loss, predictions_2021) %>%
      
      filter(DateofGame == input$date_filter)
    
  })
  
  
  
  
  #code to add the name of the selected team
  output$team <- renderUI({
    
    tags$h1(city()$team1, style = "color:white")
    
  })
  
  #code to display the name of the opponent team
  output$opponent <- renderUI({
    
    tags$h1(city()$opponent, style = "color:white")
    
  })
  
  
  #Location
  output$location_team <- renderUI({
    
    if(city()$Loc == 0){
      a <- paste('<span style=color:lightblue>', 'Away', '</span>')
      
    }else{
      a <- paste('<span style=color:lightblue>', 'Neutral', '</span>')
      
    }
    tags$h4(HTML(a))
    
  })
  
  
  #Opponent location
  output$location_opponent <- renderUI({
    
    if(city()$Loc == 0){
      
      a <- paste("<span style=color:salmon>", "Home", "</span>")
      
    }else{
      
      a <- paste('<span style=color:lightblue>', 'Neutral', '</span>')
      
    }
    
    tags$h4(HTML(a))
    
  })
  
  
  
  #Final points scored for the selected team
  output$team_points <- renderUI({
    
    if(city()$win_loss == 'w'){
      a <- tags$h3(city()$predictions_2021)
      
    }else{
      a <- tags$br()
      
    }
    
  })
  
  #Final points scored for the opponent
  output$opponent_points <- renderUI({
    
    if(city()$win_loss == 'w'){
      a <- tags$br()
      
    }else{
      a <- tags$h3(city()$predictions_2021)
      
    }    
    
  })
  
  
  
  #Winner Label
  output$team_label <- renderUI({
    
    if(city()$win_loss == 'w'){
      a <- paste("<span style= background-color:seagreen; font-color:white>", " Winner ", "</span>")
      
    }else{
      a <- tags$br()
      
    }
    
    tags$h1(HTML(a))
    
  })
  
  
  #Same as above, but this time for the opponent team
  output$opponent_label <- renderUI({
    
    if(city()$win_loss == 'w'){
      a <- tags$br()
      
    }else{
      a <- paste("<span style= background-color:seagreen; font-color:white>", " Winner ", "</span>")
      
    }
    
    tags$h1(HTML(a))
    
  })
  
  
  
})


#-----------------------------------------------------------------------------


shinyApp(ui = ui, server = server)
