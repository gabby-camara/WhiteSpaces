# ui (front end)
# -------------------
library(shiny)
library(shinydashboard)

# header
# -------------------
header <- dashboardHeader(
  title = 'White Spaces 2.0'
)


# sidebar
# -------------------
sidebar <- dashboardSidebar(
  # disable = TRUE
  sidebarMenu(
    menuItem("Overall", 
             tabName = "dashboard", 
             icon = icon("dashboard")),
    menuItem("Detailed Customer View", 
             tabName = "detail", 
             icon = icon("users"))
  )
)


# body
# -------------------
body <- dashboardBody(
  tabItems(
    # first tab = overal opporttunities df
    tabItem('dashboard',
            DT::dataTableOutput("opportunities")),
    
    # second tab = detailed view (entity)
    tabItem('detail',
            fluidRow(
              valueBoxOutput("value1"),
              valueBoxOutput("value2"),
              valueBoxOutput("value3")
            ),
            DT::dataTableOutput("customer"))
  )
  
)


# function to call
# -------------------
ui = dashboardPage(header, sidebar, body, skin = 'red')