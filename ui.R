# ui (front end)
# -------------------
library(shiny)
library(shinydashboard)
library(shinyBS)
library(tableHTML)

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
  # convert css colours to Absa colours
  tags$style(
    type = 'text/css', 
    '.bg-purple {background-color: #870A3C!important; }',
    '.bg-orange {background-color: #FF780F!important; }',
    '.bg-green {background-color: #F05A78!important; }',
    '.bg-blue {background-color: #DC0032!important; }',
    '.bg-red {background-color: #F52D28!important; }'
  ),
  tags$head(tags$style(HTML(".small-box {height: 100px}"))),
  tags$style(make_css(list('.box', 
                           c('font-size', 'font-family', 'color'), 
                           c('16px', 'arial', 'grey')))),
  
  tabItems(
    # first tab = overal opporttunities df
    tabItem('dashboard',
          
          
          fluidRow(
            # drop down for segment
            box(width = 3,
                selectInput('sector_selected',
                            'Select Sector',
                            choices = sort(unique(sector_product$Client.Sector)),
                            selected = 'Agriculture')),
            # count of parents
            # valueBoxOutput("total_parent", width = 3),
            # definition
            box(width = 6,
                paste("Whitespaces provide information regarding Product Sectoral Offering.", "In addition, it recommends the 'best' next Product Offering and the associated potential Revenue.", sep="\n"),
                align = 'center',
                height = '100px')
           
          ),
            
          # Overall KPIs
          fluidRow(
              valueBoxOutput("total_entity", width = 3), 
              valueBoxOutput("total_products", width = 3),
              valueBoxOutput("total_value", width = 3),
              valueBoxOutput("total_predictions", width = 3)
            ),
          
          # tab boxes to chnage visualization options
          fluidRow(
          tabBox(
            # title = "First tabBox",
            width = 12,
            # The id lets us use input$tabset1 on the server to find the current tab
            id = "tabset1", # height = "250px",
            tabPanel("Graphs displaying Insight per Chosen Sector",
                     fluidRow(
                       column(6, plotOutput('revenue_product')),
                       column(6, plotOutput('ABC_entity'))),
                     fluidRow(
                       column(6, plotOutput('revenue_product_time')),
                       column(6, plotOutput('revenue_desk'))
                     )
                     ),
            tabPanel("Predicted Ratings Table per Chosen Sector", 
                     DT::dataTableOutput("opportunities")))
          ),
          
          # add tool tips
          bsTooltip(id = 'total_predictions', 
                    title = 'Number of Products with a predicted Rating greater than a threshold (>= 5).',
                    placement = 'top', 
                    trigger = 'hover'),
          
         bsTooltip(id = 'total_value', 
              title = 'Total Revenue over all Products averaged over the last 12 months.',
              placement = 'top', 
              trigger = 'hover'),
         
         bsTooltip(id = 'total_products', 
                   title = 'Count of current Product Holding',
                   placement = 'top', 
                   trigger = 'hover')
    
           ),
          
          
          # table with ratings overall
          #DT::dataTableOutput("opportunities")),
    
    # second tab = detailed view (entity)
    tabItem('detail',
            
            # provide inputs in order to generate correct view
            fluidRow(
              # choose banker
              box(width = 3,
                  selectInput('banker_name',
                              'Bankers Name',
                              choices = sort(metadata$Banker),
                              selected = 'HENDRIK VAN DER LINDE')
              ),
              # choose parent entity
              box(width = 3,
                selectInput('parent_name',
                            'Parent Name',
                            choices = sort(metadata$Parent),
                            selected = 'AFGRI GROUP'
                            )
              ),
              # choose legal entity
              box(width = 3,
              selectInput('entity_name',
                          'Legal Entities',
                          choices = sort(metadata$Legal.Entity.Subsidiary)
                          )
              ),
              # choose CVP
              box(width = 3,
                  selectInput('CVP_chosen',
                              'Selected CVP:',
                              choices = sort(CVP_selection$CVPSector),
                              selected = 'Agriculture')
            )),
            
          
            # KPI Value boxes
            fluidRow(
              valueBoxOutput("value1", width = 3),
              valueBoxOutput("value2", width = 3),
              valueBoxOutput("value3", width = 3),
              valueBoxOutput('value4', width = 3)
            ),
            
            # databale of rating details
            DT::dataTableOutput("customer"),
            DT::dataTableOutput('table_play'))
  )
)


# function to call
# -------------------
ui = dashboardPage(header, sidebar, body, skin = 'red')