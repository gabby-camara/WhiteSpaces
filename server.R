library(shiny)
library(shinydashboard)
library(DT)


server = function(input, output) {
  
  #names(opportunities) = sprintf('<div style="transform:rotate(-90deg);margin-top:30px;">%s</div>', names(df))
  #opportunities = datatable(opportunities, escape = FALSE)
  
  
  # names(opportunities) <- sprintf('<div style="transform:rotate(-90deg);margin-top:30px;">%s</div>', names(opportunities))
  # dt <- datatable(opportunities, escape = FALSE)
  # htmlwidgets::saveWidget(dt, tf<-tempfile(fileext = ".html"))
  # shell.exec(tf)
  # 
  # visualization of clustering tables
  output$opportunities = DT::renderDataTable(
    datatable(opportunities,
    class = 'cell-border stripe',
    # filter = 'input',
    options = list(scrollX = TRUE)
    ) %>%
      formatStyle(names(opportunities), backgroundColor = styleEqual(c("GOT IT"), c('lightblue'))) 
      # formatStyle('ABSA.CAPITAL', color = 'red', backgroundColor = 'orange', fontWeight = 'bold')
  )
  
  # DT for detailed customer view
  output$customer = DT::renderDataTable(
    datatable(cust_opp) %>%
      formatStyle('pred.Rating',
                  backgroundColor = styleEqual(c("GOT IT"), c('lightblue')))
  )
  
  #creating the valueBoxOutput requirements
  good_rating = 5
  core_opp = nrow(filter(cust_opp, pred.Rating == "GOT IT"))
  pred_opp = nrow(filter(cust_opp, pred.Rating > good_rating)) - core_opp
  banker = 'G CAMARA'
  
  # creating the valueBoxOutput values
  
  # % core opportunities utilised
  output$value1 <- renderValueBox({
    valueBox(
      formatC(core_opp, format="d", big.mark=','),
      paste('% Core Porducts Util: ',round(core_opp/68 * 100), 2, '%'),
      icon = icon("hand-holding-usd"),
      color = "purple")  
  })
  
  # number core opportunities > threshold of predicted rating
  output$value2 <- renderValueBox({ 
    valueBox(
      formatC(pred_opp, format="d", big.mark=','),
      'Total Predicted Product Opportunities',
      icon = icon("arrow-up"),
      color = "green")  
  })
  
  output$value3 <- renderValueBox({
    valueBox(
      formatC(banker, format="d", big.mark=','),
      paste('Banker'),
      icon = icon("piggy-bank"),
      color = "yellow")   
  })
  
  output$value4 <- renderValueBox({
    valueBox(
      formatC('Public Sector', format="d", big.mark=','),
      paste('Sector'),
      icon = icon("piggy-bank"),
      color = "blue")   
  })
  
  # revenue graph per desk based on customer view

 

}

