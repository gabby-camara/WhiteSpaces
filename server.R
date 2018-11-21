library(shiny)
library(shinydashboard)
library(DT)
library(shinyBS)

server = function(input, output, session) {
  
  # create colour RGB ratings for predictions
  range = 1:9
  clrs = round(seq(176, 80, length.out = length(range) + 1), 0) %>% {paste0("rgb(", ., ",255,",., ")")}
  
  # observe event of sector being chosen
  observe({
    sector = make.names(input$`sector_selected`)
  })
  
  # pulling correct predicted opportunities dataframe based on sector
  react_sector = reactive({
    dataframe = eval(parse(text=paste('opportunities.', make.names(input$`sector_selected`), sep = "")))
    dataframe
  })
  
  # pulling correct metadata relating to selected sector
  react_sector_metadata = reactive({
    dataframe = sector_product %>% filter(Client.Sector == input$sector_selected)
    dataframe
  })
  
  # graph: revenue over time per desk
  # generae correct data
  react_sector_desk_spread = reactive({
    # spread data per date
    desk = react_sector_metadata() %>% select(8, 10:21) %>% gather(key, value, 2:13) %>% group_by(Major.Desk, key) %>% summarise(value = sum(value, na.rm = TRUE))
    desk$key = as.Date(desk$key) # convert to correct date
    desk
  })
  
  # render graph
  output$revenue_desk = renderPlot({
      ggplot(react_sector_desk_spread(), 
           aes(x = key, y = value, color = Major.Desk)) + geom_line() + geom_point() + 
      scale_y_continuous(labels = comma) +
      labs(title = 'Revenue per Month per Desk',
           x = 'Months',
           y = 'Revenue-To-Date',
           color = 'Desks')
  })

  
  # graph: top 10 revenue products per sector per 12 months (Actual value)
  # get correct data
  react_sector_product = reactive({
    # latest_date = 10 #`2018-08-31`
    sector = react_sector_metadata() %>% select(Product, Actual) %>% 
              group_by(Product) %>% summarise(Actual = sum(Actual), Count = n()) %>% arrange(desc(Actual))
    
    sector_display = sector %>% select(Product, Actual) %>% group_by(Product) %>% summarise(Actual = sum(Actual)) %>% arrange(desc(Actual)) %>% head(10)
    return(sector_display)
  })
  
  # render graph
  output$revenue_product = renderPlot({
    ggplot(react_sector_product(), 
           aes(x = reorder(Product, -Actual), y = Actual)) + geom_bar(stat = "identity", fill = "#E69F00") + 
    geom_text(aes(label = interaction(Product), y = 0), angle = 90, hjust = -.05, size = 3, vjust = 1.5, colour = "black") + 
    labs(title = 'Top 10 Revenue Products',
         x = 'Products',
         y = 'Average Revenue per Latest 12 Months') + 
    theme(axis.text.x=element_blank()) + 
    scale_y_continuous(labels = comma)
  })
  
  # graph: trend of top 10 products over time
  # get data
  react_sector_product_time = reactive({
    products = unique(react_sector_product()$Product)
    
    prod = react_sector_metadata() %>% filter(Product %in% products) %>% select(9, 10:21) %>% gather(key, value, 2:13) %>% group_by(Product, key) %>% summarise(value = sum(value, na.rm = TRUE))
    prod$key = as.Date(prod$key) # convert to correct date
    prod
  })
  
  # render graph
  output$revenue_product_time = renderPlot({
    ggplot(react_sector_product_time(), 
           aes(x = key, y = value, color = Product)) + geom_line() + geom_point() + 
      scale_y_continuous(labels = comma) +
      labs(title = 'Revenue per Month per Product',
           x = 'Months',
           y = 'Revenue-To-Date',
           color = 'Products')
  })
  
  # graph: ABC classification of entities per sector
  # correct data 
  react_sector_entity = reactive({
    ABC_sector = react_sector_metadata() %>% select(Legal.Entity.Subsidiary, ABC.Entity) %>% unique()
    ABC_sector
  })
  

  # render graph
  output$ABC_entity = renderPlot({
    ggplot(react_sector_entity(), aes(x = ABC.Entity)) + 
    geom_bar(fill = "#F52D28", width = 0.5) +
    geom_text(stat='count', aes(label=..count..), vjust = -0.5) +
    labs(title = "ABC Classfication per Entity",
         x = "ABC Classification Labels",
         y = "Count of Legal Entities")
  })
  
  # threshold for good predicted opportunities
  good_rating = 4

  # valueBoxes: KPIs per sector
  # currrent product holding
  output$total_products <- renderValueBox({
    valueBox(
      formatC(sum(react_sector()[, -c(1:3)] == 'GOT IT'), format="d", big.mark=','),
      paste('Total (Count) of Current Product Holding'),
      # paste('% Core Porducts Util: ',round(values$core_opp/68 * 100), 2, '%'),
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "red")  
  })
  
  # total legal entities
  output$total_entity <- renderValueBox({
    valueBox(
      formatC(paste(length(unique(react_sector_metadata()$Parent)), '|', nrow(react_sector())), format="d", big.mark=','),
      paste('Count of Parents | Legal Entities in Sector'),
      icon = icon("users"),
      color = "purple")  
  })
  
  # total parents (value moved to entity; stored for business potential)
  # output$total_parent <- renderValueBox({
  #   valueBox(
  #     formatC(length(unique(react_sector_metadata()$Parent)), format="d", big.mark=','),
  #     paste('Count of Parents in Sector'),
  #     icon = icon("users"),
  #     color = "purple")  
  # })
  
  # total products predicted > threshhold rating
  output$total_predictions <- renderValueBox({
    valueBox(
      formatC(sum(react_sector()[, -c(1:3)] > good_rating) - sum(react_sector()[, -c(1:3)] == 'GOT IT'), format = 'd', big.mark = ','),
      paste('Total (Count) of Predicted Product Opportunities'),
      # paste('% Core Porducts Util: ',round(values$core_opp/68 * 100), 2, '%'),
      icon = icon("arrow-up", lib = "glyphicon"),
      color = "orange")  
  })
  
  # revenue per sector for 12 month period
  output$total_value <- renderValueBox({
    valueBox(
      paste('R', formatC(sum(react_sector_metadata() %>% select(Actual)), format = 'd', big.mark = ',')),
      paste('Revenue to Date (12 Month-Average)'),
      icon = icon("credit-card", lib = "glyphicon"),
      color = "green")  
  })
  
  # table: predicted opportunities
  output$opportunities = DT::renderDataTable(
    datatable(
    react_sector(),
    class = 'cell-border stripe',
    options = list(scrollX = TRUE)
    ) %>%
      formatStyle(names(react_sector()[, -c(1:3)]), backgroundColor = styleInterval(range, clrs)) # intensity of opportunity = colour pixel
  )
  
  
  
  # table: DT for detailed customer view
  # observe events: bankers -> parent -> chosen entity
  observe({
    parent = input$parent_name
    entity = input$entity_name
    banker = input$banker_name
    
    choice_parent = metadata %>% filter(Banker == banker) %>% select(Parent) %>% unique()
    choice_entity = metadata %>% filter(Parent == parent) %>% select(Legal.Entity.Subsidiary)
    
    updateSelectInput(session, "entity_name", choices = choice_entity, selected = entity)
    updateSelectInput(session, "parent_name", choices = choice_parent, selected = parent)
    
  })
  
  # reactive df structures
  # subet CVP df to chosen CVP as selected by user
  react_CVP = reactive({
    CVP_selection %>% filter(CVPSector == input$CVP_chosen) 
  })
  
  # subset cust_opp df & include match to CVP offering
  react_Client = reactive({
    cust_df = client_opp_full %>% filter(Legal.Entity.Subsidiary == input$entity_name, pred.Rating  != 'Not Recommended') %>% drop_na()
    CVP_df = react_CVP()
    
    cust_df$CVP_Match = CVP_df$Offering[match(cust_df$Product.Selection,
                                              CVP_df$Product.Selection)]
    cust_df
  })
  
  
  # DT: opportunities per legal entity (client)
  output$customer = DT::renderDataTable(
    datatable(react_Client()) %>%
      formatCurrency('Revenue',currency = "R", interval = 3, mark = ",") %>%
      formatStyle('pred.Rating', backgroundColor = styleInterval(range, clrs))
  )
  
  # valueBox: KPIs per legal entity
  # reactive values for valueBoxes
  initial_core = nrow(filter(client_opp_full, pred.Rating == "GOT IT"))
  values = reactiveValues(core_opp = nrow(filter(client_opp_full, pred.Rating == "GOT IT")),
                          pred_opp = nrow(filter(client_opp_full, (pred.Rating > good_rating) & (pred.Rating != 'Not Recommended'))) - initial_core,
                          sector = 0,
                          revenue = 0)  
  
  observe({
    values$core_opp = nrow(filter(filter(client_opp_full, Legal.Entity.Subsidiary == input$entity_name), pred.Rating == "GOT IT"))
    values$pred_opp = nrow(filter(filter(client_opp_full, Legal.Entity.Subsidiary == input$entity_name), (pred.Rating > good_rating) & (pred.Rating != 'Not Recommended'))) - values$core_opp 
    values$sector = as.character(metadata$Client.Sector[metadata$Legal.Entity.Subsidiary == input$entity_name])
    values$revenue = sum(filter(sector_product, Legal.Entity.Subsidiary == input$entity_name)$Actual)
    
  })
  
  # creating the valueBoxOutput values
  # % core opportunities utilised
  output$value1 <- renderValueBox({
    valueBox(
      formatC(values$core_opp, format="d", big.mark=','),
      paste('Total (Count) of Current Product Holding'),
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "purple")  
  })
  
  # number core opportunities > threshold of predicted rating
  output$value2 <- renderValueBox({ 
    valueBox(
      formatC(values$pred_opp, format="d", big.mark=','),
      'Total Predicted Product Opportunities',
      icon = icon("arrow-up", lib = "glyphicon"),
      color = "blue")  
  })
  
  # sector client belongs to
  output$value3 <- renderValueBox({
    valueBox(
      formatC(values$sector, format="d", big.mark=','),
      paste('Client Sector'),
      icon = icon("map-marker", lib = "glyphicon"),
      color = "orange")   
  })
  
  # average revenue to date
  output$value4 <- renderValueBox({
    valueBox(
      paste('R', formatC(values$revenue, format="d", big.mark=',')),
      paste('Revenue to Date (12 month-average)'),
      icon = icon("credit-card", lib = "glyphicon"),
      color = "green")   
  })

}

