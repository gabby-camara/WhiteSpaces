# Use Case: Agriculture
# To note: functions & data reductions should be made generic (chsoen via user on dashboard)
# -----------------------

agriculture = sector_product %>% filter(Client.Sector == 'Agriculture')

# In order to view sector by size of business area products
area_summary = agriculture %>% group_by(Product.Business.Area, Product.Selection) %>%
                        summarise(count.Product.Selection = n(),
                                  Actual.Value = sum(Actual)) %>%
                          mutate(freq.Products = count.Product.Selection/sum(count.Product.Selection)*100,
                                 freq.Value = Actual.Value/sum(Actual.Value)*100) %>% 
                            arrange(desc(count.Product.Selection))

# To view number of products & value of products by parent **SORT BY 
parent_summary = agriculture %>% group_by(Parent) %>%
                        summarise(count.Products = n(), Actual.Value = sum(Actual)) %>%
                          mutate(freq.Products = count.Products/ sum(count.Products)*100,
                                 freq.Value = Actual.Value/ sum(Actual.Value)*100)

# product selection by parent
product_summary = agriculture %>% group_by(Parent, Major.Desk, Product) %>%
                    summarise(Actual.Value = sum(Actual))
                    
# analysis of data
# -----------------------
data = agriculture %>% group_by(Product, Source.Product) %>%
        summarise(min.Value = min(Actual), max.Value = max(Actual))


# visualizations of area
# -----------------------
# area_summary$count.BusinessArea = c(21, 149, 327, 327, 327, 106, 106, 3537, 3537, 3537) # to automate

# area_treemap = treemap(area_summary,
#                        index = c('Product.Business.Area', 'Product.Selection'),
#                        vSize = 'count.Product.Selection',
#                        type = 'index')

area_graph = plot_ly(area_summary, x = ~Product.Selection, y = ~count.Product.Selection, type = 'bar', color = ~Product.Business.Area) %>%
              layout(title = 'Count of Products',
                     xaxis = list(title = 'Products'),
                     yaxis = list(title = 'Count'))
                    

area_graph_value = plot_ly(area_summary, x = ~Product.Selection, y = ~Actual.Value, type = 'bar', color = ~Product.Business.Area) %>%
                    layout(title = 'Total Value of Products',
                           xaxis = list(title = 'Products'),
                           yaxis = list(title = 'Total Value'))


# visualizations of parent & product
# -----------------------
parent_graph = plot_ly(parent_summary, x = ~freq.Products, y = ~Parent, name = 'Count of Products') %>%
                add_trace(x = ~freq.Value, name = 'Value of Products')

product_graph = plot_ly(product_summary, x = ~Major.Desk, y = ~Product, mode = 'markers',
                        type = 'scatter', size = ~Actual.Value, color = ~Parent,
                        marker = list(opacity = 0.5, sizemode = 'diameter' #, 
                        # width = 1000, height = 800
                        )) %>%
                    layout(showlegend = FALSE,
                           xaxis = list(tickangle = 90))


