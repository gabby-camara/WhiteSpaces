# Use Case: Agriculture
# To note: functions & data reductions should be made generic (chsoen via user on dashboard)
# -----------------------

agriculture = sector_product %>% filter(Client.Sector == 'Agriculture')

# In order to view sector by size of business area products
area_summary = agriculture %>% group_by(Product.Business.Area) %>%
                        summarise(count.BusinessArea = n())

# To view number of products & value of products by parent **SORT BY 
parent_summary = agriculture %>% group_by(Parent) %>%
                        summarise(count.Products = n(), Actual.Value = sum(Actual)) %>%
                          mutate(freq.Products = count.Products/ sum(count.Products)*100,
                                 freq.Value = Actual.Value/ sum(Actual.Value)*100)

# product selection by parent
product_summary = agriculture %>% group_by(Parent, Major.Desk, Product) %>%
                    summarise(Actual.Value = sum(Actual))
                    

# visualizations
# -----------------------
area_treemap = treemap(area_summary,
                       index = 'Product.Business.Area',
                       vSize = 'count.BusinessArea',
                       type = 'index')

parent_graph = plot_ly(parent_summary, x = ~Parent, y = ~freq.Products, name = 'Count of Products') %>%
                add_trace(y = ~freq.Value, name = 'Value of Products')

product_graph = plot_ly(product_summary, x = ~Product, y = ~Major.Desk, mode = 'markers',
                        type = 'scatter', size = ~Actual.Value, color = ~Parent,
                        marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
                  layout(showlegend = TRUE)


