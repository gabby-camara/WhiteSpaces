# summary of the data
# ---------------------
summary(sector_product)

# min & max per (source) product
# ---------------------
min_max_product = sector_product %>%
                  group_by(Product, Source.Product) %>%
                  summarise(min.Product = min(Actual), 
                            max.Product = max(Actual)) %>%
                  arrange(Product)

p3 = ggplot(sector_product, aes(x=Product, y=Actual)) + 
geom_boxplot()

ggplotly(p3)
# to determine outliers? 
# balanced (for every negative there is a positive)

scale = scale(sector_product$Actual)

# count of sectors
p1 = sector_product %>%
     plot_ly(x = ~Client.Sector) %>%
     add_histogram()
p1

# count of bsuiness areas
p2 = sector_product %>%
     plot_ly(x = ~Major.Desk) %>%
     add_histogram()
p2

