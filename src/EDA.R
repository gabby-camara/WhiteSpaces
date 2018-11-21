# absa colours
#870A3C: burgundy
#DC0032: dark red
#F52D28: light red
#FF780F: orange
#F05A78: pink

# cour brewer palette most closely linked to absa: Reds, RdGy

# understanding structure
# ------------------------
summary(sector_product)

# multiplot function
# ------------------------
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# bar chart: revenue over time
# ------------------------
revenue_date = sector_product %>% select(6, 10:21) %>% gather(key, value, 2:13, -Product.Business.Area) %>% group_by(Product.Business.Area, key) %>% summarise(total = sum(value, na.rm = TRUE))
revenue_date$key = as.Date(revenue_date$key)


ggplot(revenue_date) + geom_bar(aes(x = key, y = total, fill = Product.Business.Area), 
                                stat = 'identity'
                                ) +
                                # fill = '#870A3C') +
  scale_y_continuous(labels = comma) +
  labs(title = 'Total Revenue over 12 Months',
       x = 'Month',
       y= 'Revenue Value') + 
  scale_x_date(breaks = revenue_date$key) + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

rm(revenue_date)


# boxplot of actual (revenue) values
# ------------------------
p1 = ggplot(sector_product, aes(x = Product.Selection, y = Actual)) + geom_boxplot() + #actual values
      labs(title = 'BoxPlot of Actual Revenue',
           x = 'Product Selection (CVP)',
           y = 'Actual Revenue') + scale_y_continuous(labels = comma) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  
p2 = ggplot(sector_product, aes(x = Product.Selection, y = log.Actual)) + geom_boxplot() + # log values; why non-finite warning?
      labs(title = 'BoxPlot of log of Actual Revenue',
           x = 'Product Selection (CVP)',
           y = 'log of Actual Revenue') + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

multiplot(cols = 2, p1, p2)

rm(p1, p2)

# distribution of log values
# ------------------------
ggplot(sector_product, aes(x = log.Actual)) + geom_histogram(bins = 45) + 
  labs(title = 'Distribution of log of Actual Values',
       x = 'log(Actual Average revenue for 12 Months)',
       y = 'Count of Products per Legal Entity')

# ggplot(sector_product, aes(x = log.Actual, fill = Segment)) + geom_histogram(bins = 45) + scale_fill_manual(values = absaPalette)
# deteremined to use log of values in order to not overweight signifcant outliers. 


# latest month revenue to date per sector
# grid plot per sector
# ------------------------
#sector_chosen = 'Diversified'
#latest_date = 11

sector = sector_product %>% select(Segment, Product, `2018-08-31`) %>% 
          group_by(Segment,Product) %>% summarise(Count = n(), Actual = sum(`2018-08-31`)) %>% arrange(desc(Actual))

# top 10 products in sector
sector_display = sector %>% select(Product, Actual) %>% group_by(Product) %>% summarise(Actual = sum(Actual)) %>% arrange(desc(Actual)) %>% head(10)

ggplot(sector_display[1:10,], aes(reorder(Product, -Actual, sum), Actual)) + geom_bar(stat = "identity", fill = "#E69F00") + 
  geom_text(aes(label = Product, y = 0), angle = 90, hjust = -.05, size = 3, vjust = 1.5, colour = "black") +
  labs(title = 'Top 10 Revenue Products',
       x = 'Products',
       y = 'Revenue per Latest Month') + 
  theme(axis.text.x=element_blank())
  # theme(axis.text.x = element_text(angle = 45, hjust = 1))


# multiplot of ABC classification per segment
# ----------------------------
p = ggplot(sector_product)

draw_hist_plot = function(var){
  
  p = ggplot(sector_product %>% filter(Segment == var))
  p + geom_histogram(aes(x = ABC.Parent), stat = "count", fill = "#56B4E9") + labs(title = var)
  
}

segments = unique(sector_product$Segment)
plotlist = lapply(segments, function(x) {draw_hist_plot(x)})
multiplot(cols = 3, plotlist = plotlist)


# revenue over time per desk 
# ------------------------
desk = sector_product %>% select(8, 10:21) %>% gather(key, value, 2:13) %>%
          group_by(Major.Desk, key) %>% summarise(value = sum(value, na.rm = TRUE)) 

# reassign key values to actaul dates (factors aren't suited for geom_line())
# assumption that month = at month-end
desk$key = as.Date(desk$key)

ggplot(desk, aes(x = key, y = value, color = Major.Desk)) + 
  geom_line() + geom_point() +
  scale_y_continuous(labels = comma) + 
  labs(title = 'Revenue per Month per Desk',
       x = 'Months',
       y = 'Revenue-To-Date',
       color = 'Desks')

# min & max per product
# ------------------------
price_range_product = sector_product %>% select(Product, Actual) %>% group_by(Product) %>% summarise(min = min(Actual), max = max(Actual)) %>% mutate(Range = max -min)

kable(price_range_product)


# multiplot of Actual values per source product
# ----------------------------
products_list = sort(unique(sector_product_full$Product))
products = products_list[67:68]


draw_box_plot = function(var){
  p = ggplot(sector_product_full %>% filter(Product == var))
  p + geom_boxplot(aes(x = Source.Product, y = Actual, colour = Source.Product)) + labs(title = var) + scale_y_continuous(labels = comma) +
    theme(axis.text.x = element_blank())
    # theme(axis.text.x=element_text(angle=0,hjust=-1))
}

plotlist = lapply('OVERDRAFTS (CHEQUE)', function(x) {draw_box_plot(x)})
multiplot(cols = 2, plotlist = plotlist)

# ABC classification
ggplot(sector_product %>% select(ABC.Parent)) + geom_bar(aes(x = ABC.Parent), stat = 'count', fill = '#FF780F' ) + 
  labs(title = 'Count of ABC Classification per Parent')

# memory clean up
# ------------------------
rm(desk, p, plotlist, price_range_product, sector, sector_display)
