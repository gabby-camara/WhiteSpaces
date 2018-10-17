# visualization of MBA
# https://cran.r-project.org/web/packages/arulesViz/vignettes/arulesViz.pdf
# https://rpubs.com/hoakevinquach/Association-Rules

# initial analysis to determine product vs source.product
product_vs_source = sector_product %>%
                    distinct(Product, Source.Product) %>%
                    arrange(Product)


# convert dataframe into transaction data 
# all products that a client has in one line
# --------------------------

# think you need to convert clientkey to factor first!

product_data = ddply(sector_product,c('ClientKey'),
                         function(df1)paste(df1$Product,
                                            collapse = ","))

# ClientKey not needed further for investigation; thus set to NULL
product_data$ClientKey = NULL

# Renames col to Products
colnames(product_data) = c('Products')
# data has now been converted to the basket format

# Convert basket format -> csv -> transaction class
# --------------------------
# trObj<-as(dataframe.dat,"transactions")
write.csv(product_data,"F:/NYDSA/WhiteSpaces 2.0/shiny/data/product_data_MBA.csv", quote = FALSE, row.names = TRUE)

trans_products = read.transactions('F:/NYDSA/WhiteSpaces 2.0/shiny/data/product_data_MBA.csv', 
                                   format = 'basket', 
                                   sep=',')

trans_products
summary(trans_products)

# num_purchased_prod = 0.0006038995*8029*8097
# NB: to display the distribution of num of items & num of transactions

# display Frequency Bar Plot
# --------------------------
itemFrequencyPlot(trans_products,
                  topN = 20,
                  type = "absolute",
                  main = 'Absolute Frequency Plot')
# may alos have type = relative (frequency of product relative to others)

# mine association rules
# --------------------------

association.rules = apriori(trans_products, 
                            parameter = list(supp = 0.01, 
                                             conf = 0.8,
                                             maxlen = 10))
# association_df = as(association.rules, "data.frame")

summary(association.rules)
length(association.rules) # 7 551 565 rules with supp = 0.01; now only 81 rules with supp = 0.1

inspect(association.rules[1:20])

# sort by lift
rules_lift = sort(association.rules, 
                  by = 'lift',
                  decreasing = TRUE)
inspect(rules_lift[1:20])

# visualizing
# --------------------------
top10subRules = head(association.rules, n = 10, by = "confidence")
top20subRules = head(association.rules, n = 10, by = "lift")

plot(top20subRules, method = "graph",  engine = "htmlwidget")

subRules2 = head(association.rules, n=20, by="lift")
plot(top20subRules, method="paracoord")

# remove redundant rules (rules whihc are subsets of larger rules)
# --------------------------
subset.rules =  which(colSums(is.subset(association.rules, association.rules)) > 1) # get subset rules in vector
length(subset.rules)  #> 64
subset.association.rules. = association.rules[-subset.rules] # remove subset rules.

inspect(subset.association.rules.)

subRules2 = head(subset.rules, n=20, by="lift")
plot(subset.association.rules., method="paracoord")
plot(subset.association.rules., method = "graph",  engine = "htmlwidget")

# focuisng on one product on lhs
# --------------------------
overdraft.rules <- apriori(trans_products, 
                                  parameter = list(supp=0.001, conf=0.8),
                                  appearance = list(lhs=c("CHEQUE ACCOUNTS", 'CHEQUE FEES', 'OVERDRAFTS (CHEQUE)'),
                                                    default="rhs"))

subRules3 = head(overdraft.rules, n=20, by="lift")
plot(subRules3, method="paracoord")
inspect(overdraft.rules)


write(overdraft.rules, file = "groceryrules.csv", sep = ",", quote = TRUE, row.names = FALSE)
