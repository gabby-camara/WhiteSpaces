# conditional probability
# ---------------------------
# objective = grouping similar product groupings from sales data
# https://rpubs.com/ashutoshnanda/salesdata-marketbasketanalysis

# not going to consider Sector/ Segment; as some clients exist >1
rdf = sector_product %>% 
  select(Legal.Entity.Subsidiary, Product, Actual) %>% #Segment, Client.Sector,
  group_by(Legal.Entity.Subsidiary, Product) %>%
  mutate(Actual = sum(Actual)) %>%
  unique()

rdf = spread(rdf, Product, Actual)

# rdf = rdf %>% filter(Segment == 'Agriculture')

# convert to spare matrix
# convert df to 0/1 - correct format
# ---------------------------
product_df = rdf

rownames(product_df) = product_df$Legal.Entity.Subsidiary
product_df$Legal.Entity.Subsidiary = NULL #drop Legal Entity col
product_df$Segment = NULL
product_df$Client.Sector = NULL


product_df[is.na(product_df)] = 0
product_df[product_df > 0] = 1
product_df[product_df < 0] = 1

# product_df = product_df[,colSums(product_df) > 0]
# to be included if considered sector view only!

head(product_df[, 1:10], n = 10)
# notes: looks sparse; client per row, product holding in column (where = 1)


# probability of product over dataset 
# to make dynamic! if considered on dashboard
# ---------------------------
prob_occurence = function(df) {
  probabilities = sapply(colnames(df), function(x) {
    sum(df[[x]]) / nrow(df)
  })
}

probabilities = prob_occurence(product_df)
probabilities = as.data.frame(probabilities) # convert to df in order to use ggplot

# hist(probabilities, main = "Probabilities for Items", xlab = "Percentage of Transactions", ylab = "Count")
ggplot(probabilities, aes(x = probabilities)) + geom_histogram() + 
  labs(title = 'Probabilities of Products Bought',
       x = 'Percentage of Probability',
       y =' Count of Products')

# create conditional probabilities
# ---------------------------

# filter for clients who has the product as specified in the function
filter_prod = function(df, item) {
  df.subset = df[df[[item]] == 1, ]
  return(df.subset)
}

# build conditional matrix
conditional_matrix = function(df, preserve.diagonal = TRUE) {
  items = colnames(df)
  matrix = data.frame()
  for(i in 1:length(items)) {
    item = items[i]
    filtered.df = filter_prod(df, item)
    cond_prob_item = prob_occurence(filtered.df)
    for(j in 1:length(cond_prob_item)) {
      matrix[i, j] = cond_prob_item[j]
      if(i == j && !preserve.diagonal) {
        matrix[i, j] = 0
      }
    }
  }
  colnames(matrix) = items
  rownames(matrix) = items
  return(data.matrix(matrix))
}

matrix = conditional_matrix(product_df)
head(matrix[1:5, 1:5], n = 20)

# heatmap in order to see hotspot of a matrix
# change colour pallete - Absa
absa_clrs = c("#FFFFFF", "#F05A78", "#FF780F", "#F52D28", "#DC0032", "#870A3C")

colors = colorRampPalette(c("white","red"))(256) # think the colours may be inverted!
colors = colorRampPalette(absa_clrs)(256)
map = heatmap(t(coincidence.matrix), col = colors, symm = TRUE, main = "Heatmap of Conditional Probabilities for the Items")# , 
               # xlab = "Item Number", ylab = "Item Number")

# PCA
# ---------------------------

matrix_PCA = conditional_matrix(product_df, preserve.diagonal = FALSE)
matrix_PCA = prcomp(matrix_PCA, center = FALSE, scale = FALSE)


# coincidence.matrix = conditional_matrix(df = product_df, preserve.diagonal = FALSE)
# matrix_PCA = prcomp(coincidence.matrix, center = FALSE, scale = FALSE)
cumulative_variance = cumsum(matrix_PCA$sdev^2 / sum(matrix_PCA$sdev^2))

plot(cumulative_variance, ylim = c(0, 1), type = "l", main = "Determining Acceptability of PCA", ylab = "Variance Retained", xlab = "Number of Principal Components")

head(cumulative_variance, n = 5)

number_components = 3 #chosen in order to visualise
matrix_PCA_reduced = matrix_PCA$x[,1:number_components] %*%
                      t(matrix_PCA$rotation[,1:number_components])

colors = colorRampPalette(c("white","royalblue"))(256);
heatmap(t(coincidence.matrix.reduced), col = colors, symm = TRUE, Rowv = map$rowInd, keep.dendro = FALSE, main = "Heatmap of Conditional Probabilities for the Items", xlab = "Item Number", ylab = "Item Number")

products_PCA = matrix_PCA$rotation[, 1:number_components]
head(products_PCA, n = 10)

# k-means clustering 
# -----------------------

fits = sapply(1:20, function(x) {
  clustering = kmeans(products_PCA, x, nstart = 50)
  return(clustering$tot.withinss)
})
plot(1:length(fits), fits, xlab = "Number of Clusters", ylab = "Sum of Intracluster Squared Error", 
     main = "Determining Optimal Number of Clusters")

# STHDA clustering!
# optimal number of clusters
fviz_nbclust(products_PCA, kmeans, method = "gap_stat")

# try 10 clusters
desired.fit = kmeans(products_PCA, 10, nstart = 50)

cluster.assignments = desired.fit$cluster
cluster.groups = lapply(unique(cluster.assignments), function(x) {
  indices = cluster.assignments == x
  return(dimnames(products_PCA)[[1]][indices])
})
print(cluster.groups)

library(scatterplot3d) #Plotting Library
scatterplot3d(x = products_PCA[, 1], y = products_PCA[, 2], z = products_PCA[, 3], color = desired.fit$cluster, main = "Visualization of k-means clustering", xlab = "First Principal Component", ylab = "Second Principal Component", zlab = "Third Principal Component")

