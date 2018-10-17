# input to clustering is the normalized Actual value that has been set between [-1, 1]

# pull varaibles set to be used for clustering
to_cluster = sector_product %>%
             select(Product.Business.Area, 
                    Major.Desk,
                    Product,
                    Source.Product,
                    normal.Actual)

# to_cluster_sample = sample_n(to_cluster, 4000)
# pulling a stratified sample of records based on Source.product in order to determine optimal number of clusters
to_cluster_sample = stratified(to_cluster,
                                 'Source.Product',
                                 0.1)

# Visualize in order to determine if the sample is a correct representation
# Distribution of Source products
ggplot(to_cluster, aes(Source.Product)) + geom_bar()
ggplot(to_cluster_sample, aes(Source.Product)) + geom_bar()

# should be based on actual Actual value (not the normal one - ha)
ggplot(to_cluster, aes(normal.Actual)) + geom_histogram()
ggplot(to_cluster_sample, aes(normal.Actual)) + geom_histogram()


# one-hot encode categorical variables
# factors are non-ordinal; thus simply using the dummies package
# https://www.analyticsvidhya.com/blog/2016/12/practical-guide-to-implement-machine-learning-with-caret-package-in-r-with-practice-problem/

dmy = dummyVars(" ~ .", data = to_cluster_sample, fullRank = TRUE)
data_transformed = data.frame(predict(dmy, newdata = to_cluster_sample))

data_transformed_full = data.frame(predict(dmy, newdata = to_cluster))

# k-means
# ---------------------
# to determine optimal number of clusters
# fviz_nbclust(data_transformed, kmeans, method = "gap_stat")

fviz_nbclust(data_transformed, kmeans, method = "wss") +
             geom_vline(xintercept = 4, linetype = 2)+
             labs(subtitle = "Elbow method")

# optimal number clusters (ELbow method) = 4
km_cluster = kmeans(data_transformed, 4)
km_cluster_full = kmeans(data_transformed_full, 4)

# fviz_cluster(km_cluster, data = data_transformed,
#              ellipse.type = "convex",
#              palette = "jco",
#              ggtheme = theme_minimal())

#add cluster number to original dataset
# ---------------------
sector_product_cluster = cbind(sector_product, Cluster = km_cluster_full$cluster)

# hierachial
# ---------------------
hc_cluster = data_transformed %>%
             scale %>%
             dist(method = 'euclidean') %>%
             hclust(method = 'ward.D2')

# run PCA
# ---------------------
# compute distance (correlation-based distance)
# dist = get_dist(data_transformed, 
#                 method = 'pearson')
# 
# # to ensure all data in numeric in order to perform PCA
# str(data_transformed)
# 
# PCA = prcomp(data_transformed, 
#              scale = TRUE)
# names(PCA)
# 
# # data_transformed = x variables
# # PCA = y variables (reduction of n varaibles)
# std_dev = PCA$sdev
# PCA_var = std_dev^2
# 
# prop_var_ex = PCA_var/sum(PCA_var)
# prop_var_ex[1:20]
# 
# plot(prop_var_ex[1:100], x = 1:100,
#      xlab = "Principal Component",
#      ylab = "Proportion of Variance Explained",
#      type = "b")
# 
# plot(cumsum(prop_var_ex), xlab = "Principal Component",
#      ylab = "Cumulative Proportion of Variance Explained",
#      type = "b")
# 
# 
