# Recommendation engine
# https://rpubs.com/jt_rpubs/285729
# --------------

# change df to correct format
# https://rpubs.com/mm-c/gather-and-spread
# --------------
# rdf = ratings dataframe
# rdf = sector_product %>% 
#       select(Legal.Entity.Subsidiary, Product, Actual) %>%
#       group_by(Legal.Entity.Subsidiary, Product) %>%
#       mutate(Actual = sum(Actual)) %>%
#       distinct()

# using log of Actual values
rdf = sector_product %>% 
  select(Legal.Entity.Subsidiary, Product, log.Actual) %>%
  group_by(Legal.Entity.Subsidiary, Product) %>%
  mutate(log.Actual = sum(log.Actual)) %>%
  distinct()

rdf = spread(rdf, Product, log.Actual)

# set ClientKey as row indices
rownames(rdf) = rdf$Legal.Entity.Subsidiary
rdf$Legal.Entity.Subsidiary = NULL # as explicit column in df is not necessary

# analysis
# --------------
rdf$count.Products = rowSums(!is.na(rdf)) # count of products per clientkey
dim(rdf)

summary(rdf$count.Products)
# 6696 * 68 # potential number of ratings
# 6696 * mean(count.Products) 'actual density' = actual number of ratings

# number of products per client
ggplot(rdf, aes(x = count.Products)) + geom_histogram(binwidth = 1) +
  labs(title = 'Count of Products',
       x = 'Count of Products',
       y = 'Count of Legal Entities')

rdf$count.Products = NULL # drop column to reduce df size

# normalize: scale between 0 & 10
# --------------

# prior to normalization
summary(rdf)
summary(rdf$`ABSA CAPITAL`)

# rescale
rdf_re = data.frame(lapply(rdf, function(x) scales::rescale(x, to = c(0, 10)))) # rescale
colnames(rdf_re) = colnames(rdf) # keep correct format of colnames (no . for spaces/ ()/ -)

summary(rdf_re$`ABSA CAPITAL`)


#validate scale
# --------------
min(rdf_re[][], na.rm = TRUE)
max(rdf_re[][], na.rm = TRUE)

# visualization
# *** convert to ggplot!

hist(as.vector(as.matrix(rdf_re)), main = "Distribution of Ratings",
     col = "yellow", xlab = "Ratings")

boxplot(as.vector(as.matrix(rdf_re)), col = "yellow", 
        main = "Distribution of Ratings", ylab = "Ratings")

average_ratings_per_entity = rowMeans(rdf_re, na.rm = TRUE)
# compare average ratings to actual revenus to 

hist(average_ratings_per_entity, main = "Distribution of the average rating per Entity",
     col = "yellow")

rm(average_ratings_per_entity)

# creat test & training substes
# --------------
rmat = as.matrix(rdf_re)

# convert matrix to a recommenderlab realRatingMatrix
rmat = as(rmat,"realRatingMatrix")

# split the data into the training and the test set
# tune parameters - max & min for number of products per client
e = evaluationScheme(rmat, 
                     method ="split", 
                     train = 0.75, 
                     goodRating = 0, 
                     given = -1)

# algorithms
# user-based collaborative filtering
# --------------
#train UBCF cosine similarity models
# non-normalized
UBCF_N_C = Recommender(getData(e, "train"), "UBCF", 
                        param=list(normalize = NULL, method="Cosine"))

# centered
UBCF_C_C = Recommender(getData(e, "train"), "UBCF", 
                        param=list(normalize = "center",method="Cosine"))

# Z-score normalization
UBCF_Z_C = Recommender(getData(e, "train"), "UBCF", 
                        param=list(normalize = "Z-score",method="Cosine"))


# compute predicted ratings
# type = ratings = ratings returns only predicted ratings
# type = ratingMatrix = completed rating matrix

p1 = predict(UBCF_N_C, 
             getData(e, "known"), 
             type="ratings") # ratings returns only predicted ratings

p2 = predict(UBCF_C_C, 
             getData(e, "known"), 
             type="ratings")

p3 = predict(UBCF_Z_C, 
             getData(e, "known"), 
             type="ratings")

# set all predictions that fall outside the valid range to the boundary values
p1@data@x[p1@data@x[] < 0] = 0
p1@data@x[p1@data@x[] > 10] = 10

p2@data@x[p2@data@x[] < 0] = 0
p2@data@x[p2@data@x[] > 10] = 10

p3@data@x[p3@data@x[] < 0] = 0
p3@data@x[p3@data@x[] > 10] = 10

# aggregate the performance statistics
error_UCOS = rbind(
  UBCF_N_C = calcPredictionAccuracy(p1, getData(e, "unknown")),
  UBCF_C_C = calcPredictionAccuracy(p2, getData(e, "unknown")),
  UBCF_Z_C = calcPredictionAccuracy(p3, getData(e, "unknown"))
)
kable(error_UCOS)

boxplot(as.vector(as(p1, "matrix")), col = "yellow", 
        main = "Distribution of Predicted Values for UBCF Z-Score/Cosine Model", 
        ylab = "Ratings")

hist(as.vector(as(p3, "matrix")), 
     main = "Distrib. of Predicted Values for UBCF Z-Score/Cosine Model", col = "yellow", 
     xlab = "Predicted Ratings")

# train UBCF Euclidean distance models
# non-normalized
UBCF_N_E = Recommender(getData(e, "train"), "UBCF", 
                        param=list(normalize = NULL, method="Euclidean"))

# centered
UBCF_C_E = Recommender(getData(e, "train"), "UBCF", 
                        param=list(normalize = "center",method="Euclidean"))

# Z-score normalization
UBCF_Z_E = Recommender(getData(e, "train"), "UBCF", 
                        param=list(normalize = "Z-score",method="Euclidean"))

# compute predicted ratings
p1 = predict(UBCF_N_E, getData(e, "known"), type="ratings")

p2 = predict(UBCF_C_E, getData(e, "known"), type="ratings")

p3 = predict(UBCF_Z_E, getData(e, "known"), type="ratings")

# set all predictions that fall outside the valid range to the boundary values
p1@data@x[p1@data@x[] < 0] = 0
p1@data@x[p1@data@x[] > 10] = 10

p2@data@x[p2@data@x[] < 0] = 0
p2@data@x[p2@data@x[] > 10] = 10

p3@data@x[p3@data@x[] < 0] = 0
p3@data@x[p3@data@x[] > 10] = 10

# aggregate the performance statistics
error_UEUC = rbind(
  UBCF_N_E = calcPredictionAccuracy(p1, getData(e, "unknown")),
  UBCF_C_E = calcPredictionAccuracy(p2, getData(e, "unknown")),
  UBCF_Z_E = calcPredictionAccuracy(p3, getData(e, "unknown"))
)
kable(error_UEUC)

#train UBCF pearson correlation models
# non-normalized
UBCF_N_P = Recommender(getData(e, "train"), "UBCF", 
                        param=list(normalize = NULL, method="pearson"))

# centered
UBCF_C_P = Recommender(getData(e, "train"), "UBCF", 
                        param=list(normalize = "center",method="pearson"))

# Z-score normalization
UBCF_Z_P = Recommender(getData(e, "train"), "UBCF", 
                        param=list(normalize = "Z-score",method="pearson"))

# compute predicted ratings
p1 = predict(UBCF_N_P, getData(e, "known"), type="ratings")

p2 = predict(UBCF_C_P, getData(e, "known"), type="ratings")

p3 = predict(UBCF_Z_P, getData(e, "known"), type="ratings")

# set all predictions that fall outside the valid range to the boundary values
p1@data@x[p1@data@x[] < 0] = 0
p1@data@x[p1@data@x[] > 10] = 10

p2@data@x[p2@data@x[] < 0] = 0
p2@data@x[p2@data@x[] > 10] = 10

p3@data@x[p3@data@x[] < 0] = 0
p3@data@x[p3@data@x[] > 10] = 10

# aggregate the performance statistics
error_UPC = rbind(
  UBCF_N_P = calcPredictionAccuracy(p1, getData(e, "unknown")),
  UBCF_C_P = calcPredictionAccuracy(p2, getData(e, "unknown")),
  UBCF_Z_P = calcPredictionAccuracy(p3, getData(e, "unknown"))
)
kable(error_UPC)

# memory cleanup
rm(UBCF_N_C, UBCF_C_C, UBCF_Z_C,
   UBCF_N_E, UBCF_C_E, UBCF_Z_E,
   UBCF_N_P, UBCF_C_P, UBCF_Z_P)

#train IBCF cosine similarity models
# non-normalized
IBCF_N_C = Recommender(getData(e, "train"), "IBCF", 
                        param=list(normalize = NULL, method="Cosine"))

# centered
IBCF_C_C = Recommender(getData(e, "train"), "IBCF", 
                        param=list(normalize = "center",method="Cosine"))

# Z-score normalization
IBCF_Z_C = Recommender(getData(e, "train"), "IBCF", 
                        param=list(normalize = "Z-score",method="Cosine"))

# compute predicted ratings
p1 = predict(IBCF_N_C, getData(e, "known"), type="ratings")

p2 = predict(IBCF_C_C, getData(e, "known"), type="ratings")

p3 = predict(IBCF_Z_C, getData(e, "known"), type="ratings")

# set all predictions that fall outside the valid range to the boundary values
p1@data@x[p1@data@x[] < 0] = 0
p1@data@x[p1@data@x[] > 10] = 10

p2@data@x[p2@data@x[] < 0] = 0
p2@data@x[p2@data@x[] > 10] = 10

p3@data@x[p3@data@x[] < 0] = 0
p3@data@x[p3@data@x[] > 10] = 10

# aggregate the performance statistics
error_ICOS = rbind(
  IBCF_N_C = calcPredictionAccuracy(p1, getData(e, "unknown")),
  IBCF_C_C = calcPredictionAccuracy(p2, getData(e, "unknown")),
  IBCF_Z_C = calcPredictionAccuracy(p3, getData(e, "unknown"))
)

kable(error_ICOS)

#train IBCF Euclidean Distance models
# non-normalized
IBCF_N_E = Recommender(getData(e, "train"), "IBCF", 
                        param=list(normalize = NULL, method="Euclidean"))

# centered
IBCF_C_E = Recommender(getData(e, "train"), "IBCF", 
                        param=list(normalize = "center",method="Euclidean"))

# Z-score normalization
IBCF_Z_E = Recommender(getData(e, "train"), "IBCF", 
                        param=list(normalize = "Z-score",method="Euclidean"))

# compute predicted ratings
p1 = predict(IBCF_N_E, getData(e, "known"), type="ratings")

p2 = predict(IBCF_C_E, getData(e, "known"), type="ratings")

p3 = predict(IBCF_Z_E, getData(e, "known"), type="ratings")

# set all predictions that fall outside the valid range to the boundary values
p1@data@x[p1@data@x[] < 0] = 0
p1@data@x[p1@data@x[] > 10] = 10

p2@data@x[p2@data@x[] < 0] = 0
p2@data@x[p2@data@x[] > 10] = 10

p3@data@x[p3@data@x[] < 0] = 0
p3@data@x[p3@data@x[] > 10] = 10

boxplot(as.vector(as(p3, "matrix")), col = "yellow", 
        main = "Distribution of Predicted Values for IBCF_Z_E Model", 
        ylab = "Ratings")

hist(as.vector(as(p3, "matrix")), 
     main = "Distrib. of Predicted Values for IBCF_Z_E Model", col = "yellow", 
     xlab = "Predicted Ratings")

# aggregate the performance statistics
error_IEUC = rbind(
  IBCF_N_E = calcPredictionAccuracy(p1, getData(e, "unknown")),
  IBCF_C_E = calcPredictionAccuracy(p2, getData(e, "unknown")),
  IBCF_Z_E = calcPredictionAccuracy(p3, getData(e, "unknown"))
)
kable(error_IEUC)

#train IBCF pearson correlation models
# non-normalized
IBCF_N_P = Recommender(getData(e, "train"), "IBCF", 
                        param=list(normalize = NULL, method="pearson"))

# centered
IBCF_C_P = Recommender(getData(e, "train"), "IBCF", 
                        param=list(normalize = "center",method="pearson"))

# Z-score normalization
IBCF_Z_P = Recommender(getData(e, "train"), "IBCF", 
                        param=list(normalize = "Z-score",method="pearson"))

# compute predicted ratings
p1 = predict(IBCF_N_P, getData(e, "known"), type="ratings")

p2 = predict(IBCF_C_P, getData(e, "known"), type="ratings")

p3 = predict(IBCF_Z_P, getData(e, "known"), type="ratings")

# set all predictions that fall outside the valid range to the boundary values
p1@data@x[p1@data@x[] < 0] = 0
p1@data@x[p1@data@x[] > 10] = 10

p2@data@x[p2@data@x[] < 0] = 0
p2@data@x[p2@data@x[] > 10] = 10

p3@data@x[p3@data@x[] < 0] = 0
p3@data@x[p3@data@x[] > 10] = 10

# aggregate the performance statistics
error_IPC = rbind(
  IBCF_N_P = calcPredictionAccuracy(p1, getData(e, "unknown")),
  IBCF_C_P = calcPredictionAccuracy(p2, getData(e, "unknown")),
  IBCF_Z_P = calcPredictionAccuracy(p3, getData(e, "unknown"))
)
kable(error_IPC)

# memory clean up
rm(IBCF_N_C, IBCF_C_C, IBCF_Z_C,
   IBCF_N_E, IBCF_C_E, IBCF_Z_E,
   IBCF_N_P, IBCF_C_P, IBCF_Z_P)

# combine all results
c_res = data.frame(rbind(error_UCOS, error_UEUC, error_UPC, error_ICOS, error_IEUC, error_IPC))
c_res = c_res[order(c_res$RMSE ),]
kable(c_res)

# las = 3: rotate x axis labels to perendicular; las = 1: rotate y axis labels
barplot(c_res$RMSE, col = "yellow", 
        main = "Barplot of Model RMSE's", 
        las = 2, ylab = "RMSE", horiz = FALSE, names.arg = rownames(c_res), cex.names=.8)


# ---------------------------------------
# Return likely products 
# ---------------------------------------
# opportunities
opportunities = predict(IBCF_N_E, 
                        rmat,
                        type = 'ratings')
opportunities = as.data.frame(as.matrix(opportunities@data))

colnames(opportunities) = colnames(rdf)

# unscale predicted ratings
# ---------------------------------------
unscale_pred = function(rdf, opp){
  
  unscaled = rdf
  products = colnames(rdf)
  # perhaps make 0 == NA?
  
  for(i in 1:length(products)){
    product = products[i]
    
    # unscaled[i] = numeric()
    # i = 1
    min = min(rdf[i], na.rm = TRUE)
    max = max(rdf[i], na.rm = TRUE)
    
    unscaled[i] = sapply(opp[i], function(x) (10^((x/10)*(max-min)+min)))
  }
  return(unscaled)
}

unscaled = unscale_pred(rdf, opportunities)
# add actual values & predicted values to one df

opportunities$Entity = rownames(rdf)
opportunities = opportunities %>%
  select(Entity, everything())

# correct format of df
round_df = function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}
opportunities = round_df(opportunities, 3)


# whitespaces = as.data.frame(as.matrix(rmat@data))
whitespaces = rdf_re
whitespaces$Entity = rownames(rdf)
whitespaces = whitespaces %>%
  select(Entity, everything())

total_cols = length(whitespaces)
total_rows = length(whitespaces$Entity)
true_false = as.data.frame(is.na(whitespaces))

for(i in 1:total_rows){
  for(j in 2:total_cols){
    if(true_false[i, j] == FALSE){
      opportunities[i,j] = 'GOT IT'
    }
  }
}



# to note: df now has converted numeric to char to include the 'GOT'

# ---------------------------------------
# return opportunities per individual customer
# ---------------------------------------
cust_opp_full = opportunities %>%
           gather(Product, pred.Rating, -Entity) %>%
           arrange(desc(pred.Rating))

cust_opp_full$Entity = as.factor(cust_opp_full$Entity)
cust_opp_full$Product = as.factor(cust_opp_full$Product)


# join to determine product metadata: product selection
# optimise with join? 
cust_opp_full$Product.Selection = product_metadata$Product.Selection[match(cust_opp_full$Product, 
                                                                           product_metadata$Product)]



# memory clean-up
rm(e, p1, p1df, p2, p3)



