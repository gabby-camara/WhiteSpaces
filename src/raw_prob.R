# raw probability score 
# all non-NA revenue figures = 1

# using Actual Values
rdf = sector_product %>% 
  select(Legal.Entity.Subsidiary, Product, Actual) %>%
  group_by(Legal.Entity.Subsidiary, Product) %>%
  mutate(Actual = sum(Actual)) %>%
  distinct()

rdf$Actual = 1 # populate raw prob score
rdf = spread(rdf, Product, Actual)

# set ClientKey as row indices
rownames(rdf) = rdf$Legal.Entity.Subsidiary
rdf$Legal.Entity.Subsidiary = NULL # as explicit column in df is not necessary

# creat test & training substes
# --------------
rmat = as.matrix(rdf)

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
# best performing algorithm: UBCF_N_C 
test = Recommender(getData(e, "train"), "UBCF",
                   param = list(normalize = NULL, method = "Cosine"))

data = predict(test, 
             getData(e, "known"), 
             type="ratings") # ratings returns only predicted ratings

calcPredictionAccuracy(data, getData(e, "unknown"))
# RMSE: 0.522


# ---------------------------------------
# Return likely products 
# ---------------------------------------
# opportunities_prob
opportunities_prob = predict(test, 
                        rmat,
                        type = 'ratings')
opportunities_prob = as.data.frame(as.matrix(opportunities_prob@data))

# colnames(opportunities_prob) = colnames(rdf)


# add actual values & predicted values to one df

opportunities_prob$Entity = rownames(rdf)
opportunities_prob = opportunities_prob %>% select(Entity, everything())

# correct format of df
round_df = function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}
opportunities_prob = round_df(opportunities_prob, 3)


# whitespaces = as.data.frame(as.matrix(rmat@data))
whitespaces = rdf
whitespaces$Entity = rownames(rdf)
#whitespaces = whitespaces %>% select(Entity, everything())
whitespaces = whitespaces[ , c(64, 1:63)]

total_cols = length(whitespaces)
total_rows = length(whitespaces$Entity)
true_false = as.data.frame(is.na(whitespaces))

for(i in 1:total_rows){
  for(j in 2:total_cols){
    if(true_false[i, j] == FALSE){
      opportunities_prob[i,j] = 'GOT IT'
    }
  }
}



# to note: df now has converted numeric to char to include the 'GOT'

# ---------------------------------------
# return opportunities_prob per individual customer
# ---------------------------------------
cust_opp_full_prob = opportunities_prob %>%
  gather(Product, pred.Rating, -Entity) %>%
  arrange(desc(pred.Rating))

cust_opp_full_prob$Entity = as.factor(cust_opp_full_prob$Entity)
cust_opp_full_prob$Product = as.factor(cust_opp_full_prob$Product)


# join to determine product metadata: product selection
# optimise with join? 
cust_opp_full_prob$Product.Selection = product_metadata$Product.Selection[match(cust_opp_full_prob$Product, 
                                                                           product_metadata$Product)]


