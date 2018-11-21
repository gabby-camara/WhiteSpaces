# Final recommendation algorithms
# --------------------

# data
# --------------------
rdf_final = sector_product %>% 
            select(Client.Sector, Segment, Legal.Entity.Subsidiary, Product, log.Actual) %>%
            group_by(Client.Sector, Segment, Legal.Entity.Subsidiary, Product) %>%
            mutate(log.Actual = sum(log.Actual)) %>%
            distinct()

rdf_final = spread(rdf_final, Product, log.Actual)


# unsacle pred ratings over entire dataset
# --------------------
unscale_pred = function(rdf, opp){
  
  unscaled = rdf[, -c(1:3)]
  products = colnames(rdf[, -c(1:3)])
  # perhaps make 0 == NA?
  
  for(i in 1:length(products)){
    product = products[i]
    
    # unscaled[i] = numeric()
    # i = 1
    min = min(rdf[i+3], na.rm = TRUE)
    max = max(rdf[i+3], na.rm = TRUE)
    
    unscaled[i] = sapply(opp[i], function(x) round((10^((x/10)*(max-min)+min)), 2))
  }
  return(unscaled)
}

# correct format of df
round_df = function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

# predictions method:
IBCF_Z_E = Recommender(getData(e, "train"), "IBCF", 
                       param=list(normalize = "Z-score",method="Euclidean"))

# parameters
# --------------------
# r_method = IBCF_N_E # pull in best performing algorithm
# distance = 'Euclidian'
# normalise = NULL
# sector = 'Agriculture'
# data = rdf_final
# method = IBCF_Z_E


# function 
# --------------------

define_opportunities = function(method, sector, data){
  
  # get correct data per sector; rescale log values between 0-10
  data_sector = data %>% filter(Client.Sector == sector) # unscaled df (log of Actual values)
  data_sector[ , -c(1:3)] = data.frame(lapply(data_sector[ , -c(1:3)], function(x) scales::rescale(x, to = c(0, 10)))) # scale between 0-10
  
  # log values of actual revenue stored
  rdf_final = data %>% filter(Client.Sector == sector) # unscaled values of actual product holding
  
  #data_sector$Client.Sector = NULL
  #data_sector$Segment = NULL
  
  #rownames(data_sector) = data_sector$Legal.Entity.Subsidiary # suggest storing in vector
  
  # convert to correct rmat
  rmat = as.matrix(data_sector[ , -c(1,2,3)])
  rmat = as(rmat,"realRatingMatrix")
  
  # predict ratings
  # pass parameters instead
  method = Recommender(rmat, "IBCF",
                       param=list(normalize = "Z-score", method="Euclidean"))
  
  opportunities = predict(method,
                          rmat, 
                          type = 'ratings')
  
  # define boundaries
  opportunities@data@x[opportunities@data@x[] < 0] = 0
  opportunities@data@x[opportunities@data@x[] > 10] = 10
  
  opportunities = as.data.frame(as.matrix(opportunities@data)) # predicted ratings (scaled between 0-10)
  
  # unscaled = unscale_pred(data, opportunities)
  unscaled = unscale_pred(rdf_final, opportunities) # unscale opportunities from ratings (0-10) to log of Actual Predicted revenue
  # unscaled = round_df(unscaled, 2)
  
  # full product & predicted value
  products_fill = unscaled
    
  total_cols = length(unscaled) # number of products
  total_rows = nrow(unscaled)   # number of entities in sector
  
  true_false = as.data.frame(is.na(rdf_final[, -c(1:3)]))
  
  for(i in 1:total_rows){
    for(j in 1:total_cols){
      if (true_false[i, j] == FALSE) {
        products_fill[i,j] = (10^rdf_final[i, j + 3]) - a_log # unlog actual values
      } else {
        products_fill[i, j] = unscaled[i,j]
      }
    }
  }
  colnames(products_fill) = colnames(opportunities)
  products_fill[is.na(products_fill)] = 'Not Recommended'
  
  
  # keep only product with a rating > 0
  opportunities = opportunities[ , colSums(opportunities) >0]
  products_fill = products_fill[colSums(!is.na(products_fill)) > 0] #to determine if necessary
  
  products_fill$Legal.Entity.Subsidiary = rdf_final$Legal.Entity.Subsidiary
  products_fill$Segment = rdf_final$Segment
  products_fill$Client.Sector = rdf_final$Client.Sector
  products_fill = products_fill %>% select(Client.Sector, Segment, Legal.Entity.Subsidiary, everything())
  products_fill = as.data.frame(products_fill)
  
  # colnames(opportunities) = colnames(data[, -c(1:3)])
  
  # correct format
  opportunities = round_df(opportunities, 0)
  opportunities$Legal.Entity.Subsidiary = data_sector$Legal.Entity.Subsidiary
  opportunities$Segment = data_sector$Segment
  opportunities$Client.Sector = data_sector$Client.Sector
  opportunities = opportunities %>% select(Client.Sector, Segment, Legal.Entity.Subsidiary, everything()) %>% mutate_if(is.numeric, as.character)
  
  products = colnames(opportunities)
  
  id = match(products, names(data_sector))

  whitespaces = data_sector[ , id]
  
  # whitespaces = data_sector
  
  total_cols = length(whitespaces)
  total_rows = length(whitespaces$Legal.Entity.Subsidiary)
  
  true_false = as.data.frame(is.na(whitespaces))

  for(i in 1:total_rows){
    for(j in 4:total_cols){
      if(true_false[i, j] == FALSE){
        opportunities[i,j] = 'GOT IT'
        }
      }
  }
  return(list(opportunities, products_fill))
  # return(opportunities)
}


# create list of all sesctors
# -------------------
sectors = unique(sector_product$Client.Sector)


# apply function(opportunities) to all sectors
# produces df per sector listing 2 outputs: recommendations[1] = rating opportunities; recommendations[2] = predicted & actual revenue values
# ------------------
for(i in 1:length(sectors)){
  assign(make.names(paste('recommendations.', sectors[i], sep = "")), define_opportunities(data = rdf_final, sector = sectors[i]))
}

# only opp dataframes
# for(i in 1:length(sectors)){
#   assign(make.names(paste('opportunities.', sectors[i], sep = "")), define_opportunities(data = rdf_final, sector = sectors[i]))
# }


# merage all dataframes in order to derive customer view
# -------------------

# pull rated opportunities per sector
opportunities.Agriculture = as.data.frame(recommendations.Agriculture[1])
opportunities.Construction = as.data.frame(recommendations.Construction[1])
opportunities.Diversified = as.data.frame(recommendations.Diversified[1])
opportunities.Financial.Institutions = as.data.frame(recommendations.Financial.Institutions[1])
opportunities.Healthcare = as.data.frame(recommendations.Healthcare[1])
opportunities.Hotel...Leisure = as.data.frame(recommendations.Hotel...Leisure[1])
opportunities.Industrials = as.data.frame(recommendations.Industrials[1])
opportunities.Manufacturing = as.data.frame(recommendations.Manufacturing[1])
opportunities.Mining...Metals = as.data.frame(recommendations.Mining...Metals[1])
opportunities.Natural.Resources = as.data.frame(recommendations.Natural.Resources[1])
opportunities.Non.Banking.Financial.Institutions = as.data.frame(recommendations.Non.Banking.Financial.Institutions[1])
opportunities.Oil...Gas = as.data.frame(recommendations.Oil...Gas[1])
opportunities.Other = as.data.frame(recommendations.Other[1])
opportunities.Professional.Services = as.data.frame(recommendations.Professional.Services[1])
opportunities.Public.Sector = as.data.frame(recommendations.Public.Sector[1])
opportunities.PUI = as.data.frame(recommendations.PUI[1])
opportunities.Real.Estate = as.data.frame(recommendations.Real.Estate[1])
opportunities.Retail = as.data.frame(recommendations.Retail[1])
opportunities.TMT = as.data.frame(recommendations.TMT[1])
opportunities.Unknown = as.data.frame(recommendations.Unknown[1])

# pull predicted & actual revenue values per sector
revenue_r.Agriculture = as.data.frame(recommendations.Agriculture[2])
revenue_r.Construction = as.data.frame(recommendations.Construction[2])
revenue_r.Diversified = as.data.frame(recommendations.Diversified[2])
revenue_r.Financial.Institutions = as.data.frame(recommendations.Financial.Institutions[2])
revenue_r.Healthcare = as.data.frame(recommendations.Healthcare[2])
revenue_r.Hotel...Leisure = as.data.frame(recommendations.Hotel...Leisure[2])
revenue_r.Industrials = as.data.frame(recommendations.Industrials[2])
revenue_r.Manufacturing = as.data.frame(recommendations.Manufacturing[2])
revenue_r.Mining...Metals = as.data.frame(recommendations.Mining...Metals[2])
revenue_r.Natural.Resources = as.data.frame(recommendations.Natural.Resources[2])
revenue_r.Non.Banking.Financial.Institutions = as.data.frame(recommendations.Non.Banking.Financial.Institutions[2])
revenue_r.Oil...Gas = as.data.frame(recommendations.Oil...Gas[2])
revenue_r.Other = as.data.frame(recommendations.Other[2])
revenue_r.Professional.Services = as.data.frame(recommendations.Professional.Services[2])
revenue_r.Public.Sector = as.data.frame(recommendations.Public.Sector[2])
revenue_r.PUI = as.data.frame(recommendations.PUI[2])
revenue_r.Real.Estate = as.data.frame(recommendations.Real.Estate[2])
revenue_r.Retail = as.data.frame(recommendations.Retail[2])
revenue_r.TMT = as.data.frame(recommendations.TMT[2])
revenue_r.Unknown = as.data.frame(recommendations.Unknown[2])


# bind opportunities to one dataframe
client_opp_full = bind_rows(opportunities.Agriculture,
                            opportunities.Construction,
                            opportunities.Diversified,
                            opportunities.Financial.Institutions,
                            opportunities.Healthcare,
                            opportunities.Hotel...Leisure,
                            opportunities.Industrials,
                            opportunities.Manufacturing,
                            opportunities.Mining...Metals,
                            opportunities.Natural.Resources,
                            opportunities.Non.Banking.Financial.Institutions,
                            opportunities.Oil...Gas,
                            opportunities.Other,
                            opportunities.Professional.Services,
                            opportunities.Public.Sector,
                            opportunities.PUI,
                            opportunities.Real.Estate,
                            opportunities.Retail,
                            opportunities.TMT,
                            opportunities.Unknown)

# bind revenue vlaues to one dataframe (used by shiny dashboard)
products_fill=rbind(revenue_r.Agriculture,
                      revenue_r.Construction,
                      revenue_r.Diversified,
                      revenue_r.Financial.Institutions,
                      revenue_r.Healthcare,
                      revenue_r.Hotel...Leisure,
                      revenue_r.Industrials,
                      revenue_r.Manufacturing,
                      revenue_r.Mining...Metals,
                      revenue_r.Natural.Resources,
                      revenue_r.Non.Banking.Financial.Institutions,
                      revenue_r.Oil...Gas,
                      revenue_r.Other,
                      revenue_r.Professional.Services,
                      revenue_r.Public.Sector,
                      revenue_r.PUI,
                      revenue_r.Real.Estate,
                      revenue_r.Retail,
                      revenue_r.TMT,
                      revenue_r.Unknown)

# gather df
#client_opp_full = opportunities.Agriculture
# convert to correct product naming standards
names(client_opp_full) = gsub(x = names(client_opp_full),
                                  pattern = "\\.",
                                  replacement = " ")

names(client_opp_full) = gsub(x = names(client_opp_full),
                                  pattern = "   ",
                                  replacement = " - ")

names(client_opp_full)[names(client_opp_full) == 'OVERDRAFTS  CHEQUE '] = 'OVERDRAFTS (CHEQUE)'
names(client_opp_full)[names(client_opp_full) == 'LOAN PORTFOLIO  CORPORATE '] = 'LOAN PORTFOLIO (CORPORATE)'
names(client_opp_full)[names(client_opp_full) == 'Client Sector'] = 'Client.Sector'
names(client_opp_full)[names(client_opp_full) == 'Legal Entity Subsidiary'] = 'Legal.Entity.Subsidiary'

# overdrafts (cheque); loan portfolio (corporate)

client_opp_full[is.na(client_opp_full)] = 'Not Recommended'

#client_opp_full$Legal.Entity.Subsidiary = as.character(client_opp_full$Legal.Entity.Subsidiary)
#client_opp_full$Product = as.character(client_opp_full$Product)

client_opp_full = client_opp_full[, -c(1:2)] %>% gather(Product, pred.Rating, -Legal.Entity.Subsidiary) %>% arrange(desc(pred.Rating))

# gather revenue & predicted revenue values
colnames(products_fill) = colnames(rdf_final)
products_fill = products_fill[, -c(1:2)] %>% gather(Product, pred.Rating, -Legal.Entity.Subsidiary) %>% arrange(desc(pred.Rating))

# convert to correct data types
#client_opp_full$Legal.Entity.Subsidiary = as.character(client_opp_full$Legal.Entity.Subsidiary)
#client_opp_full$Product = as.character(client_opp_full$Product)

#products_fill$Legal.Entity.Subsidiary = as.character(products_fill$Legal.Entity.Subsidiary)
#products_fill$Product = as.character(products_fill$Product)


# pull correct product.selection (matched CVP : ensure matched; else dropped in Shiny)
client_opp_full$Product.Selection = product_metadata$Product.Selection[match(client_opp_full$Product, 
                                                                             product_metadata$Product)]

# add revenue values 
# client_opp_full$Revenue = NULL
# colnames=  match
client_opp_full$Revenue = products_fill$pred.Rating[match(interaction(client_opp_full$Legal.Entity.Subsidiary, client_opp_full$Product), 
                                                          interaction(products_fill$Legal.Entity.Subsidiary, products_fill$Product))]

# tidying data up
client_opp_full$Revenue = as.numeric(client_opp_full$Revenue)
client_opp_full$Revenue = round(client_opp_full$Revenue, 2) # round values; 2 decimals
client_opp_full$Revenue[is.na(client_opp_full$Revenue)] = 'Not Recommended'


# format view
client_opp_full = client_opp_full %>% select(Legal.Entity.Subsidiary, Product.Selection, Product, pred.Rating, Revenue)
# client_opp_full = client_opp_full %>% select(Legal.Entity.Subsidiary, Product.Selection, Product, pred.Rating)


# get metadata pulled correctly
# -------------------
metadata = sector_product %>%
  select(Parent, Legal.Entity.Subsidiary, Banker, Client.Sector) %>%
  unique()

# clean up 


rm(revenue_r.Agriculture,
    revenue_r.Construction,
    revenue_r.Diversified,
    revenue_r.Financial.Institutions,
    revenue_r.Healthcare,
    revenue_r.Hotel...Leisure,
    revenue_r.Industrials,
    revenue_r.Manufacturing,
    revenue_r.Mining...Metals,
    revenue_r.Natural.Resources,
    revenue_r.Non.Banking.Financial.Institutions,
    revenue_r.Oil...Gas,
    revenue_r.Other,
    revenue_r.Professional.Services,
    revenue_r.Public.Sector,
    revenue_r.PUI,
    revenue_r.Real.Estate,
    revenue_r.Retail,
    revenue_r.TMT,
    revenue_r.Unknown)

rm(recommendations.Agriculture,
   recommendations.Construction,
   recommendations.Diversified,
   recommendations.Financial.Institutions,
   recommendations.Healthcare,
   recommendations.Hotel...Leisure,
   recommendations.Industrials,
   recommendations.Manufacturing,
   recommendations.Mining...Metals,
   recommendations.Natural.Resources,
   recommendations.Non.Banking.Financial.Institutions,
   recommendations.Oil...Gas,
   recommendations.Other,
   recommendations.Professional.Services,
   recommendations.Public.Sector,
   recommendations.PUI,
   recommendations.Real.Estate,
   recommendations.Retail,
   recommendations.TMT,
   recommendations.Unknown)
