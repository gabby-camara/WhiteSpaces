# ----------------
# load data
# ----------------

# load data about sector, clients & products
# inlc argument = colClasses to correctly read in numeric from Actual value (factors drps the negative numbers)
# ----------------
setwd('F:/NYDSA/WhiteSpaces 2.0')

sector_product = read.csv('shiny/data/Sector_Product_1.csv', 
                          header = TRUE,
                          na.strings = c("", '#N/A'), 
                          colClasses = c('character', 'character', 'character', 'character', 'character',
                                         'character', 'character', 'character', 'character', 'character',
                                         'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
                                         'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric'))
sector_product$ID = NULL
# transform to correct datatype 
# ----------------
# sector_product$Actual = as.numeric(sector_product$Actual) - not needed (colClasses used instead)

# remove NA rows
# ----------------
colSums(is.na(sector_product))
sector_product = sector_product %>% drop_na(ClientKey) #dropping na from poorly creating the df @ beginning

# convert characters to Factors
sector_product = mutate_if(sector_product, is.character, as.factor)


# load data about business defined desk & product 
# ----------------
# desk_product = read.csv('shiny/data/Desk_Product.csv',
                        # header = TRUE)

# load banker information
# ----------------
banker_entity = read.csv('shiny/data/banker_client.csv', 
                         header = TRUE, 
                         na.strings = '')


# load CVP - Product Selection Info
# ----------------
CVP_selection = read.csv('shiny/data/CVP_ProductSelection.csv',
                          header = TRUE,
                          na.strings = '')

# load product metadata
# +- 90% complete; missing Barclays info (compare to Product Hierachy)
# ----------------
product_metadata = read.csv('shiny/data/product_metadata.csv',
                             header = TRUE,
                             na.strings = '')

