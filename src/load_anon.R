# load anon data

setwd('F:/NYDSA/WhiteSpaces 2.0') # set accordingly & update read.csv functions

# load data about sector, clients & products
# inlc argument = colClasses to correctly read in numeric from Actual value (factors drps the negative numbers)
# ----------------
sector_product = read.csv('shiny/data/Sector_Product_Anon.csv', 
                          header = TRUE,
                          na.strings = c("", '#N/A'),
                          colClasses = c('character', 'character', 'character', 'character', 'character', 'character',
                                         'character', 'character', 'character', 'character', 
                                         'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
                                         'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
                                         'numeric', 'numeric', 'numeric', 'character', 'character', 'character'))

sector_product$X = NULL
colnames(sector_product)[10] = '2018-08-31'
colnames(sector_product)[11] = '2018-07-31'
colnames(sector_product)[12] = '2018-06-30'
colnames(sector_product)[13] = '2018-05-31'
colnames(sector_product)[14] = '2018-04-30'
colnames(sector_product)[15] = '2018-03-31'
colnames(sector_product)[16] = '2018-02-28'
colnames(sector_product)[17] = '2018-01-31'
colnames(sector_product)[18] = '2017-12-31'
colnames(sector_product)[19] = '2017-11-30'
colnames(sector_product)[20] = '2017-10-31'
colnames(sector_product)[21] = '2017-09-30'

# take log of Actual value
# ---------------------
# log values = resulting in inf: log10(0)
# 104 clients that have an Actual = 0

a_log = 1 - min(sector_product$Actual.log)
sector_product$log.Actual = log10(sector_product$Actual.log + a_log)


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