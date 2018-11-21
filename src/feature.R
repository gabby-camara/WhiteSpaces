sector_product_full = sector_product

sector_product_full = sector_product_full %>%
  mutate(Actual = rowMeans(select(.,starts_with("Actual")), na.rm = TRUE))

colnames(sector_product_full)[10] = '2018-08-31'
colnames(sector_product_full)[11] = '2018-07-31'
colnames(sector_product_full)[12] = '2018-06-30'
colnames(sector_product_full)[13] = '2018-05-31'
colnames(sector_product_full)[14] = '2018-04-30'
colnames(sector_product_full)[15] = '2018-03-31'
colnames(sector_product_full)[16] = '2018-02-28'
colnames(sector_product_full)[17] = '2018-01-31'
colnames(sector_product_full)[18] = '2017-12-31'
colnames(sector_product_full)[19] = '2017-11-30'
colnames(sector_product_full)[20] = '2017-10-31'
colnames(sector_product_full)[21] = '2017-09-30'

# in order to simplyfy calculations will reduce df by 1 level (remove source product & roll up to Product only)
# ---------------------
sector_product = sector_product %>% group_by(Client.Sector, Parent, Legal.Entity.Subsidiary, ClientKey, Product.Business.Area, Product.Selection, Major.Desk, Product) %>% 
                  summarise_if(is.numeric, sum, na.rm = TRUE)
sector_product = sector_product %>% ungroup()

# add banker info to main df(sector_product)
# total length original sector_product = 48 673; added extra rows?
# ---------------------
banker_entity$ClientKey = as.character(banker_entity$ClientKey)
sector_product$ClientKey = as.character(sector_product$ClientKey)

sector_product = left_join(sector_product, banker_entity[, c('ClientKey', 'Banker')], by = 'ClientKey')


# impute missingness for bankers
# if $Banker = NA = 'UNKNOWN'
# colSums(is.na(sector_product))

sector_product$Banker = as.character(sector_product$Banker)
sector_product$Banker[is.na(sector_product$Banker)] = 'UNKNOWN'
sector_product$Banker[sector_product$Banker == 'UNKNOWN UNKNOWN'] = 'UNKNOWN'


# create one actual value = mean of values over 12 month period
# ---------------------
sector_product = sector_product %>%
                 mutate(Actual = rowMeans(select(.,starts_with("Actual")), na.rm = TRUE))  

# rename cols to dates for later analysis
# colnames(select(sector_product, starts_with("Actual")))
# ---------------------
colnames(sector_product)[9] = '2018-08-31'
colnames(sector_product)[10] = '2018-07-31'
colnames(sector_product)[11] = '2018-06-30'
colnames(sector_product)[12] = '2018-05-31'
colnames(sector_product)[13] = '2018-04-30'
colnames(sector_product)[14] = '2018-03-31'
colnames(sector_product)[15] = '2018-02-28'
colnames(sector_product)[16] = '2018-01-31'
colnames(sector_product)[17] = '2017-12-31'
colnames(sector_product)[18] = '2017-11-30'
colnames(sector_product)[19] = '2017-10-31'
colnames(sector_product)[20] = '2017-09-30'

# normalize actual values 
# ---------------------
# sector_product$pos.Actual = ifelse(sector_product$Actual >= 0, sector_product$Actual, NA)
# sector_product$pos.Actual = ifelse(sector_product$pos.Actual == 0, 0.0001, sector_product$pos.Actual)
# 
# sector_product$neg.Actual = ifelse(sector_product$Actual < 0, sector_product$Actual*-1, NA)
# sector_product$neg.Actual = ifelse(sector_product$neg.Actual == 0, 0.0001, sector_product$neg.Actual)

# remove extreme outliers 
# ---------------------
sector_product$Actual.log = ifelse(sector_product$Actual > 50000000, 50000000,
                               ifelse((sector_product$Actual < -20000), -20000, sector_product$Actual))



# take log of Actual value
# ---------------------
# log values = resulting in inf: log10(0)
# 104 clients that have an Actual = 0

a_log = 1 - min(sector_product$Actual.log)
sector_product$log.Actual = log10(sector_product$Actual.log + a_log)

# sector_product$log.Actual = ifelse(is.na(sector_product$pos.Actual), 
#                                         (log10(sector_product$neg.Actual)*-1), 
#                                         (log10(sector_product$pos.Actual)))


# sector_product$pos.Actual = NULL
# sector_product$neg.Actual = NULL


# add segment to dataframe
# ---------------------
# sector_product$Parent = as.character(sector_product$Parent)
# sector_product$Banker = as.character(sector_product$Banker)

segment_banker = segment_banker %>% unique()

sector_product = merge(sector_product, segment_banker[, c('Parent', 'Banker', 'Segment')],
                       by.x = c('Parent', 'Banker'), by.y = c('Parent', 'Banker'))

# ---------------------
# pareto classication (classify class of client)
# ---------------------
pareto_parent = sector_product %>%
                group_by(Parent) %>%
                summarise(total.Actual = sum(Actual)) %>%
                arrange(desc(total.Actual))

# add classification label (A, B, C)
pareto_parent = within(pareto_parent, {
                        Percent.Actual = cumsum(rev(sort(total.Actual)))/sum(total.Actual)
                        ABC.Parent = ifelse(Percent.Actual > 0.91, "C",
                              ifelse(Percent.Actual < 0.81, "A", "B"))
})

pareto_entity = sector_product %>%
                group_by(Legal.Entity.Subsidiary) %>%
                summarise(total.Actual = sum(Actual)) %>%
                arrange(desc(total.Actual))

# add classification label (A, B, C)
pareto_entity = within(pareto_entity, {
                        Percent.Actual = cumsum(rev(sort(total.Actual)))/sum(total.Actual)
                        ABC.Entity = ifelse(Percent.Actual > 0.91, "C",
                        ifelse(Percent.Actual < 0.81, "A", "B"))
})

# join classifications to main data frame: sector_product
sector_product = left_join(sector_product, pareto_parent %>% select(Parent, ABC.Parent), 
                           by = 'Parent')
sector_product = left_join(sector_product, pareto_entity %>% select(Legal.Entity.Subsidiary, ABC.Entity), 
                           by = 'Legal.Entity.Subsidiary')

# Convert all characters to factors
# ---------------------
sector_product = mutate_if(sector_product, is.character, as.factor)

# check NAs in df
# ---------------------
colSums(is.na(sector_product))

rm(banker_entity, pareto_entity, pareto_parent, segment_banker) # clean up


