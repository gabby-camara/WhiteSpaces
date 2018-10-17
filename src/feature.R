# add banker info to main df(sector_product)
# total length original sector_product = 48 673; added extra rows?
# ---------------------
banker_entity$ClientKey = as.character(banker_entity$ClientKey)
sector_product$ClientKey = as.character(sector_product$ClientKey)
sector_product = left_join(sector_product, banker_entity[, c('ClientKey', 'Banker')], by = 'ClientKey')

rm(banker_entity) # clean up

# convert NA bankers to 'Unknown'
#sector_product = mutate_if(sector_product, is.character, as.factor)
#sector_product$Banker = mutate_if(sector_product$Banker, is.na, 'Unknown')

# NB! NB! NB! NB! NB! NB! NB! 
# need to remove outliers!

# create one actual value = mean of values over 12 month period
sector_product = sector_product %>%
                 mutate(Actual = rowMeans(select(.,starts_with("Actual")), na.rm = TRUE))    

# take log of Actual value
# ---------------------
# sector_product$log.Actual = log10(sector_product$Actual)

# normalize actual value
# ---------------------
sector_product$pos.Actual = ifelse(sector_product$Actual >= 0, sector_product$Actual, NA)
sector_product$neg.Actual = ifelse(sector_product$Actual < 0, sector_product$Actual*-1, NA)

sector_product$normal10.Actual = ifelse(is.na(sector_product$pos.Actual), 
                                        (-1 * 10 * sector_product$neg.Actual-min(sector_product$neg.Actual, na.rm = TRUE))/(max(sector_product$neg.Actual, na.rm = TRUE)-min(sector_product$neg.Actual, na.rm = TRUE)), 
                                        (10 * sector_product$pos.Actual-min(sector_product$pos.Actual, na.rm = TRUE))/(max(sector_product$pos.Actual, na.rm = TRUE)-min(sector_product$pos.Actual, na.rm = TRUE)))


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

# convert new features to correct data type
sector_product$ABC.Parent = as.factor(sector_product$ABC.Parent)
sector_product$ABC.Entity = as.factor(sector_product$ABC.Entity)
