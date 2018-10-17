# select specific cluster

# drop columns
cluster_vis = subset(sector_product_cluster, select = -c(ID, 
                                                         pos.Actual,
                                                         neg.Actual
                                                         ))
# subset to cluster number
cluster_spec = cluster_vis %>% 
               filter(Cluster == 2)

# visualization
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(cluster_spec, aes(Client.Sector)) + geom_bar(fill="#56B4E9", colour="#56B4E9") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(cluster_spec, aes(Product)) + geom_bar(fill="#F0E442", colour="#F0E442") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(cluster_spec, aes(Major.Desk)) + geom_bar(fill="#F0E442", colour="#F0E442", width = 0.3) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)

ggplot(cluster_spec, aes(ABC.Entity)) + geom_bar(fill="#F0E442", colour="#F0E442") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)

ggplot(cluster_spec, aes(Product.Selection)) + geom_bar(fill="#F0E442", colour="#F0E442", width = 0.3) +
  geom_text(stat='count', aes(label=..count..), vjust=0) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

cluster_prod = cluster_spec %>%
               group_by(Product, Source.Product) %>%
               summarise(min.Actual = min(Actual),
                         max.Actual = max(Actual)) %>%
               arrange(desc(max.Actual))

cluster_prod = cluster_spec %>%
  group_by(Product) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
