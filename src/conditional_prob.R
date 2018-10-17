# conditional probability
# ---------------------------
# https://rpubs.com/ashutoshnanda/salesdata-marketbasketanalysis

# convert to spare matrix
library(Matrix)

product_matrix = sparseMatrix(i = sector_product$Legal.Entity.Subsidiary,
                              j = sector_product$Product)
