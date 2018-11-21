# annonomise data
# -----------------
library(digest)

# cols_to_anon = c("Parent", "Banker", "Legal.Entity.Subsidiary")

anonymise = function(data, cols_to_anon, algo = "sha256")
{
  if(!require(digest)) stop("digest package is required")
  to_anon = subset(data, select = cols_to_anon)
  unname(apply(to_anon, 1, digest, algo = algo))
}

# sector product df
sector_product$Parent = anonymise(sector_product, 'Parent')
sector_product$Banker = anonymise(sector_product, 'Banker')
sector_product$Legal.Entity.Subsidiary = anonymise(sector_product, 'Legal.Entity.Subsidiary')

# write new anon data
setwd('F:/NYDSA/WhiteSpaces 2.0/shiny/data')
write.csv(sector_product, file = 'Sector_Product_Anon.csv')
                        