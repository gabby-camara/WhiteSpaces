# set working directory
# ----------------------
setwd('F:/NYDSA/WhiteSpaces 2.0/shiny') # set accordingly

# source files
# ----------------------
source('src/ini.R')
source('src/load.R') # - just to view code (removed because of anon data)
source('src/feature.R') # - just to view code (features included in anon data)
source('src/load_anon.R')
source('src/EDA.R')
source('src/recommendation.R')
source('src/recommendation_final.R')

# Shiny dashbaord files
# ----------------------
source('shiny/ui.R')
source('shiny/server.R')


# source('src/visualization.R')
# source('src/cluster.R')
# source('src/main.R')
