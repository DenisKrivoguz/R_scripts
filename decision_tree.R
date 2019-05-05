library(dplyr)
library(rpart)
library(rpart.plot)

df <- readRDS("D:/GIS/Kerch_peninsula/stackdf.Rda")

cols <- names(df)[3:12]

df <- df %>% mutate_at(vars(-lat, -lon, -dem), funs(round(.,0)))
names <- c("ndvi", "mbi", "dtr", "dtw", "lsi3", "lsi8", "slope", "soils", "spi")
df[, names] <- lapply(df[, names], factor)

tree <- rpart(lsi3 ~ dem + ndvi + mbi + dtr + dtw + slope + soils + spi, data = df)

prp(tree)

