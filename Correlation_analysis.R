library(corrplot)

df <- readRDS("D:/GIS/Kerch_peninsula/stackdf_factors.Rda")

corr <- cor(df)
source("http://www.sthda.com/upload/rquery_cormat.r")
cormat <- rquery.cormat(df, graphType = "heatmap")

ggcorrplot(corr, hc.order = T, type = "lower", lab = T)

