df <- readRDS("D:/GIS/Kerch_peninsula/stackdf_factors.Rda")

df <- na.omit(df) #Проверяем на наличие NoData
df <- scale(df) #Стандартизируем переменные

df <- apply(df, 2, diff) #Рассчитываем разницу между последовательными элементами

df <- t(df) #Транспонируем данные в таблице

hc <- hclust(dist(df))

plot(hc)