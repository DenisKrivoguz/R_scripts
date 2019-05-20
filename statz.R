setwd("D:/GIS/Data/Kerch_pen/factors")

#Загружаем пакеты
library(raster)
library("dendextend", 
        lib.loc="D:/Programs/R-3.5.2/library")


######################################
# I. Подготовка данных
######################################
#Загружаем данные
aspect <- raster("aspect.tif")
chetv <- raster("chetv.tif")
dem <- raster("dem.tif")
dochetv <- raster("dochetv.tif")
dtr <- raster("dtr.tif")
dtw <- raster("dtw.tif")
lsi3 <- raster("lsi3.tif")
lsi8 <- raster("lsi8.tif")
ndvi <- raster("ndvi.tif")
prec <- raster("prec.tif")
slope <- raster("slope.tif")
soils <- raster("soils.tif")
spi <- raster("spi.tif")
twi <- raster("twi.tif")

#Задаем экстент
ext <- extent(227783, 315714, 
              4987754, 5040340)

#Присваиваем экстент для каждого фактора
extent(aspect) <- ext
extent(chetv) <- ext
extent(dem) <- ext
extent(dochetv) <- ext
extent(dtr) <- ext
extent(dtw) <- ext
extent(lsi3) <- ext
extent(lsi8) <- ext
extent(ndvi) <- ext
extent(prec) <- ext
extent(slope) <- ext
extent(soils) <- ext
extent(spi) <- ext
extent(twi) <- ext

#Ресэмпл фактоов по слою aspect
chetv_res <- resample(chetv, aspect)
dem_res <- resample(dem, aspect)
dochetv_res <- resample(dochetv, aspect)
dtr_res <- resample(dtr, aspect)
dtw_res <- resample(dtw, aspect)
lsi3_res <- resample(lsi3, aspect)
lsi8_res <- resample(lsi8, aspect)
ndvi_res <- resample(ndvi, aspect)
prec_res <- resample(prec, aspect)
slope_res <- resample(slope, aspect)
soils_res <- resample(soils, aspect)
spi_res <- resample(spi, aspect)
twi_res <- resample(twi, aspect)

#Создаем стек из факторов
raster_stack <- stack(c(aspect, chetv_res, dem_res, 
                        dochetv_res, dtr_res, dtw_res, 
                        lsi3_res, lsi8_res, ndvi_res, prec_res, 
                        slope_res, soils_res, spi_res, twi_res))

#Создаем матрицу из стека
stackdf <- rasterToPoints(raster_stack)
stackdf <- na.omit(stackdf)

#Создаем датафрэйм из матрицы
stackdf <- as.data.frame(stackdf, row.names = T)

#Конвертируем в Rda
save(stackdf, file = "D:/GIS/Data/Kerch_pen/factors/stackdf.Rda")

#Очищаем workspace
rm(list = ls())

###############################################
###############################################

#Загружаем Rda с факторами
setwd("D:/GIS/Data/Kerch_pen/factors")
load("stackdf.Rda")

#Отделяем факторы от ненужных переменных
factors <- subset(stackdf, 
                  select = c("aspect", "chetv", "dem", 
                                      "dochetv", "dtr", "dtw", 
                                      "ndvi", "prec", "slope", 
                                      "soils", "spi", "twi"))

#Создаем датафрэйм для 3 классов LSI
factors3 <- subset(stackdf, 
                   select = c("lsi3", "aspect", "chetv", 
                                       "dem", "dochetv", "dtr", 
                                       "dtw", "ndvi", "prec", 
                                       "slope", "soils", "spi", "twi"))

#Создаем датафрейм для 8 классов LSI
factors8 <- subset(stackdf, 
                   select = c("lsi8", "aspect", "chetv", 
                                       "dem", "dochetv", "dtr", 
                                       "dtw", "ndvi", "prec", "slope", 
                                       "soils", "spi", "twi"))

#Проводим нормирование данных
factors_norm <- as.data.frame(scale(factors))
factors3_norm <- as.data.frame(scale(factors3[2:13]))
factors8_norm <- as.data.frame(scale(factors8[2:13]))

factors3_norm <- data.frame(stackdf$lsi3, factors3_norm)
factors8_norm <- data.frame(stackdf$lsi8, factors8_norm)
###########################################
# Корреляционный анализ 
###########################################
#Проводим корреляционный анализ для факторов
corr3 <- as.data.frame(cor(factors3_norm[-1], 
                           factors3_norm$lsi3)) 
corr8 <- as.data.frame(cor(factors8_norm[-1], 
                           factors8_norm$lsi8))


###########################################
# анализ основных компонент 
###########################################

#Проводим анализ основных компонент PCA
factors3.pca <- prcomp(factors3_norm[2:13])
factors8.pca <- prcomp(factors8_norm[2:13])

summary(factors3.pca)
summary(factors8.pca)

#Определяем количество необходимых основных компонент
screeplot(factors3.pca, type="lines")
screeplot(factors8.pca, type="lines")

#Рассчитываем критерий Кайзера
(factors3.pca$sdev)^2
(factors8.pca$sdev)^2

#Рассчитываем нагрузки для основных компонент
factors3.pca$rotation
factors8.pca$rotation

###########################################
# Кластерный анализ 
###########################################

#Рассчитываем разницу между последовательными элементами
diff3 <- apply(factors3_norm[2:13], 2, diff)
diff8 <- apply(factors8_norm[2:13], 2, diff)

#Транспонируем данные в таблице
diff3 <- t(diff3)
diff8 <- t(diff8)

hc3 <- hclust(dist(diff3))
hc8 <- hclust(dist(diff8))

plot(hc3)
plot(hc8)


