df <- readRDS("D:/GIS/Kerch_peninsula/stackdf_factors.Rda")

df <- na.omit(df) #��������� �� ������� NoData
df <- scale(df) #��������������� ����������

df <- apply(df, 2, diff) #������������ ������� ����� ����������������� ����������

df <- t(df) #������������� ������ � �������

hc <- hclust(dist(df))

plot(hc)