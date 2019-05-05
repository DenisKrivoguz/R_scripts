df <- readRDS("D:/GIS/Kerch_peninsula/stackdf.Rda")
df <- as.data.frame(df, row.names = T)
library(ggplot2)

p1 <- ggplot(data = df, aes(x = ndvi_utm_cut))+
  geom_histogram(binwidth = 1, colour = "white", alpha = 0.75)+
  xlab("NDVI")

p2 <- ggplot(data = df, aes(x = mbi_utm_cut))+
  geom_histogram(binwidth = 1, colour = "white", alpha = 0.75)+
  xlab("Mass balance index")

p3 <- ggplot(data = df, aes(x = dtr_utm_cut))+
  geom_histogram(binwidth = 1, colour = "white", alpha = 0.75)+
  xlab("Distance to roads")

p4 <- ggplot(data = df, aes(x = dtw_utm_cut))+
  geom_histogram(binwidth = 1, colour = "white", alpha = 0.75)+
  xlab("Distance to water")

p5 <- ggplot(data = df, aes(x = slope_utm_cut))+
  geom_histogram(binwidth = 1, colour = "white", 
                 alpha = 0.75)+
  xlab("Slope")

p6 <- ggplot(data = df, aes(x = soils_utm_cut))+
  geom_histogram(binwidth = 1, colour = "white", 
                 alpha = 0.75)+
  xlab("Soil type")

p7 <- ggplot(data = df, aes(x = spi_utm_cut))+
  geom_histogram(binwidth = 1, colour = "white", alpha = 0.75)+
  xlab("Stream power index")

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(p1, p2, p3, p4, p5, p6, p7, cols = 2)