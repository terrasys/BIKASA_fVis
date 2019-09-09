fMapDyn <- function(W.DIR,
                 IN.DIR,
                 V.DYN,
                 R.LS,
                 R.K,
                 R.SHD,
                 CROP.NAME,
                 D.L,
                 ALPHA){
p <- read.csv2(file.path(W.DIR,IN.DIR,V.DYN))
ls <- raster(file.path(W.DIR,IN.DIR,R.LS))
k <- raster(file.path(W.DIR,IN.DIR,R.K))
shd <- raster(file.path(W.DIR,IN.DIR,R.SHD))
#Plotting
DOY <- DOY
N <- 10
D <- 0
my.palette <- rev(brewer.pal(n = N, name = "Spectral"))
breaks.qt <- classIntervals(values(ls*k), n = (N-1), style = "quantile")
pb <- txtProgressBar(min=1, max=length(DOY), style=3)
par(xpd = T, mar = par()$mar + c(0,0,0,5))
for(i in 1:length(DOY)){
    sl2 <- ls * k *p[DOY[i],]$MAX * (1-p[DOY[i],]$SC)
    plot(shd,
         col = grey(100:1/100),
         box=FALSE,
         legend=FALSE,
         axes = TRUE)
    plot(sl2,
         breaks = breaks.qt$brks,
         col=my.palette,
         legend=FALSE,
         axes = TRUE,
         box=FALSE,
         main=paste(CROP),
         alpha=ALPHA,
         add=TRUE)
    x.max <- max(data.frame(coordinates(sl2))$x)
    x.min <- min(data.frame(coordinates(sl2))$x)
    y.max <- max(data.frame(coordinates(sl2))$y)
    y.min <- min(data.frame(coordinates(sl2))$y)
    legend(x.max+((x.max-x.min)/D.L),y.max,
           title=expression(paste(italic(Eakt))), 
           legend = round(breaks.qt$brks,1), 
           fill = my.palette,
           bty="n",
           cex=1,
           xpd = TRUE)
    
  setTxtProgressBar(pb, i)
  }
}
