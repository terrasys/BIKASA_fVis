fMapLSK <- function(W.DIR,
                 IN.DIR,
                 R.LS1,
                 R.LS2,
                 R.K,
                 R.SHD,
                 SHP1,
                 SHP2,
                 D.L,
                 ALPHA){
s <- shapefile(file.path(W.DIR,IN.DIR,SHP1))
ls <- raster(file.path(W.DIR,IN.DIR,R.LS1))
k <- raster(file.path(W.DIR,IN.DIR,R.K))
shd <- raster(file.path(W.DIR,IN.DIR,R.SHD))
#Plotting
N <- 9
D <- 0
my.palette <- rev(brewer.pal(n = N, name = "Spectral"))
breaks.qt <- classIntervals(values(ls*k), n = (N-1), style = "quantile")

#par(mfrow = c(3,1))
par(xpd = T, mar = par()$mar + c(0,0,0,5))
    sl <- ls * k
    plot(shd,
         col = grey(100:1/100),
         box=FALSE,
         legend=FALSE,
         axes = TRUE)
    plot(sl,
         breaks = breaks.qt$brks,
         col=my.palette,
         alpha=ALPHA,
         add=TRUE,
         legend=FALSE)
    x.max <- max(data.frame(coordinates(sl))$x)
    x.min <- min(data.frame(coordinates(sl))$x)
    y.max <- max(data.frame(coordinates(sl))$y)
    y.min <- min(data.frame(coordinates(sl))$y)
    legend("bottomleft", 
           title=expression(paste(Faktoren)), 
           legend=c(as.expression(bquote({italic(K)} == .(round(quantile(values(k),na.rm=TRUE)[[4]],1)))),
                    as.expression(bquote({italic(LS)} == .(round(quantile(values(ls),na.rm=TRUE)[[4]],1))))),
           text.col=c(1,1),
           cex=1,
           bty != "n",
           bg="white")
    legend(x.max+((x.max-x.min)/D.L),y.max,
           title=expression(paste(italic(Enat))), 
           legend = round(breaks.qt$brks,1), 
           fill = my.palette,
           bty="n",
           cex=1,
           xpd = TRUE)
    plot(s,add=TRUE)

    
    s <- shapefile(file.path(W.DIR,IN.DIR,SHP2))
    ls <- raster(file.path(W.DIR,IN.DIR,R.LS2))
    sl <- ls * k
    plot(shd,
         col = grey(100:1/100),
         box=FALSE,
         legend=FALSE,
         axes = TRUE)
    plot(sl,
         breaks = breaks.qt$brks,
         col=my.palette,
         alpha=ALPHA,
         add=TRUE,
         legend=FALSE)
    x.max <- max(data.frame(coordinates(sl))$x)
    x.min <- min(data.frame(coordinates(sl))$x)
    y.max <- max(data.frame(coordinates(sl))$y)
    y.min <- min(data.frame(coordinates(sl))$y)
    legend("bottomleft", 
           title=expression(paste(Faktoren)), 
           legend=c(as.expression(bquote({italic(K)} == .(round(quantile(values(k),na.rm=TRUE)[[3]],1)))),
                    as.expression(bquote({italic(LS)} == .(round(quantile(values(ls),na.rm=TRUE)[[3]],1))))),
           text.col=c(1,1),
           cex=1,
           bty != "n",
           bg="white")
    legend(x.max+((x.max-x.min)/D.L),y.max,
           title=expression(paste(italic(Enat))), 
           legend = round(breaks.qt$brks,1), 
           fill = my.palette,
           bty="n",
           cex=1,
           xpd = TRUE)
    plot(s,add=TRUE)
    
    
    s <- shapefile(file.path(W.DIR,IN.DIR,SHP1))
    ls <- raster(file.path(W.DIR,IN.DIR,R.LS1))
    sl <- ls * k/2
    plot(shd,
         col = grey(100:1/100),
         box=FALSE,
         legend=FALSE,
         axes = TRUE)
    plot(sl,
         breaks = breaks.qt$brks,
         col=my.palette,
         alpha=ALPHA,
         add=TRUE,
         legend=FALSE)
    x.max <- max(data.frame(coordinates(sl))$x)
    x.min <- min(data.frame(coordinates(sl))$x)
    y.max <- max(data.frame(coordinates(sl))$y)
    y.min <- min(data.frame(coordinates(sl))$y)
    legend("bottomleft", 
           title=expression(paste(Faktoren)), 
           legend=c(as.expression(bquote({italic(K)} == .(round(quantile(values(k/2),na.rm=TRUE)[[3]],1)))),
                    as.expression(bquote({italic(LS)} == .(round(quantile(values(ls),na.rm=TRUE)[[4]],1))))),
           text.col=c(1,1),
           cex=1,
           bty != "n",
           bg="white")
    legend(x.max+((x.max-x.min)/D.L),y.max,
           title=expression(paste(italic(Enat))), 
           legend = round(breaks.qt$brks,1), 
           fill = my.palette,
           bty="n",
           cex=1,
           xpd = TRUE)
    plot(s,add=TRUE)
    
    
    s <- shapefile(file.path(W.DIR,IN.DIR,SHP2))
    ls <- raster(file.path(W.DIR,IN.DIR,R.LS2))
    sl <- ls * k/2
    plot(shd,
         col = grey(100:1/100),
         box=FALSE,
         legend=FALSE,
         axes = TRUE)
    plot(sl,
         breaks = breaks.qt$brks,
         col=my.palette,
         alpha=ALPHA,
         add=TRUE,
         legend=FALSE)
    x.max <- max(data.frame(coordinates(sl))$x)
    x.min <- min(data.frame(coordinates(sl))$x)
    y.max <- max(data.frame(coordinates(sl))$y)
    y.min <- min(data.frame(coordinates(sl))$y)
    legend("bottomleft", 
           title=expression(paste(Faktoren)), 
           legend=c(as.expression(bquote({italic(K)} == .(round(quantile(values(k/2),na.rm=TRUE)[[3]],1)))),
                    as.expression(bquote({italic(LS)} == .(round(quantile(values(ls),na.rm=TRUE)[[3]],1))))),
           text.col=c(1,1),
           cex=1,
           bty != "n",
           bg="white")
    legend(x.max+((x.max-x.min)/D.L),y.max,
           title=expression(paste(italic(Enat))), 
           legend = round(breaks.qt$brks,1), 
           fill = my.palette,
           bty="n",
           cex=1,
           xpd = TRUE)
    plot(s,add=TRUE)
    }
