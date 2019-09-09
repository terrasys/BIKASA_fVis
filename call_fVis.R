#-----------------------------------------------------------------------------------------------------
#Basic Settings
#-----------------------------------------------------------------------------------------------------
W.DIR = "..."
FUNC.DIR = "_function/"
#-----------------------------------------------------------------------------------------------------
print("Import functions")
#-----------------------------------------------------------------------------------------------------
source(file.path(W.DIR,FUNC.DIR,"fPackages.R"))
source(file.path(W.DIR,FUNC.DIR,"fVis.R"))
source(file.path(W.DIR,FUNC.DIR,"fMap.R"))
source(file.path(W.DIR,FUNC.DIR,"fMapK.R"))
#-----------------------------------------------------------------------------------------------------
print("Call functions")
#-----------------------------------------------------------------------------------------------------
#Visualization of relations between phenological windows and heavy rain events
#-----------------------------------------------------------------------------------------------------

DOY <- c(14,91,121,131,139,147,166,177,187,193,203,208,214,221,229,227,233,243,252,261,274,317)
OUT.DIR = "_output/"
IN.DIR = "_input/"
R.LS <-  "DESTLI0503850019/DEM10_LS.asc"
R.K <- "DESTLI0503850019/DEM10_K.asc"
R.SHD <- "DESTLI0503850019/DEM10_SHD.asc"

V.DYN <- "DESTLI0503850019_RADOLANGT10MM_buffer5000_202.csv"
pdf(paste(W.DIR,OUT.DIR,"DESTLI0503850019_WW1.pdf",sep=""), 
    height=4.5,width=10)
fVis(W.DIR,
     IN.DIR,
     V.DYN,
     R.LS,
     R.K,
     PLANT)
dev.off()

V.DYN <- "DESTLI0503850019_RADOLANGT10MM_buffer5000_215.csv"
pdf(paste(W.DIR,OUT.DIR,"DESTLI0503850019_MS1.pdf",sep=""), 
    height=4.5,width=9.5)
fVis(W.DIR,
        IN.DIR,
        V.DYN,
        R.LS,
        PLANT)
dev.off()

#Mapping of DOY-specific soil erosion maps
#-----------------------------------------------------------------------------------------------------
V.DYN <- "DESTLI0503850019_RADOLANGT10MM_buffer5000_215.csv"
pdf(paste(W.DIR,OUT.DIR,"DESTLI0503850019_MS2.pdf",sep=""), 
    height=4.5,width=6)
fMapDyn(W.DIR,
        IN.DIR,
        V.DYN,
        R.LS,
        R.K,
        R.SHD,   
        CROP.NAME="Mais",
        D.L=50,
        ALPHA=0.6)
dev.off()


V.DYN <- "DESTLI0503850019_RADOLANGT10MM_buffer5000_202.csv"
pdf(paste(W.DIR,OUT.DIR,"DESTLI0503850019_WW2.pdf",sep=""), 
    height=4.5,width=6)
fMap(W.DIR,
     IN.DIR,
     V.DYN,
     R.LS,
     R.K,
     R.SHD,
     CROP.NAME="Winterweizen",
     D.L=50,
     ALPHA=0.6)
dev.off()

#-----------------------------------------------------------------------------------------------------
#Visualization of relations between soil erodibility, slope length and slope steepness 
#-----------------------------------------------------------------------------------------------------
W.DIR = "d:/Dropbox/_git/BIKASA_fVis/"
OUT.DIR = "_output/"
IN.DIR = "_input/DESTLI0500950004/"
R.K <- "DEM10_K.asc"
SHP1 <- "DESTLI0500950004_b1.shp"
SHP2 <- "DESTLI0500950004_b6.shp"
R.LS1 <- "DEM10_LS-FB1.asc"
R.LS2 <- "DEM10_LS-FB6.asc"
R.SHD <- "DEM10_SHD.asc"


pdf(paste(W.DIR,OUT.DIR,"DESTLI0500950004_K.pdf",sep=""), 
    height=4,width=5.7)
fMapK(W.DIR,
     IN.DIR,
     R.LS1,
     R.LS2,
     R.K,
     R.SHD, 
     SHP1,
     SHP2,
     D.L=50,
     ALPHA=0.5)
dev.off()
