install.packages("maps")
install.packages("mapdata")
install.packages("maptools")

library(maps)
library(mapdata)

sites <- data.frame(
  site = c("Castle Beach", "Mount Batten Beach", "Kilve Beach", "East Sands",
           "Menai Bridge Beach", "Lee-on-the-Solent Beach", "Ovingdean Beach",
           "Shoeburyness Beach", "Beadnell Haven"),
  lon = c(-5.056734994217719, -4.125926867674579, -3.227272262107197, -2.780355363984616,
          -4.163650119638755, -1.202467657206486, -0.07106788257516067, 0.7998852485055727,
          -1.6309864431422856),
  lat = c(50.14854875208407, 50.357248394430734, 51.19323592159223, 56.33812657252612,
          53.22176814619481, 50.800221632313836, 50.80574312757662, 51.53061177555716, 
          55.559121951833006))
sites$abbr <- c("CAS", "MTB", "KIL", "EAS", "MEN", "LEE", "OVI", "SHO", "BEA")

xlim <- c(min(sites$lon)-3.5, max(sites$lon)+2)
ylim <- c(min(sites$lat)-0.5, max(sites$lat)+3)

# Create empty plotting area
map("world", region="UK",
    xlim=xlim, ylim=ylim,
    fill=TRUE,
    col="grey90",
    border="grey40",
    lwd=0.8,
    bg="white")

# Add coordinate grid
abline(v=seq(-8, 2, by=1), col="grey80", lty=3)
abline(h=seq(50, 60, by=1), col="grey80", lty=3)

# Add site points
points(sites$lon, sites$lat,
       pch=21,
       bg="steelblue",
       col="black",
       cex=1)
text(sites$lon, sites$lat,
     labels = sites$abbr,
     pos = 4,
     cex = 0.9,
     col = "white")

text(sites$lon, sites$lat,
     labels = sites$abbr,
     pos = 4,
     cex = 0.8,
     font = 2)

# Add professional axes
axis(1, at=seq(-8,2,by=2), labels=paste0(seq(-8,2,by=2),"°"))
axis(2, at=seq(50,60,by=2), labels=paste0(seq(50,60,by=2),"°"), las=1)

