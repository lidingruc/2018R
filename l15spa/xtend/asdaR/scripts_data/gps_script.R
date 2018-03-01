wp0 <- readGPS()
wp0$long <- wp0$V9
wp0$lat <- wp0$V8
wp0$id <- as.character(wp0$V2)
wp0$alt <- as.numeric(substring(as.character(wp0$V19), 1, (nchar(as.character(wp0$V19))-1)))
wp0$time <- as.POSIXct(strptime(paste(as.character(wp0$V3), as.character(wp0$V4)), format="%d-%b-%y %H:%M:%S"))
wp <- wp0[c("long", "lat", "id", "alt", "time")]
coordinates(wp) <- c("long", "lat")
proj4string(wp) <- CRS("+proj=longlat +datum=WGS84")
wp
plot(wp, add=TRUE, col="red", lwd=2)