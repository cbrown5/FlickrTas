# ---------------
# Mapping Flickr data
# ---------------
#CJ Brown 16 Apr 2017

rm(list = ls())
library(rgdal)
library(raster)
library(rgeos)
library(dplyr)
library(PlotTools)
library(PNG)

#Recorded accuracy level of the location information. World level is 1, Country is ~3, Region ~6, City ~11, Street ~16. Current range is 1-16. Defaults to 16 if not specified.

# ---------------
# Data
# ---------------
setwd("~/Code/FlickrTas/data")
landtenure <- readOGR("LIST_LAND_TENURE_STATEWIDE","list_land_tenure_statewide")
#Landtenure available at https://www.thelist.tas.gov.au/app/content/data/geo-meta-data-record?detailRecordUID=9b8bf099-d668-433d-981b-a0f8f964f827

spnp <- subset(landtenure, TEN_CLASS == "National Park")
spnp2 <- gSimplify(spnp, 100)
spnr <- subset(landtenure, TEN_CLASS == "Conservation Area")
spnr2 <- gSimplify(spnr, 100)


# Create coastalines dataset for Tasmania and reproject to local UTM
# Data available at http://openstreetmapdata.com/data/coastlines
#setwd("~/Databases/general_map_data/coastlines-generalized-3857")
#coast <- readOGR(".", "coastlines_z7")
#coastll <- spTransform(coast, sp::CRS("+init=epsg:4326"))
#coasttas <- crop(coastll, extent(143, 149, -44, -39)) %>%
#    spTransform(CRS(proj4string(landtenure)))

#writeOGR(coasttas, dsn = ".",layer = "coasttas",driver="ESRI Shapefile", overwrite = TRUE)

#load coastlines data
coasttas <- readOGR(".", "coasttas")

#
# load coordinates
#
files <- list.files()
ixy <- grep("xy", files)
nfiles <- length(ixy)

xylist <- NULL

for (ifiles in 1:nfiles){
    load(files[ixy[ifiles]])
    xylist <- c(xylist, list(savelist$xy))
}

dat <- do.call(rbind, xylist)
dat <- dat[!is.na(dat$x),]
head(dat)


coordinates(dat) <- ~ y + x
proj4string(dat) <- sp::CRS("+init=epsg:4326")
dat2 <- spTransform(dat, CRS(proj4string(landtenure)))

# ---------------
# Analysis
# ---------------

#
# Number of photos per user
#

datc <- dat2@data %>% group_by(author) %>%
    summarize(n = n()) %>% arrange(desc(n))

par(las = 1)
barplot(datc$n, xlab = "Rank order", ylab = "Count of photos",
    names.arg = 1:nrow(datc))

#
# Tenure of photo
#

dover <- over(dat2, landtenure, returnList = TRUE)
pts_tenure <- lapply(dover, function(x) as.character(x$TEN_CLASS)) %>% unlist()

pts_tenure[pts_tenure == "Future Potential Production Forest (Crown)"] <- "Crown Land"
datt <- data.frame(tenure = pts_tenure) %>%
    group_by(tenure) %>% summarize(n = n()) %>%
    arrange(desc(n))

par(las = 1, mar = c(9, 4,1,1))
xbar <- barplot(datt$n, ylab = "Count of photos",
    names.arg = NA)
text(xbar+0.5, rep(-2, nrow(datt)),
    datt$tenure, srt = 45, xpd = NA, cex = 0.6, pos = 2)

# ---------------
# Map
# ---------------

par(mar = c(1,1,1,1))
plot(coasttas)
plot(spnp2, add = T, col = hexalpha("steelblue", 0.5), border = NA)
plot(spnr2, add = T, col = hexalpha("sandybrown", 0.5), border = NA)
points(dat2$y, dat2$x, pch = 16, col = hexalpha('red', 0.7), cex = 0.5)
raster::scalebar(100*1000, xy = c(573083.81,5175741.17),
    label = "100km")

legend(x = 239117-40000, y= 5289838,
    legend = c("National Park", "Nature Reserve", "Photos"),
    fill = c("steelblue","sandybrown",NA),
    pch = c(NA, NA, 16),
    col = c(NA, NA, "red"),
    border = c(NA, NA, NA))



# ---------------
# Voronoi
# ---------------

#Convert lines to polygons
pcoast <- maptools::SpatialLines2PolySet(coasttas) %>%
    maptools::PolySet2SpatialPolygons()

vpts <- dismo::voronoi(dat2, ext = extent(coasttas))
vpts2 <- gIntersection(pcoast, vpts, byid = TRUE, drop_lower_td = TRUE)

areas <- gArea(vpts2, byid = T)
cols <- leaflet::colorNumeric(
    rev(RColorBrewer::brewer.pal(9, "Greens")),
    domain = areas, alpha = 0.5)


ppi <- 300
asp <- (extent(coasttas)[4] - extent(coasttas)[3]) / (extent(coasttas)[2] - extent(coasttas)[1])
width <- 8

png("tas_voronoi.png", width = width * ppi, height = width*ppi*asp,
        res = ppi, antialias = 'none')

par(mar = c(1,1,1,1))
plot(vpts2, border = "grey", lwd = 0.5,
    col = cols(areas), bg = hexalpha("lightblue", 0.5),
    main = "Flickr photos of nature in Tasmania")
plot(coasttas, col = "grey", add = T, lwd = 0.6)
points(dat2$y, dat2$x, pch = 16, col = hexalpha('white', 0.7), cex = 0.2)
raster::scalebar(100*1000, xy = c(573083-40000, 5175741),
    label = "100km", cex = 1.2)

leg.areas <- c(1000*10000, 150000*10000, 300000*10000)
legend(x = 239117, y= 5289838-20000,
    legend = signif(leg.areas/(10000*1000),2),
    fill = cols(leg.areas),
    border = "grey",
    title = "Density of Photos \n (per 1000 ha)",
    bty = "n", cex = 1.3)

text(167142+50000, 5171550-20000,"Data for coastlines are from OpenStreetMap. Reprojected to UTM Zone 55S. \n License at http://creativecommons.org/licenses/by-sa/4.0/", pos = 4, cex = 0.9, col = "grey30")

dev.off()
