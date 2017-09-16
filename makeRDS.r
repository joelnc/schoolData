rm(list=ls())
library(leaflet)
##library(sp)
library(rgdal)



## School districts
sds <- readOGR(dsn=".", layer="tl_2013_37_unsd",
                 verbose=TRUE, GDAL1_integer64_policy = TRUE)

sds <- spTransform(sds, CRS("+proj=longlat +datum=WGS84 +no_defs"))
saveRDS(sds, file="sds.RDS")













streams <- readOGR(dsn=".", layer="ReedyStreamsFinal",
                   verbose=TRUE, GDAL1_integer64_policy = TRUE)
streams <- spTransform(streams, CRS("+proj=longlat +datum=WGS84 +no_defs"))
saveRDS(streams, file="MY13St.RDS")



## Site coordinates
sites <- read.csv(file="c:/users/95218/documents/r/public/sites.csv",
                    stringsAsFactors=FALSE, header=TRUE, sep=",")
save(sites, file="sitesCoor.rdata")


## MY13 shed and stream
reedy <- readOGR(dsn=".", layer="ReedyShape",
                 verbose=TRUE, GDAL1_integer64_policy = TRUE)
reedy <- spTransform(reedy, CRS("+proj=longlat +datum=WGS84 +no_defs"))
saveRDS(reedy, file="MY13Ws.RDS")

streams <- readOGR(dsn=".", layer="ReedyStreamsFinal",
                   verbose=TRUE, GDAL1_integer64_policy = TRUE)
streams <- spTransform(streams, CRS("+proj=longlat +datum=WGS84 +no_defs"))
saveRDS(streams, file="MY13St.RDS")

## Phase 2
siteCodes <- list(NORMAN=NA, ROCKYR="MY1B", MCDOWELL=c("MC4", "MC2"),
                  CLARKE="MY10", UMTNISLE=NA, LCLARKE=NA,
                  GAR="MC50", LONG="MC14A", MALLARD="MY11B",
                  LMTNISLE=NA, IRWIN="MC22A", BACK=c("MY12", "MY12B"),
                  ULSUGAR="MC29A1", PAW="MC17", CATAWBA=NA,
                  REEDY=c("MY13", "MY13A", "MY13B", "MY13C"),
                  BRIAR=c("MC30A","MC33", "MC31A"), MCKEE="MY7B",
                  SUGAR=c("MC25", "MC27"), CALDWELL=NA,
                  MCALPINE=c("MC36", "MC38", "MC45", "MC45B"),
                  CLEAR="MY8", BEAVERDAM="MC66", GOOSE=c("MY14", "MY9"),
                  MCMULLEN="MC42", STEELE="MC47A", LLSUGAR="MC49A",
                  WYLIE=NA, CROOKED=NA, FOURMILE=c("MC40A", "MC40C"),
                  TWELVEMILE=NA, SIXMILE="MC51", CLEM=NA)

## Get watershed shapefiles present
wsSites <- unique(sub(pattern="(.*?)\\..*$", replacement="\\1",
                      basename(list.files("c:/users/95218/documents/r/public/leaflet/ws/"))))

rdsWs <- function(site) {
##browser()
    siteShape <- readOGR(dsn="c:/users/95218/documents/r/public/leaflet/ws",
                                  layer=site,
                                  verbose=TRUE, GDAL1_integer64_policy = TRUE)
    siteShape <- spTransform(siteShape, CRS("+proj=longlat +datum=WGS84 +no_defs"))
    saveRDS(siteShape, file=paste0(site, "ws.RDS"))
}

junk <- lapply(X=wsSites, FUN=rdsWs)


## Streams
streamSites <- unique(sub(pattern="(.*?)\\..*$", replacement="\\1",
                      basename(list.files("c:/users/95218/documents/r/public/leaflet/streams/"))))

rdsStream <- function(site) {
    streamShape <- readOGR(dsn="c:/users/95218/documents/r/public/leaflet/streams",
                                  layer=site,
                                  verbose=TRUE, GDAL1_integer64_policy = TRUE)
    streamShape <- spTransform(streamShape, CRS("+proj=longlat +datum=WGS84 +no_defs"))
    saveRDS(streamShape, file=paste0(site, ".RDS"))
}

junk <- lapply(X=streamSites, FUN=rdsStream)


## Stations
stationSites <- unique(sub(pattern="(.*?)\\..*$", replacement="\\1",
                      basename(list.files("c:/users/95218/documents/r/public/leaflet/stations/"))))
stationSites <- stationSites[-c(4,5,6,8,9,13,14,16,23,27,30,32,33)]
rdsStation <- function(site) {
print(site)
    if (file.exists(paste0("C:/Users/95218/documents/R/public/leaflet/stations/",site,".shp"))) {
        stationShape <- readOGR(dsn="c:/users/95218/documents/r/public/leaflet/stations",
                                layer=site, verbose=TRUE, GDAL1_integer64_policy = TRUE)
        stationShape <- spTransform(stationShape, CRS("+proj=longlat +datum=WGS84 +no_defs"))
        saveRDS(stationShape, file=paste0(site, ".RDS"))
    }
}
junk <- lapply(X=stationSites, FUN=rdsStation)

## All stations
stationShape <- readOGR(dsn="c:/users/95218/documents/r/public/leaflet",
                                layer="cmStations", verbose=TRUE, GDAL1_integer64_policy = TRUE)
stationShape <- spTransform(stationShape, CRS("+proj=longlat +datum=WGS84 +no_defs"))
saveRDS(stationShape, file="allStations.RDS")
















## Older stuff

streams <- readRDS("streams.RDS")

m <- leaflet() %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Mapnik") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "EsriWorld") %>%
    addMarkers(lng=-80.662587, lat=35.259081, popup="MY13") %>%
    addPolygons(data=reedy, group="Watershed",
                weight=2, opacity=1, fillOpacity=0.1,
                popup="Reedy Creek Watershed") %>%
    addPolylines(data=streams, group="Stream",
                 opacity=1, weight=1) %>%
    addLayersControl(
        baseGroups = c("Mapnik", "EsriWorld"),
        overlayGroups = c("Watershed", "Stream")
    )

m


shapeData <- readOGR("reedyShape/ReedyShape.shp",
                     GDAL1_integer64_policy = TRUE)

leaflet(reedy)


m <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng = 174.768, lat = -36.852,
             popup = "The birthplace of R")
m  # Print the map


m <- leaflet() %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
    addMarkers(lng=-80.678, lat=35.259, popup="Reedy?")


m <- leaflet() %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik)

m <- addPolygons(data=shapeData)
m

mb_tiles <- "http://a.tiles.mapbox.com/v3/kwalkertcu.l1fc0hab/{z}/{x}/{y}.png"

mb_attribution <- 'Mapbox <a href="http://mapbox.com/about/maps" target="_blank">Terms &amp; Feedback</a>'


## Create the map

leaflet() %>%
  addTiles(urlTemplate = mb_tiles,
           attribution = mb_attribution)

m <- leaflet() %>% setView(lng = -71.0589, lat = 42.3601, zoom = 12)
m %>% addTiles()

leaflet() %>%
  setView(11.965053, 57.70451, 16) %>%
  addTiles() %>%
    addMarkers(11.965053, 57.70451) %>%
    options(incl.data=TRUE)

m = leaflet()

# we want to add map tiles so we use the addTiles() function - the default is openstreetmap
m = addTiles(m)

# we can add markers by using the addMarkers() function
m = addMarkers(m, lng=-123.256168, lat=49.266063, popup="T")

