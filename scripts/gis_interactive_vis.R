# setup -------------------------------------------------------------------

library(leaflet)
library(tidyverse)

# basic map ---------------------------------------------------------------

map_lng <- -78.16478
map_lat <- 38.88687

tibble(
    long = map_lng,
    lat = map_lat
  ) %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(popup="Our classroom")

# coding an initial zoom level --------------------------------------------

ourMap <-
  tibble(
    long = map_lng,
    lat = map_lat
  ) %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(popup="Our classroom") %>%
  setView(
    lng = map_lng,
    lat = map_lat,
    zoom = 4
  )

# adding tiles ------------------------------------------------------------

# Here's a fun one!

ourMap %>%
  addWMSTiles(
    "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
    layers = "nexrad-n0r-900913",
    options = WMSTileOptions(format = "image/png", transparent = TRUE))

# Some bread-and-butter tiles:

# See: http://leaflet-extras.github.io/leaflet-providers/preview/index.html

ourMap %>%
  addProviderTiles('Esri.NatGeoWorldMap')

ourMap %>%
  addProviderTiles('Esri.WorldImagery')

ourMap %>%
  addProviderTiles('Esri.OceanBasemap')

ourMap %>%
  addProviderTiles('NASAGIBS.ViirsEarthAtNight2012')

# Adding groups -----------------------------------------------------------

ourMap %>%
  addProviderTiles('OpenStreetMap', group = 'Open street map') %>%
  addProviderTiles('Esri.NatGeoWorldMap', group = 'National Geographic') %>%
  addProviderTiles('Esri.WorldImagery', group = 'Orthophoto') %>%
  addProviderTiles('Esri.OceanBasemap', group = 'Bathymetry') %>%
  addProviderTiles('NASAGIBS.ViirsEarthAtNight2012', group = 'Night lights') %>%
  addLayersControl(
    baseGroups = c('Open street map','National Geographic', 'Orthophoto', 'Bathymetry', 'Night lights'),
    options = layersControlOptions(collapsed = FALSE)
  )

# some real data ----------------------------------------------------------

redListed <-
  occurrence(
    startdate = '2018-01-01',
    enddate = '2018-12-31',
    redlist = TRUE
  ) %>%
  as_tibble() %>%
  select(
    long = decimalLongitude,
    lat = decimalLatitude,
    spp = vernacularName
  ) %>%
  filter(
    long < -70 & long > -98,
    lat > 15 & lat < 32
  )

# Add clusters:

redList_map <-
  redListed %>%
  filter(!is.na(spp)) %>%
  filter(str_detect(spp, 'Manatee') | str_detect(spp, 'Whale')) %>%
  leaflet() %>%
  addProviderTiles('Esri.OceanBasemap') %>%
  addMarkers(
    clusterOptions = markerClusterOptions(),
    popup = ~ spp
  )

# Add rectangle describing the study extent:

redList_map %>%
  addRectangles(
    lng1=-70, lat1=15,
    lng2=-98, lat2=32,
    fillColor = "transparent"
  )


# Viewing and sharing your files interactively! Say no to cartogra --------

# There are plenty of applications where it is necessary to make official maps (e.g., publications). Most of the time, however, we use maps to communicate information to our colleagues. In these instances, isn't it better to produce a map that folks can actually interact with? There are lots of ways to explore maps interactively, one way that's fun is to share maps via Google Earth:

# Replace the x below with the (cropped) raster file associated with the first day of class:

KML(
  x,
  'day1.kml',
  col = rev(rainbow(100)),
  colNA = NA)

# Open up Google Earth and add your data and take a look! To do so, navigate to, and double-click on your KML file in your file explorer. 

