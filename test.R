# https://tutorials.inbo.be/tutorials/spatial_wms_services/
# https://trafficonese.github.io/leaflet.extras2/

library(leaflet)
library(leaflet.extras2)

leaflet() %>%
  addTiles() %>%
  addWMSTiles(
    baseUrl= "https://cwfis.cfs.nrcan.gc.ca/geoserver/wms",
    layers = "public:nbac",
    options = WMSTileOptions(
      format = "image/png",
      transparent = TRUE)) %>%
  setView(lng = -100, lat = 60, zoom = 4)

leaflet() %>%
  addTiles(group = "base") %>%
  setView(lng = -100, lat = 60, zoom = 4) %>%
  addWMS(baseUrl = "https://cwfis.cfs.nrcan.gc.ca/geoserver/wms",
         layers = "public:nbac",
      popupOptions = popupOptions(maxWidth = 600),
      checkempty = TRUE,
      options = WMSTileOptions(
        transparent = TRUE,
        format = "image/png",
        info_format = "text/html"))
