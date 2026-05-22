library(leaflet)
library(sf)
library(dplyr)

# Context Data forest

leaflet() %>%
  addTiles() %>%
  addWMSTiles(
    baseUrl = "https://wms.geo.admin.ch/",
    layers = "ch.swisstopo.swisstlm3d-wald",
    options = WMSTileOptions(format = "image/png", transparent = TRUE),
    attribution = "© swisstopo"
  )

Bodenbedeckung <- st_read("datasets/SWISSTLM3D_2026_LV95_LN02.gpkg", "tlm_bb_bodenbedeckung")
names(Bodenbedeckung)

wald <- Bodenbedeckung |> 
  filter(objektart == "Wald")

bbox <- st_bbox(c(xmin = 2694800.94, xmax = 2695780.92,
                  ymin = 1226662.62, ymax = 1227191.86),
                crs = st_crs(wald))


wald_selection_wollerau <- st_crop(wald, bbox)
st_write(wald_selection_wollerau, dsn="datasets/wald_selection_wollerau.gpkg")

plot(st_geometry(wald_selection_wollerau), col = "green")

# Vitaparcours Data with Swisstopo

## Wollerau

### True location of posts

Stations_Wollerau_20260521 <- st_read("datasets/Waypoint-2026-05-21_wollerau.gpx", layer = "waypoints")

### Tracked data
Data_Wollerau_20260521 <- st_read("datasets/Vitaparcours_wollerau_20260521.gpx", layer = "track_points")


