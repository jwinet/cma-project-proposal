library("leaflet")
library("readr")
library("sf")
library("dplyr")
library("lubridate")

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
st_write(wald_selection_wollerau, dsn="datasets/wald_selection_wollerau.gpkg") # save the forest layer for Wollerau

wald <- st_read("datasets/wald_selection_wollerau.gpkg")

# Vitaparcours Data

## Wollerau

### True location of posts

Stations_Wollerau_20260521_swisstopo <- st_read("datasets/Wollerau/Waypoint_wollerau_20260521.gpx", layer = "waypoints")

Stations_Wollerau_20260521_swisstopo <- st_transform(Stations_Wollerau_20260521_swisstopo, 2056)

### Tracked data 20260521

Swisstopo_Wollerau_20260521 <- st_read("datasets/Wollerau/Vitaparcours_wollerau_20260521.gpx", layer = "track_points")

Swisstopo_Wollerau_20260521 <- st_transform(Swisstopo_Wollerau_20260521, 2056)

Posmo_Wollerau_20260521 <- read_delim("datasets/Wollerau/posmo_20260521.csv", ",") |> 
  st_as_sf(coords = c("lon_x","lat_y"), crs = 4326) |> 
  mutate(datetime = with_tz(datetime, tzone = "Europe/Zurich")) |>
  filter(
    datetime>= as.POSIXct("2026-05-21 16:14:55", tz = "Europe/Zurich"),
    datetime < as.POSIXct("2026-05-21 17:09:19", tz = "Europe/Zurich")
  )

Posmo_Wollerau_20260521 <- st_transform(Posmo_Wollerau_20260521, 2056)

### Tracked data 20260524

Swisstopo_Wollerau_20260524 <- st_read("datasets/Wollerau/Vitaparcours_wollerau_20260524.gpx", layer = "track_points")

Swisstopo_Wollerau_20260524 <- st_transform(Swisstopo_Wollerau_20260524, 2056)

Posmo_Wollerau_20260524 <- read_delim("datasets/Wollerau/posmo_20260524.csv", ",")

Posmo_Wollerau_20260524 <- read_delim("datasets/Wollerau/posmo_20260524.csv", ",") |> 
  st_as_sf(coords = c("lon_x","lat_y"), crs = 4326) |> 
  mutate(datetime = with_tz(datetime, tzone = "Europe/Zurich")) |>
  filter(
    datetime>= as.POSIXct("2026-05-24 09:13:46", tz = "Europe/Zurich"),
    datetime < as.POSIXct("2026-05-24 10:00:43", tz = "Europe/Zurich")
  )

Posmo_Wollerau_20260524 <- st_transform(Posmo_Wollerau_20260524, 2056)
