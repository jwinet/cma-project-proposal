library("leaflet")
library("readr")
library("sf")
library("dplyr")
library("lubridate")
library("zoo")

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

<<<<<<< HEAD




=======
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
>>>>>>> 1c74f28298a0f6d546ef2f5e70eb2a20de024181















##mis konzept


##Vita Olten 1 STRAVA

Vita <- read_delim("datasets/Vita_1/track_points.csv", ",")


Vita_sf <- st_as_sf(
  Vita,
  coords = c("X", "Y"),
  crs = 2056, remove = FALSE)




##Vita Olten 2 STRAVA

Vita2 <- read_delim("datasets/Vita_2/track_points.csv", ",")


Vita_sf2 <- st_as_sf(
  Vita2,
  coords = c("X", "Y"),
  crs = 2056, remove = FALSE)


##Wollerau1

Wollerau_Vita <- read_delim("datasets/Test_Wollerau/Vita1/track_points.csv", ",")


Wollerau_Vita_sf <- st_as_sf(
  Wollerau_Vita,
  coords = c("X", "Y"),
  crs = 2056, remove = FALSE)


##Wollerau2

Wollerau_Vita2 <- read_delim("datasets/Test_Wollerau/Vita2/track_points.csv", ",")


Wollerau_Vita_sf2 <- st_as_sf(
  Wollerau_Vita2,
  coords = c("X", "Y"),
  crs = 2056, remove = FALSE)


##Waypoints Olten


wpolten <- read_delim("datasets/Olten_Waypoints/Waypoints.csv", ",")


wpolten_sf <- st_as_sf(
  wpolten,
  coords = c("X", "Y"),
  crs = 2056, remove = FALSE)


##Waypoints Wollerau

wpwollerau <- read_delim("datasets/Test_Wollerau/wp_wollerau.csv", ",")


wpwollerau_sf <- st_as_sf(
  wpwollerau,
  coords = c("X", "Y"),
  crs = 2056, remove = FALSE)



##Posmo Wollerau 1

Posmo <- read_delim("datasets/Test_Wollerau/Posmo/posmo_20260521.csv", ",")

names(Posmo)[names(Posmo) == "lon_x"] <- "X"
names(Posmo)[names(Posmo) == "lat_y"] <- "Y"

names(Posmo)[names(Posmo) == "datetime"] <- "time"
names(Posmo)[names(Posmo) == "datetime"] <- "time"


Posmo <- Posmo |> 
  st_as_sf(coords = c("X","Y"), crs = 4326, remove = FALSE) |> 
  mutate(time = with_tz(time, tzone = "Europe/Zurich")) |>
  filter(
    time>= as.POSIXct("2026-05-21 16:14:55", tz = "Europe/Zurich"),
    time < as.POSIXct("2026-05-21 17:09:19", tz = "Europe/Zurich")
  )

Posmo <- st_transform(Posmo, 2056)

Posmo[, c("X", "Y")] <- st_coordinates(Posmo)

##Posmo Wollerau 2

Posmo2 <- read_delim("datasets/Test_Wollerau/Posmo/posmo_20260524.csv", ",")

names(Posmo2)[names(Posmo2) == "lon_x"] <- "X"
names(Posmo2)[names(Posmo2) == "lat_y"] <- "Y"

names(Posmo2)[names(Posmo2) == "datetime"] <- "time"
names(Posmo2)[names(Posmo2) == "datetime"] <- "time"

Posmo2 <- Posmo2 |> 
  st_as_sf(coords = c("X","Y"), crs = 4326, remove = FALSE) |> 
  mutate(time = with_tz(time, tzone = "Europe/Zurich")) |>
  filter(
    time>= as.POSIXct("2026-05-24 09:13:46", tz = "Europe/Zurich"),
    time < as.POSIXct("2026-05-24 10:00:43", tz = "Europe/Zurich")
  )

Posmo2 <- st_transform(Posmo2, 2056)

Posmo2[, c("X", "Y")] <- st_coordinates(Posmo2)














##Scaling

Vita_sf$time <- as.POSIXct(Vita_sf$time, tz="Europe/Zurich")
Posmo$time <- as.POSIXct(Posmo$time, tz="Europe/Zurich")
Wollerau_Vita_sf$time <- as.POSIXct(Wollerau_Vita_sf$time, tz="Europe/Zurich")
Vita_sf2$time <- as.POSIXct(Vita_sf2$time, tz="Europe/Zurich")
Posmo2$time <- as.POSIXct(Posmo2$time, tz="Europe/Zurich")
Wollerau_Vita_sf2$time <- as.POSIXct(Wollerau_Vita_sf2$time, tz="Europe/Zurich")




Vita_sf <- Vita_sf[order(Vita_sf$time), ]
Vita_sf <- Vita_sf[!duplicated(Vita_sf$time), ]

Posmo <- Posmo[order(Posmo$time), ]
Posmo <- Posmo[!duplicated(Posmo$time), ]

Wollerau_Vita_sf <- Wollerau_Vita_sf[order(Wollerau_Vita_sf$time), ]
Wollerau_Vita_sf <- Wollerau_Vita_sf[!duplicated(Wollerau_Vita_sf$time), ]

Vita_sf2 <- Vita_sf2[order(Vita_sf2$time), ]
Vita_sf2 <- Vita_sf2[!duplicated(Vita_sf2$time), ]

Posmo2 <- Posmo2[order(Posmo2$time), ]
Posmo2 <- Posmo2[!duplicated(Posmo2$time), ]

Wollerau_Vita_sf2 <- Wollerau_Vita_sf2[order(Wollerau_Vita_sf2$time), ]
Wollerau_Vita_sf2 <- Wollerau_Vita_sf2[!duplicated(Wollerau_Vita_sf2$time), ]



time_grid <- seq(
  from = min(
    Vita_sf$time,
    Posmo$time,
    Wollerau_Vita_sf$time,
    Vita_sf2$time,
    Posmo2$time,
    Wollerau_Vita_sf2$time,
    na.rm = TRUE
  ),
  to = max(
    Vita_sf2$time,
    Posmo2$time,
    Wollerau_Vita_sf2$time,
    na.rm = TRUE
  ),
  by = "5 sec"   # <- hier definierst du dein Intervall
)


resample_track <- function(df, time_grid) {
  
  df <- df[order(df$time), ]
  df <- df[!duplicated(df$time), ]
  
  data.frame(
    time = time_grid,
    X = approx(df$time, df$X, xout = time_grid, rule = 2)$y,
    Y = approx(df$time, df$Y, xout = time_grid, rule = 2)$y
  )
  

}

Vita_resampled <- resample_track(Vita_sf, time_grid)
Posmo_resampled <- resample_track(Posmo, time_grid)
Wollerau_resampled <- resample_track(Wollerau_Vita_sf, time_grid)
Vita2_resampled <- resample_track(Vita_sf2, time_grid)
Posmo2_resampled <- resample_track(Posmo2, time_grid)
Wollerau2_resampled <- resample_track(Wollerau_Vita_sf2, time_grid)


Vita_resampled <- st_as_sf(Vita_resampled, coords = c("X","Y"), crs = 2056)
Posmo_resampled <- st_as_sf(Posmo_resampled, coords = c("X","Y"), crs = 2056)
Wollerau_resampled <- st_as_sf(Wollerau_resampled, coords = c("X","Y"), crs = 2056)

Vita2_resampled <- st_as_sf(Vita2_resampled, coords = c("X","Y"), crs = 2056)
Posmo2_resampled <- st_as_sf(Posmo2_resampled, coords = c("X","Y"), crs = 2056)
Wollerau2_resampled <- st_as_sf(Wollerau2_resampled, coords = c("X","Y"), crs = 2056)


coords <- st_coordinates(Vita_resampled)
Vita_resampled$X <- coords[,1]
Vita_resampled$Y <- coords[,2]

coords <- st_coordinates(Posmo_resampled)
Posmo_resampled$X <- coords[,1]
Posmo_resampled$Y <- coords[,2]

coords <- st_coordinates(Wollerau_resampled)
Wollerau_resampled$X <- coords[,1]
Wollerau_resampled$Y <- coords[,2]

coords <- st_coordinates(Vita2_resampled)
Vita2_resampled$X <- coords[,1]
Vita2_resampled$Y <- coords[,2]

coords <- st_coordinates(Posmo2_resampled)
Posmo2_resampled$X <- coords[,1]
Posmo2_resampled$Y <- coords[,2]

coords <- st_coordinates(Wollerau2_resampled)
Wollerau2_resampled$X <- coords[,1]
Wollerau2_resampled$Y <- coords[,2]
