library("leaflet")
library("readr")
library("sf")
library("dplyr")
library("lubridate")
library("zoo")
library("purrr")
library("stringr")

# Context Data forest

leaflet()  |> 
  addTiles()  |> 
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



# Vitaparcours Data created: 04.06.2026

## STRAVA Vita 1 Olten 

Strava_1 <- read_delim("datasets/Vita_1/track_points.csv", ",") # load data

Strava_1 <- st_as_sf(
  Strava_1,
  coords = c("X", "Y"),
  crs = 2056, remove = FALSE) # creat geometry variable

Strava_1$time <- with_tz(as.POSIXct(Strava_1$time, tz = "UTC"), tz="Europe/Zurich") # convert to our time zone

## STRAVA Vita 2 Olten 

Strava_2 <- read_delim("datasets/Vita_2/track_points.csv", ",") # load data

Strava_2 <- st_as_sf(
  Strava_2,
  coords = c("X", "Y"),
  crs = 2056, remove = FALSE) # creat geometry variable

Strava_2$time <- with_tz(as.POSIXct(Strava_2$time, tz = "UTC"), tz="Europe/Zurich") # convert to our time zone

## Waypoints Olten

wp_olten <- read_delim("datasets/Olten_Waypoints/Waypoints.csv", ",") # load data

wp_olten <- st_as_sf(
  wp_olten,
  coords = c("X", "Y"),
  crs = 2056, remove = FALSE) # creat geometry variable

wp_olten_coords <- st_coordinates(wp_olten) # extract  coordinates out of the geometry column

# assign them to your new X and Y columns
wp_olten$X <- wp_olten_coords[, 1]
wp_oltenY <- wp_olten_coords[, 2]

wp_wollerau <- wp_wollerau[wp_wollerau$desc != "Start", ]

## Swisstopo 1 Wollerau 

Swisstopo_1 <- st_read("datasets/Wollerau/Vitaparcours_wollerau_20260521.gpx", layer = "track_points") # load data

Swisstopo_1 <- st_transform(Swisstopo_1, crs = 2056) # transform geometry variable

Swisstopo_1_coords <- st_coordinates(Swisstopo_1) # extract  coordinates out of the geometry column

# assign them to your new X and Y columns
Swisstopo_1$X <- Swisstopo_1_coords[, 1]
Swisstopo_1$Y <- Swisstopo_1_coords[, 2]

## Swisstopo 2 Wollerau 

Swisstopo_2 <- st_read("datasets/Wollerau/Vitaparcours_wollerau_20260524.gpx", layer = "track_points") # load data

Swisstopo_2 <- st_transform(Swisstopo_2, crs = 2056) # transform geometry variable

Swisstopo_2_coords <- st_coordinates(Swisstopo_2) # extract  coordinates out of the geometry column

# assign them to your new X and Y columns
Swisstopo_2$X <- Swisstopo_2_coords[, 1]
Swisstopo_2$Y <- Swisstopo_2_coords[, 2]

## Waypoints Wollerau

wp_wollerau <- st_read("datasets/Wollerau/Waypoint_wollerau_20260521.gpx", layer = "waypoints") # load data

wp_wollerau <- st_transform(wp_wollerau, crs = 2056) # transform geometry variable

wp_wollerau_coords <- st_coordinates(wp_wollerau) # extract  coordinates out of the geometry column

# assign them to your new X and Y columns
wp_wollerau$X <- wp_wollerau_coords[, 1]
wp_wollerau$Y <- wp_wollerau_coords[, 2]

wp_wollerau <- wp_wollerau[wp_wollerau$desc != "Start", ] # remove start coordinate

## Posmo 1 Wollerau 

Posmo_1 <- read_delim("datasets/Wollerau/posmo_20260521.csv", ",")  # load data

### create same collumn nmaes

names(Posmo_1)[names(Posmo_1) == "lon_x"] <- "X"
names(Posmo_1)[names(Posmo_1) == "lat_y"] <- "Y"

names(Posmo_1)[names(Posmo_1) == "datetime"] <- "time"

Posmo_1 <- Posmo_1 |> 
  st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE) |> 
  mutate(time = with_tz(time, tzone = "Europe/Zurich")) |>
  filter(
    time>= as.POSIXct("2026-05-21 16:14:55", tz = "Europe/Zurich"),
    time < as.POSIXct("2026-05-21 17:09:19", tz = "Europe/Zurich")
  ) # creat geometry variable and filter only vitaparcours tracks with the time 

Posmo_1 <- st_transform(Posmo_1, 2056) # transfrom to swiss coordinates

Posmo_1[, c("X", "Y")] <- st_coordinates(Posmo_1)

## Posmo 2 Wollerau

Posmo_2 <- read_delim("datasets/Wollerau/posmo_20260524.csv", ",") # load data

### create same collumn nmaes

names(Posmo_2)[names(Posmo_2) == "lon_x"] <- "X"
names(Posmo_2)[names(Posmo_2) == "lat_y"] <- "Y"

names(Posmo_2)[names(Posmo_2) == "datetime"] <- "time"

Posmo_2 <- Posmo_2 |> 
  st_as_sf(coords = c("X","Y"), crs = 4326, remove = FALSE) |> 
  mutate(time = with_tz(time, tzone = "Europe/Zurich")) |>
  filter(
    time>= as.POSIXct("2026-05-24 09:13:46", tz = "Europe/Zurich"),
    time < as.POSIXct("2026-05-24 10:00:43", tz = "Europe/Zurich")
  ) # creat geometry variable and filter only vitaparcours track with the time 

Posmo_2 <- st_transform(Posmo_2, 2056) # transform geometry to swiss coordinates

Posmo_2[, c("X", "Y")] <- st_coordinates(Posmo_2) # # transform X and Y to swiss coordinates

# Save all the tracks as new datasets

## define a clean and save function
save_spatial_gpkg <- function(df, file_name) {
  # Strip columns that are 100% NA
  df_cleaned <- df %>% select(where(~ !all(is.na(.))))
  
  # Set up path to datasets/cleaned
  output_path <- file.path("datasets", "cleaned", paste0(file_name, ".gpkg"))
  
  # Write file (overwrites existing files cleanly)
  st_write(df_cleaned, output_path, delete_dsn = TRUE, quiet = TRUE)
  
  message("Saved: ", output_path)
  return(df_cleaned)
}

## Save the cleaned datasets
Strava_1_clean   <- save_spatial_gpkg(Strava_1, "Strava_1_Olten")
Strava_2_clean   <- save_spatial_gpkg(Strava_2, "Strava_2_Olten")
wp_olten_clean   <- save_spatial_gpkg(wp_olten, "Waypoints_Olten")

Swisstopo_1_clean <- save_spatial_gpkg(Swisstopo_1, "Swisstopo_1_Wollerau")
Swisstopo_2_clean <- save_spatial_gpkg(Swisstopo_2, "Swisstopo_2_Wollerau")
wp_wollerau_clean <- save_spatial_gpkg(wp_wollerau, "Waypoints_Wollerau")
Posmo_1_clean     <- save_spatial_gpkg(Posmo_1, "Posmo_1_Wollerau")
Posmo_2_clean     <- save_spatial_gpkg(Posmo_2, "Posmo_2_Wollerau")


# Extract Static Events Using a Mean Temporal Window Distance

# sf_data data frame.
# time_col timestamp column name (must be POSIXct).
# window Total size of the time window in seconds.

classify_temporal_static <- function(sf_data, time_col, window) {
  
  # orders data chronologically
  sf_data <- sf_data |>  arrange(!!sym(time_col))
  
  geoms <- st_geometry(sf_data)
  timestamps <- sf_data[[time_col]]
  
  # devides window into 2
  half_window <- dseconds(window / 2)
  
  # 1. Compute the mean distance to ALL points inside the temporal window
  stepMean_vector <- map_dbl(seq_along(timestamps), function(i) {
    current_time <- timestamps[i]
    current_geom <- geoms[i]
    
    # Find indices of all points within the temporal window (-half_window to +half_window)
    in_window_indices <- which(timestamps >= (current_time - half_window) & 
                                 timestamps <= (current_time + half_window))
    
    # Exclude the current point itself
    in_window_indices <- in_window_indices[in_window_indices != i]
    
    if (length(in_window_indices) == 0) return(NA)
    
    # Calculate spatial distances from current point to all window points
    distances <- st_distance(current_geom, geoms[in_window_indices])
    
    # Return the mean of these window distances
    mean(as.numeric(distances), na.rm = TRUE)
  })
  
  #  adds stepMean
  sf_data$stepMean <- stepMean_vector
  
  # 3. Calculate the threshold
  global_threshold <- mean(sf_data$stepMean, na.rm = TRUE)
  
  # 4. If the window mean is below the global average -> it's static
  sf_data <- sf_data  |> 
    mutate(is_static = stepMean < global_threshold)
  
  return(sf_data)
}

# Apply the temporal window to datasets (needs some minutes to comupte)

## group base datasets into a named list
datasets <- list(
  Strava_1 = Strava_1_clean,
  Strava_2 = Strava_2_clean,
  Swisstopo_1 = Swisstopo_1_clean,
  Swisstopo_2 = Swisstopo_2_clean,
  Posmo_1 = Posmo_1_clean,
  Posmo_2 = Posmo_2_clean
)

## define the time windows
windows <- c(10, 20, 30, 40)

## loop through windows and datasets dynamically and save them

for (w in windows) {
  message(str_glue("\nProcessing {w}-second window..."))
  
  for (name in names(datasets)) {
    # Generate the clear variable name (e.g., "Strava_1_processed_10")
    output_name <- str_glue("{name}_processed_{w}")
    
    # Run the classification
    processed_data <- classify_temporal_static(datasets[[name]], time_col = "time", window = w)
    
    # Strip columns that are 100% NA
    processed_data <- processed_data %>% select(where(~ !all(is.na(.))))
    
    # Set up path specifically to datasets/cleaned/static
    output_path <- file.path("datasets", "cleaned", "static", paste0(output_name, ".gpkg"))
    
    # Automatically create the nested directories if they don't exist yet
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    
    # Write file (overwrites existing files cleanly)
    st_write(processed_data, output_path, delete_dsn = TRUE, quiet = TRUE)
    
    # Print custom save message to the console
    message("Saved: ", output_path)
    
    # Save the final cleaned data to your global environment
    assign(output_name, processed_data, envir = .GlobalEnv)
  }
}






# Test Rodrigo

## STRAVA Vita 1 Olten 

Vita <- read_delim("datasets/Vita_1/track_points.csv", ",")

Vita_sf <- st_as_sf(
  Vita,
  coords = c("X", "Y"),
  crs = 2056, remove = FALSE)

## STRAVA Vita 2 Olten 

Vita2 <- read_delim("datasets/Vita_2/track_points.csv", ",")


Vita_sf2 <- st_as_sf(
  Vita2,
  coords = c("X", "Y"),
  crs = 2056, remove = FALSE)

## Waypoints Olten

wpolten <- read_delim("datasets/Olten_Waypoints/Waypoints.csv", ",")

wpolten_sf <- st_as_sf(
  wpolten,
  coords = c("X", "Y"),
  crs = 2056, remove = FALSE)

## Swisstopo 1 Wollerau 

Wollerau_Vita <- read_delim("datasets/Test_Wollerau/Vita1/track_points.csv", ",")

Wollerau_Vita_sf <- st_as_sf(
  Wollerau_Vita,
  coords = c("X", "Y"),
  crs = 2056, remove = FALSE)

## Swisstopo 2 Wollerau 

Wollerau_Vita2 <- read_delim("datasets/Test_Wollerau/Vita2/track_points.csv", ",")

Wollerau_Vita_sf2 <- st_as_sf(
  Wollerau_Vita2,
  coords = c("X", "Y"),
  crs = 2056, remove = FALSE)

## Waypoints Wollerau

wpwollerau <- read_delim("datasets/Test_Wollerau/wp_wollerau.csv", ",")

wpwollerau_sf <- st_as_sf(
  wpwollerau,
  coords = c("X", "Y"),
  crs = 2056, remove = FALSE)

## Posmo 1 Wollerau 

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

## Posmo 2 Wollerau

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
