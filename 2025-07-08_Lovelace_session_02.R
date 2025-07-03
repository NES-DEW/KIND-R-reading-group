

# packages ----
# remotes::install_github("geocompx/geocompkg") # all dependencies
library(tidyverse)
library(sf) # vectors
library(terra) # raster
library(spData)
library(spDataLarge)

# vector vs raster ----

# vector = sf (simple features)
# points within coordinate reference system (CRS)
# point, line, polygons + 3D polygons
# lon/lat CRS
# Easting/Northing CRS

# sf ----

# point, multi
# linestring, multi
# poly, multi
# geometry collection (+ a load of rare oddballs)

vignette("sf1") # or better https://r-spatial.github.io/sf/articles/sf1.html

world
spData::world$geom # multipolygon list col. Some countries have islands!
plot(world) # fast but horrible
world |>
  ggplot() +
  geom_sf(aes(fill = continent)) + # easy to ggplot
  theme_void() +
  theme(legend.position = "bottom")

world |>
  skimr::skim() # mega-advantage: other than the geom column, a standard tibble

# that means we can generate mini-maps with a bit of dplyr...
world |>
  filter(continent == "Asia") |>
  ggplot() +
  geom_sf(aes(fill = name_long)) + # easy to ggplot
  geom_sf_label(aes(label = name_long), 
               size = 3, 
               position = "jitter") +
  theme_void() +
  theme(legend.position = "none")

# more fancy base plotting
world_asia = world[world$continent == "Asia", ]
asia = st_union(world_asia)
plot(world["pop"], reset = FALSE)
plot(asia, add = TRUE, col = "red")

# refactor with base-R pipe
plot(world["pop"], reset = FALSE)

world[world$continent == "Asia", ] |>
  st_union() |> # merges together several multipolygons into one
  plot(add = TRUE, col = "red")

# centroids
plot(world["continent"], reset = FALSE)
world_cents <- st_centroid(world, of_largest = TRUE) # points in the middle of largest poly
plot(st_geometry(world_cents), add = TRUE, cex = sqrt(world$pop) / 10000)

# fiddling bounding boxes = effectively the frame of the map

india = world[world$name_long == "India", ]
plot(st_geometry(india), col = "gray", expandBB = c(0, .2, .1, 1), lwd = 3)
plot(st_geometry(world_asia), add = TRUE)
