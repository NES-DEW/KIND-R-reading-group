# this session = how far can we get with chapter 2?
# https://r.geocompx.org/spatial-class

# first impressions: equals assignment and lots of base-R plot()

# packages ----
# remotes::install_github("geocompx/geocompkg") # all dependencies
library(tidyverse)
library(sf) # vectors
library(terra) # raster
library(spData)
library(spDataLarge)
library(sfheaders)

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

# look at the data. Standard tibble plus something new
world

# mega-advantage: other than the geom column, a standard tibble
world |>
  skimr::skim() 

# multipolygon list col. Some countries have islands!
spData::world$geom 

plot(world) # fast but horrible

world |>
  # filter(continent %in% c("Antarctica", "Seven seas (open ocean)")) |>
  # filter(iso_a2 %in% c("GB", "ES", "DE", "NL", "FR", "BE", "LU", "PT", "IT", "AT", "CH", "IE")) |>
  # filter(name_long == "Switzerland") |>
  ggplot() +
  geom_sf(aes(fill = continent)) + # easy to ggplot
  theme_void() +
  theme(legend.position = "bottom") 
  # xlim(c(-10,18)) +
  # ylim(c(25, 60))

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


# constructing an sf object ----

lnd_point <- st_point(c(0.1, 51.5)) # sfg - simple feature geometry - object describing a point

lnd_geom <- st_sfc(lnd_point, crs = "EPSG:4326") # convert the st_point into sfc - simple feature geometry column - by adding a CRS

lnd_attrib = tibble( # then make non-geographical data in a data.frame/tibble
  name = "London",
  temperature = 25,
  date = as.Date("2023-06-21")
)

lnd_sf = st_sf(lnd_attrib, geometry = lnd_geom) # sf object = sfc + non-geography

# or idiomatically with pipes
st_point(c(0.1, 51.5)) |>
  st_sfc(crs = "EPSG:4326") |>
  st_sf(lnd_attrib, geometry = _)

# matrixes!!
rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2)) |>
  st_linestring()

matrix(c(1,2,3,45,6,6,34,2,3,4,5,5), ncol = 2, byrow = T) # might be less annoying

# building data from sfheaders ----
# faster than sf constructors, and independent of them
v <- c(1,1)
v_sfg_sfh <- sfheaders::sfg_point(obj = v)
v_sfg_sfh

# data frames/tibbles
df <- data.frame(x = 1:4, y = 4:1)
sfheaders::sfg_polygon(obj = df) # to make an sfc
sfheaders::sf_polygon(obj = df) # to make an sf


# data from rubbish copilot, which thinks that Cumbernauld is a city and doesn't understand how populations work
scottish_cities <- tribble(
  ~ City,  ~ Latitude,  ~ Longitude,  ~ MeanAvgTempC,  ~ Population, 
  "Glasgow",55.8642,-4.2518,8.5,632350,
  "Edinburgh",55.9533,-3.1883,8.5,506520,
  "Aberdeen",57.1497,-2.0943,8.0,198590,
  "Dundee",56.4620,-2.9707,8.2,148210,
  "Inverness",57.4778,-4.2247,7.8,47000,
  "Stirling",56.1165,-3.9369,8.1,37910,
  "Perth",56.3969,-3.4370,8.0,47000,
  "Dunfermline",56.0717,-3.4521,8.2,54990
) 

scottish_cities |>
  # select(Latitude, Longitude) |>
  sf_point(x = "Longitude", y = "Latitude", keep = T)  |> # beware of the non-tidy eval
  st_set_crs("EPSG:4326") |>
  ggplot() +
  geom_sf() +
  geom_sf_label(aes(label = City))

# return to spherical geometry later??