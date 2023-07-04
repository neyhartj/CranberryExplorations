## Plot cranberry occurrence data
##

library(tidyverse)
library(readxl)
library(neyhart)
library(wesanderson)
library(geodata)

fig_dir <- "output"


# Read in data ------------------------------------------------------------

dat <- read_excel(path = "data/cranberry_occurrence_data.xlsx")




# Plot --------------------------------------------------------------------

xlim <- c(min(dat$longitude, na.rm = TRUE), -55)
ylim <- range(dat$latitude, na.rm = TRUE)

# Colors for source
# source_colors <- setNames(umn_palette(3)[3:5], unique(dat$source))
# source_colors <- setNames(umn_palette(2)[3:5], unique(dat$source))
# source_colors <- setNames(neyhart_palette("barley")[1:3], unique(dat$source))
source_colors <- setNames(neyhart_palette("barley")[1:3], c("GRIN", "iNaturalist", "GBIF"))


# Create a base map
g_base_map <- ggplot(data = north_america_mapdata, aes(x = long, y = lat)) +
  geom_polygon(fill = "white") +
  geom_polygon(data = subset(north_america_mapdata, area == "canada"),
               aes(group = group), fill = "grey95", color = "grey50", linewidth = 0.5) + # Add canada
  geom_polygon(data = subset(north_america_mapdata, area == "usa_state"),
               aes(group = group), fill = "grey95", color = "grey50", linewidth = 0.5) +  # Add location points
  coord_map(projection = "bonne", lat0 = mean(ylim), xlim = xlim, ylim = ylim) +
  scale_color_manual(values = source_colors) +
  theme_void(base_size = 14) +
  theme(legend.position = "top", legend.box = "vertical", legend.box.just = "left")

# Add points
g_cran_points <- g_base_map +
  geom_point(data = dat, aes(x = longitude, y = latitude, color = source, shape = source))

# Different size points
g_cran_points1 <- g_base_map +
  geom_point(data = subset(dat, source != "GRIN"), size = 0.75,
             aes(x = longitude, y = latitude, color = source)) +
  geom_point(data = subset(dat, source == "GRIN"), size = 2,
             aes(x = longitude, y = latitude, color = source))

# Save this plot
ggsave(filename = "cranberry_occurrence_plot_points.jpg", plot = g_cran_points1,
       path = fig_dir, height = 6, width = 10, dpi = 500)




# Plot worldclim data for occurrences and collections ---------------------


# Get the worldclim data
env_dir <- "C:/Users/jeffrey.neyhart/OneDrive - USDA/Documents/CranberryLab/EnvironmentalData/"

# Read in already downloaded worldclim data
path <- file.path(env_dir, "WorldClim")
subdir <- file.path(path, "wc2.1_2.5m")
if (dir.exists(subdir)) {
  files <- list.files(path = subdir, pattern = ".tif", full.names = TRUE)
  worldclim_dat <- rast(files)
} else {
  worldclim_dat <- worldclim_global(var = "bio", res = "2.5", path = file.path(env_dir, "WorldClim"))
}



# Subset data for each coordinate
location_metadata1 <- dat %>%
  distinct(latitude, longitude) %>%
  filter_at(vars(latitude, longitude), all_vars(!is.na(.))) %>%
  mutate(site = paste0("site", seq_len(nrow(.))),
         data = list(NULL))

# # Iterate over rows
# location_worldclim_data <- location_metadata1 %>%
#   group_by(site) %>%
#   do(data = {
#
#     row <- .
#
#     # Get latitude/longitude
#     lat_i <- row$latitude[1]
#     long_i <- row$longitude[1]
#
#     # Build extent
#     geo_add <- 1e-5
#     extent_i <- extent(list(x = c(long_i - geo_add, long_i + geo_add),
#                             y = c(lat_i - geo_add, lat_i + geo_add)))
#
#     # Crop the data
#     worldclim_dat_i <- crop(x = worldclim_dat, y = extent_i)
#
#     # Create an empty data.frame
#     t(as.matrix(worldclim_dat_i)) %>%
#       as.data.frame() %>%
#       rownames_to_column("bioc_var") %>%
#       rename(value = V1) %>%
#       as_tibble()
#
#   })
#


for (i in seq_len(nrow(location_metadata1))) {

  # Get latitude/longitude
  lat_i <- location_metadata1$latitude[i]
  long_i <- location_metadata1$longitude[i]

  # Build extent
  geo_add <- 1e-5
  extent_i <- extent(list(x = c(long_i - geo_add, long_i + geo_add),
                          y = c(lat_i - geo_add, lat_i + geo_add)))

  # Crop the data
  worldclim_dat_i <- crop(x = worldclim_dat, y = extent_i)

  # Create an empty data.frame
  bioclim_dat <- t(as.matrix(worldclim_dat_i)) %>%
    as.data.frame() %>%
    rownames_to_column("bioc_var") %>%
    rename(value = V1) %>%
    as_tibble()

  # Add this tibble to the larger data.frame
  location_metadata1$data[[i]] <- bioclim_dat

}

# Unnest
location_metadata2 <- unnest(location_metadata1, data) %>%
  mutate(bioc_var = str_remove(bioc_var, "wc2\\.1_2\\.5m_")) %>%
  spread(bioc_var, value)

location_worldclim_data <- inner_join(dat, location_metadata2)

# Save
save("location_worldclim_data", file = "data/cranberry_occurrence_worldclim_data.RData")


