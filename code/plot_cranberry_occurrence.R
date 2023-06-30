## Plot cranberry occurrence data
##

library(tidyverse)
library(readxl)
library(neyhart)
library(wesanderson)

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




