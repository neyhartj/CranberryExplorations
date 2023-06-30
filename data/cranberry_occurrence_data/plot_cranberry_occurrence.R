## Plot cranberry occurrence data
##

library(tidyverse)
library(readxl)
library(neyhart)



# Read in data ------------------------------------------------------------

dat <- read_excel(path = "cranberry_occurrence_data.xlsx")




# Plot --------------------------------------------------------------------

xlim <- c(-100, -50)
ylim <- range(dat$latitude, na.rm = TRUE)


g_base_map <- ggplot(data = north_america_mapdata, aes(x = long, y = lat)) +
  geom_polygon(fill = "white") +
  geom_polygon(data = subset(north_america_mapdata, area == "canada"),
               aes(group = group), fill = "grey95", color = "grey50", linewidth = 0.5) + # Add canada
  geom_polygon(data = subset(north_america_mapdata, area == "usa_state"),
               aes(group = group), fill = "grey95", color = "grey50", linewidth = 0.5) +  # Add location points
  geom_point(data = dat, aes(x = longitude, y = latitude, color = source)) +
  coord_map(projection = "bonne", lat0 = mean(ylim), xlim = xlim, ylim = ylim) +
  theme_void(base_size = 14) +
  theme(legend.position = "top", legend.box = "vertical", legend.box.just = "left")


