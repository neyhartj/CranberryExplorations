## Cranberry Collection
##
## Reformat iNaturalist observation data
##
## This script will read in the raw iNaturalist observation data and, remove irrelevant columns,
## and perform some basic filters
##
## Filters include:
## 1. Subset observations that coincide with public lands
## 2.
## 3.
##


# Load packages
library(tidyverse)
library(rgdal)

# Read in the data
data_file <- "data_raw/iNaturalistObservations/iNaturalist_Vmacrocarpon_observations_202306111.csv"
raw_data <- read_csv(file = data_file)


# Basic column subsetting -------------------------------------------------


# Select only relevant columns
raW_data1 <- raw_data %>%
  select(id, observed_on, quality_grade, url, image_url, tag_list, description,
         num_identification_agreements, num_identification_disagreements, captive_cultivated,
         place_guess, latitude, longitude, positional_accuracy, geoprivacy, taxon_geoprivacy,
         place_county_name, place_state_name, place_country_name, place_admin1_name,
         place_admin2_name) %>%
  # reorganize columns
  select(id, latitude, longitude, quality_grade, observed_on, positional_accuracy, geoprivacy, taxon_geoprivacy,
         starts_with("place"), names(.))


# Save the new CSV
write_csv(x = raW_data1, file = "data/Vmacrocarpon_inaturalist_observations_reformatted_20230611.csv")



# Identify observations in public lands -----------------------------------

# Create a spatial points DF from the occurence observations
vm_occ_points <- raW_data1
coordinates(vm_occ_points) <- c("longitude", "latitude")
proj4string(vm_occ_points) <- CRS("+proj=longlat +datum=NAD83 +units=m +no_defs")

# EXAMPLE

# List to store data
vm_occ_points_ann <- list()

# List the shapefiles
pad_shapefiles <- list.files(path = "data_raw/USGS_ProtectedAreasDatabase/", pattern = "Combined_State[A-Z]{2}.shp$", full.names = TRUE,
                             recursive = TRUE)

# Iterate over those shapefiles
for (i in seq_along(pad_shapefiles)) {

  # Read in example state shapefile
  state_pad_polygons <- readOGR(dsn = pad_shapefiles[i])

  # Transform spatial projection
  state_pad_polygons1 <- spTransform(x = state_pad_polygons, CRSobj = proj4string(vm_occ_points))

  # Which state are we dealing with?
  state_name <- unique(state_pad_polygons1$d_State_Nm)

  # Subset the vm points for this state
  vm_occ_points1 <- subset(vm_occ_points, place_state_name == state_name)

  # If number of rows is zero, skip
  if (nrow(vm_occ_points1) == 0) next

  # Determine overlaps
  pad_overlaps <- over(vm_occ_points1, state_pad_polygons1) %>%
    # Add private/public column if na
    mutate(land_type = ifelse(is.na(FeatClass), "Private/unknown", "Public")) %>%
    # subset relevant colums
    select(., land_type, location_owner = Loc_Own, location_type = d_Des_Tp, location_description = Loc_Ds, location_name = Unit_Nm,
           status = d_GAP_Sts)

  # Combine pad overlaps with the vm occurence data
  vm_occ_points2 <- bind_cols(as.data.frame(vm_occ_points1), pad_overlaps)

  # Save to a list
  vm_occ_points_ann[[i]] <- vm_occ_points2

}



# Bind rows of the vm_occ_points_ann list
vm_occ_points_ann_df <- bind_rows(vm_occ_points_ann)

# Save
write_csv(x = vm_occ_points_ann_df, file = "data/Vmacrocarpon_inaturalist_observations_reformatted_annotated.csv")

