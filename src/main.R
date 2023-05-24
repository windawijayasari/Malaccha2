# This program objective is to extract markov transition matrices from MODIS 
# database to view the changes of LULC over the years. The input required are
# MODIS land cover database and boundary area in shapefile format (.shp).

# Clean the environment
rm(list = ls())

# Load the required libraries
library(terra)
library(tidyverse)
library(MASS)
library(ggalluvial)
library(rgdal)
library(dplyr)


# Define paths and inputs
modis_database_path = "../data/MCD12Q1/"
dataset_selected = "LC_Type1"
bounding_shapefile_path = "../data/Citarum hulu/Citarum_Hulu_Curug_Jompong.shp"
bounding_shapefile = readOGR(bounding_shapefile_path)
reclass_table_path = "../data/reclassification_table.csv"
reclass_order_path = "../data/classification_factor_order.csv"
jcr_database_path = "../data/JCR_CitarumHulu/"

# Load source scripts
source("./Load_reproject_crop_MODIS.R")
source("./Extract_reclassify_rasters.R")
source("./Get_matrix_by_year.R")
source("./Calculate_transition_matrices.R")
source("./Calculate_average_transition_matrix.R")
source("./Load_jcr_raster.R")

# Resample Modis and Jcr data to 500m resolution
source("./refining_coarsing.R")
source("./LC Performance.R")

#Make directory for results
dir.create("../Results_example")
dir.create("../Results_example/Files")

# Load reproject and crop MODIS
modis_cropped_rasters = 
  Load_reproject_crop_MODIS(modis_database_path,
                            dataset_selected,
                            bounding_shapefile_path)
source("./refining_coarsing.R")

#Load JCR data and resample to resolution 500m
jcr_cropped_500 = 
  Load_jcr_raster(jcr_database_path,
                  bounding_shapefile_path)

#Get jcr table
source("./Extract_jcr_raster.R")
all_jcr_table = 
  Extract_jcr_rasters(jcr_cropped_500)

# Create the histogram.
hist(all_jcr_table$value, main="Surface Water Value ",
     col = "blue", border = "black")
abline(v = mean(all_jcr_table$value), col='red', lwd = 3)


ggplot(all_jcr_table, aes(Year, value, group = Year)) +geom_boxplot()

# Extract and reclassify rasters into one reclassified table
reclassified_table = 
  Extract_reclassify_rasters(modis_cropped_500,
                             reclass_table_path,
                             reclass_order_path) %>%
  drop_na()

source("./Plot_map.R")
source("./Plot_LC_bar.R")
#Plot map after reclassification
plot_reclassified =
  plot_reclassified_table(reclassified_table)


#plot bar
bar_plot = LCC_bar_plot(reclassified_table)

# Get list of years
years = reclassified_table$Year %>% unique() %>% sort()

# Get all LC matrices
all_lc_matrices = 
  lapply(years, 
         function(year){
           Get_matrix_by_year(reclassified_table, year)})

# Calculate all transition matrices
all_transition_matrices = 
  mapply(
    function(a, b){
      Calculate_transition_matrix(reclassified_table, a, b)},
    years[1:(length(years)-1)], 
    years[2:length(years)], 
    SIMPLIFY = FALSE)

# Calculate average transition matrix
average_transition_matrix = 
  Calculate_average_transition_matrix(all_transition_matrices)

# Countable every year
countTable = reclassified_table %>%
  group_by(Year, LC_class) %>%
  summarise(Count = n()) %>%
  print()
write.csv(countTable, "../results/countable_every year.csv", row.names = FALSE)

#plots the count
ggplot(countTable) + 
  geom_area(aes(Year, Count, group = LC_class, fill = LC_class)) +
  scale_fill_manual(values = c("#009e73", "#f0e442", "#d55e00")) + 
  theme(legend.position="bottom")

#pixel over time
source("./Plot_LC_pixel_over_time.R")
Pixel_over_time(reclassified_table)


#performance check
proj_2001_2020 = project_LC_change(all_lc_matrices[[1]], average_transition_matrix, 19, all_lc_matrices[[20]])
proj_2001_2020$performance


# Get list of years
years = reclassified_table$Year %>% unique() %>% sort()
#Get water transition
source("./winda.R")
all_transition_matrices_water = 
  mapply(
    function(a, b){
      Calculate_transition_matrix_water(reclassified_table,all_jcr_table, a, b)},
    years[1:(length(years)-1)], 
    years[2:length(years)], 
    SIMPLIFY = FALSE)

#Get average of matrix with water
source("./winda2.R")
selected_years = 2001:2019
average_water_matrices = 
  water_transition_matrices2(reclassified_table,all_jcr_table,selected_years)




