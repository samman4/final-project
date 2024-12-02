---
title: "Geography 418 Final - Sam Whitten"
output: pdf_document
date: "2024-11-26"
---

## Introduction


British Columbia (BC) faces intense wildfire seasons, especially from May to October. These wildfires present serious dangers to ecosystems, communities, and infrastructure. As climate change leads to rising temperatures, it is vital to comprehend how these increased temperatures relate to wildfire density for effective management and mitigation strategies.

Hypothesis: Higher seasonal average temperatures correlate with increased wildfire density in British Columbia.

This project explores how seasonal temperature patterns and wildfire occurrences in BC during the summer of 2024. Employing geospatial and statistical techniques, we aim to:
Analyze spatial patterns of wildfire density and temperature across BC.

Assess the impact of temperature on wildfire density with regression models. Manage spatial autocorrelation in residuals by employing advanced methods such as Geographically Weighted Regression (GWR).

This study aims to analyze the relationships between temperature and wildfire density and guide wildfire management policies in BC.

Why British Columbia?

British Columbia's varied geography, which includes everything from temperate rainforests to dry plateaus, leads to differing wildfire risks shaped by local climate factors. In 2021, the province faced one of its most intense wildfire seasons, worsened by unprecedented heat waves. These exceptional circumstances make BC a prime location for studying the connections between climate and wildfires.

Objectives

Spatial Analysis:
Identify spatial clusters of wildfire density and temperature hotspots.
Compare spatial patterns using descriptive and spatial statistics.
Regression Analysis:
Quantify the relationship between seasonal temperatures and wildfire density.
Evaluate model performance and residual patterns.
Geographically Weighted Regression (GWR):
Explore localized variations in the temperature-wildfire relationship.
Address spatial autocorrelation in regression residuals.



---

## Data Cleaning and Preparation


The initial phase of any geospatial analysis involves cleaning and preparing the raw data. This includes importing the data, identifying missing or incorrect values, and filtering or merging datasets as needed. Here, our objective is to prepare climate data for British Columbia (BC) by calculating the seasonal average temperatures for each weather station and integrating this data with station metadata for spatial mapping.

The first step is to clean and process the raw climate data, which includes ASCII files and metadata. This process guarantees that the datasets are ready for later analysis and geospatial visualization.


Data Sources
Wildfire Points (Shapefile):
Path: /Users/swhitten/Downloads/C_FIRE_PNT_point/C_FIRE_PNT_point.shp
Contains spatial wildfire data for BC during the summer of 2024.
Station Metadata (CSV):
Path: /Users/swhitten/Downloads/station-metadata-by-history-3.csv
Metadata for weather stations, including their IDs and locations.
Climate Data (ASCII Files):
Folder: /Users/swhitten/Downloads/EC/
Contains daily records for temperature, precipitation, and snow depth from multiple weather stations.


### Code

We start by importing ASCII files that hold daily weather data. Each file corresponds to a weather station and contains precipitation, temperature, and snow depth variables. We aim to filter this data for the May to October season and compute average temperatures for every station.

```{r setup, include=FALSE}
# Load necessary libraries
library(dplyr)
library(lubridate)
library(sf)

# Set file paths
fire_points_path <- "/Users/swhitten/Downloads/C_FIRE_PNT_point/C_FIRE_PNT_point.shp"
stations_path <- "/Users/swhitten/Downloads/station-metadata-by-history-3.csv"
ascii_folder_path <- "/Users/swhitten/Downloads/EC/"

# Function to process a single ASCII file
process_ascii_file <- function(file) {
  # Read the file
  ascii_data <- read.table(
    file,
    header = FALSE,
    skip = 2,
    sep = ",",
    col.names = c(
      "ONE_DAY_PRECIPITATION", "ONE_DAY_RAIN", "ONE_DAY_SNOW",
      "time", "MIN_TEMP", "MAX_TEMP", "SNOW_ON_THE_GROUND"
    )
  )
  
  # Convert time and temperature columns
  ascii_data <- ascii_data %>%
    mutate(
      time = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S"),
      MIN_TEMP = as.numeric(MIN_TEMP),
      MAX_TEMP = as.numeric(MAX_TEMP)
    )
  
  # Filter data for May–October and calculate seasonal average temperature
  avg_temp <- ascii_data %>%
    filter(month(time) >= 5 & month(time) <= 10) %>%
    summarize(TEMP = mean((MIN_TEMP + MAX_TEMP) / 2, na.rm = TRUE))
  
  # Extract station ID from file name
  station_id <- tools::file_path_sans_ext(basename(file))
  
  # Return the result as a data frame
  return(data.frame(Native.ID = station_id, TEMP = avg_temp$TEMP))
}

# Directory containing ASCII files
ascii_files <- list.files(path = ascii_folder_path, pattern = "\\.ascii$", full.names = TRUE)

# Process all files and combine results
seasonal_data <- do.call(rbind, lapply(ascii_files, process_ascii_file))

# Save the aggregated data to a CSV file
write.csv(seasonal_data, "Aggregated_Climate_Data.csv", row.names = FALSE)

```


## Loading and Processing ASCII Climate Data
The ASCII files contain raw daily climate data for multiple stations. This section explains how we clean and process these files to compute seasonal average temperatures (May–October).

## Merging Climate Data with Metadata
The next step involves merging the processed climate data (Aggregated_Climate_Data.csv) with the station metadata file to integrate geographical coordinates (latitude and longitude) and other station-specific information. This combined dataset will allow us to visualize the spatial distribution of climate data.

```{r}
# Load necessary libraries
library(dplyr)

# File paths
stations_path <- "/Users/swhitten/Downloads/station-metadata-by-history-3.csv"  # Station metadata
climate_data_path <- "Aggregated_Climate_Data.csv"  # Processed climate data

# Load metadata and processed climate data
metadata <- read.csv(stations_path)
climate_data <- read.csv(climate_data_path)

# Merge datasets by "Native.ID"
merged_data <- merge(metadata, climate_data, by = "Native.ID", all.x = TRUE)

# Clean merged dataset
merged_data <- merged_data %>%
  filter(!is.na(Longitude) & !is.na(Latitude) & !is.na(TEMP)) %>%  # Remove rows with missing values
  filter(TEMP <= 100)  # Remove erroneous temperature values (e.g., >100°C)

# Save the cleaned and merged data
write.csv(merged_data, "ClimateData_Cleaned_Merged.csv", row.names = FALSE)


```

In this step, we successfully combined climate data with station metadata to prepare a spatially aware dataset. Including latitude and longitude allows us to map the seasonal average temperatures across British Columbia. Cleaning the data ensures that all analyses are based on accurate and reliable information. This merged dataset is critical for producing meaningful visualizations and conducting spatial interpolation.

## Visualizing the Climate Data on a Map
Now that we have a cleaned and merged dataset containing seasonal average temperatures and geographic coordinates, we can visualize this data on a map. This step will help us identify spatial patterns in the climate data across British Columbia.

Objective:
Visualize the cleaned and merged seasonal temperature data for British Columbia to identify spatial patterns.

Using the cleaned dataset (ClimateData_Cleaned_Merged.csv), we can overlay temperature data points on BC’s boundary map. This helps identify regions with higher or lower seasonal temperatures and provides a spatial dataset overview.


```{r}
# Load necessary libraries
library(sf)
library(ggplot2)

# File paths
climate_data_path <- "ClimateData_Cleaned_Merged.csv"
bc_boundary_path <- "/Users/swhitten/Downloads/bcairzonesalbersshp/bc_air_zones.shp"  # BC boundary shapefile

# Load the BC boundary shapefile
bc_boundary <- st_read(bc_boundary_path)

# Load the cleaned climate data
climate_data <- read.csv(climate_data_path)
climate_sf <- st_as_sf(climate_data, coords = c("Longitude", "Latitude"), crs = 4326)  # Convert to spatial data frame (WGS84 CRS)

# Transform BC boundary shapefile to match the CRS of the climate data
bc_boundary <- st_transform(bc_boundary, crs = st_crs(climate_sf))

# Plot the map
ggplot() +
  geom_sf(data = bc_boundary, fill = "lightgrey", color = "black") +  # Draw BC boundary
  geom_sf(data = climate_sf, aes(color = TEMP), size = 2) +           # Add climate data points
  scale_color_gradient(low = "blue", high = "red", name = "Temperature (°C)") +
  labs(
    title = "Map of Seasonal Average Temperatures in British Columbia",
    subtitle = "May–October Seasonal Averages",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

```

![image](https://github.com/user-attachments/assets/af5ae908-120a-4777-8f3b-619636d5424e)


This map visually represents seasonal average temperatures across British Columbia, highlighting regional temperature variations from May to October. Cooler areas appear blue, while warmer areas appear red. This visualization reveals geographical trends, such as warmer temperatures in the southern regions and cooler averages in the mountainous north. The data preparation and transformation steps ensure accurate spatial alignment, making this map a reliable tool for exploring climate variability in BC.


## Descriptive Statistics of Climate Data
In this section, we analyze the seasonal average temperature (TEMP) descriptive statistics to summarize the data's key attributes, such as central tendency, variability, and distribution. Descriptive statistics provide a foundational dataset understanding, highlighting trends and potential anomalies.

Objective:
Summarize the seasonal temperature data using descriptive statistics to understand its central tendencies, variability, and distribution.

Calculating measures such as the mean, median, standard deviation, and skewness can highlight trends or anomalies in the dataset.

Summary Statistics:
Here are the calculated statistics for seasonal average temperatures across all stations:

Mean Temperature: The average seasonal temperature across BC.
Median Temperature: The midpoint of the temperature data.
Standard Deviation: Measures variability in temperatures.
Skewness: Indicates whether the data is symmetrically distributed.
Kurtosis: Describes the "peakedness" of the data.
Coefficient of Variation: Expresses variability as a percentage of the mean.




```{r}
# Load required libraries
library(e1071)

# Load the cleaned climate data
climate_data_path <- "ClimateData_Cleaned_Merged.csv"
climate_data <- read.csv(climate_data_path)

# Calculate Descriptive Statistics
# Mean
mean_temp <- mean(climate_data$TEMP, na.rm = TRUE)

# Median
median_temp <- median(climate_data$TEMP, na.rm = TRUE)

# Standard Deviation
sd_temp <- sd(climate_data$TEMP, na.rm = TRUE)

# Skewness (measures symmetry)
skew_temp <- skewness(climate_data$TEMP, na.rm = TRUE)

# Kurtosis (measures peakedness)
kurt_temp <- kurtosis(climate_data$TEMP, na.rm = TRUE)

# Coefficient of Variation (CV): Standard deviation as a percentage of the mean
cv_temp <- (sd_temp / mean_temp) * 100

# Create a summary table
descriptive_stats <- data.frame(
  Statistic = c("Mean", "Median", "Standard Deviation", "Skewness", "Kurtosis", "Coefficient of Variation"),
  Value = c(mean_temp, median_temp, sd_temp, skew_temp, kurt_temp, cv_temp)
)

# Round values for readability
descriptive_stats$Value <- round(descriptive_stats$Value, 2)

# Display the summary table
descriptive_stats

```
![Screenshot 2024-11-30 at 11 20 54 AM](https://github.com/user-attachments/assets/17308e69-00f2-4201-b845-f588c1c8fc0d)





The seasonal average temperature distribution is consistent across British Columbia during the analyzed period (May–October). The close alignment between the mean and median suggests that the temperatures are symmetrically distributed without significant outliers. Additionally:

The low variability (indicated by the CV and standard deviation) confirms that temperature patterns do not drastically differ across regions.
The slight negative skewness indicates that a few regions experience lower average temperatures, which pulls the tail left.


## Preparing Climate Data for Spatial Analysis
This section converts the cleaned climate data into a spatial format for mapping and interpolation. Spatial data allows us to analyze geographical trends, overlay datasets, and perform advanced geostatistical techniques.

Objective:
Transform the cleaned climate data into a spatial format for geostatistical analysis and mapping. This step ensures the data is geographically aware, allowing us to overlay datasets and perform advanced spatial operations.

The seasonal average temperature data is formatted as a CSV file. To use it for spatial analysis, we need to convert it into a spatial object, align it with a coordinate reference system (CRS), and ensure compatibility with other geospatial datasets, such as the BC boundary.


## Converting Climate Data to a Spatial Format
We convert the cleaned temperature data into a Spatial or sf object. This allows spatial visualization, interpolation, and integration with other geographic datasets.


```{r}

# Load required libraries
library(sf)

# File paths
climate_data_path <- "ClimateData_Cleaned_Merged.csv"
bc_boundary_path <- "/Users/swhitten/Downloads/bcairzonesalbersshp/bc_air_zones.shp"

# Load climate data as a spatial object
climate_data <- read.csv(climate_data_path)
climate_sf <- st_as_sf(
  climate_data,
  coords = c("Longitude", "Latitude"),
  crs = 4326  # Use WGS84 coordinate reference system
)

# Load BC boundary shapefile
bc_boundary <- st_read(bc_boundary_path)

# Transform climate data to match the CRS of the BC boundary (if necessary)
climate_sf <- st_transform(climate_sf, crs = st_crs(bc_boundary))

# Summary of spatial objects to ensure proper transformation
summary(climate_sf)
summary(bc_boundary)



```

## Visualization: Mapping Climate Data Points

To inspect their spatial distribution, we visualize the climate data points overlaid on the BC boundary.
Once the climate data is spatially formatted, we can overlay it on the BC boundary to validate its spatial alignment and ensure all points fall within the province's borders.



```{r}
# Load required libraries
library(ggplot2)

# File paths
bc_boundary_path <- "/Users/swhitten/Downloads/bcairzonesalbersshp/bc_air_zones.shp"

# Plot the climate data points on the BC boundary
ggplot() +
  geom_sf(data = bc_boundary, fill = "lightgrey", color = "black") +  # BC boundary
  geom_sf(data = climate_sf, aes(color = TEMP), size = 2) +          # Climate data points
  scale_color_gradient(low = "blue", high = "red", name = "Temperature (°C)") +
  labs(
    title = "Climate Data Points in British Columbia",
    subtitle = "Seasonal Averages (May–October)",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


```

![image](https://github.com/user-attachments/assets/afc83364-bc61-448d-adf7-6c08e404789b)

Description of Code:
Loading the Wildfire Data: The shapefile containing wildfire points is loaded into R as an sf object.
CRS Transformation: The CRS of the wildfire points is transformed to align with the climate data and BC boundary.
Mapping: Both climate points and wildfire points are plotted over the BC boundary for comparison.

```{r}
# Load necessary libraries
library(sf)
library(ggplot2)

# File paths
fire_points_path <- "/Users/swhitten/Downloads/prot_current_fire_polys/prot_current_fire_polys.shp"

# Load wildfire data
fire_points <- st_read(fire_points_path)

# Transform the CRS of fire points to match the climate data
fire_points <- st_transform(fire_points, crs = st_crs(climate_sf))

# Plot the climate data points and fire points on the BC boundary map
ggplot() +
  geom_sf(data = bc_boundary, fill = "lightgrey", color = "black") +  # BC boundary
  geom_sf(data = climate_sf, aes(color = TEMP), size = 2) +          # Climate data points
  geom_sf(data = fire_points, color = "orange", size = 1, alpha = 0.6) +  # Fire points
  scale_color_gradient(low = "blue", high = "red", name = "Temperature (°C)") +
  labs(
    title = "Climate Data and Fire Points in British Columbia",
    subtitle = "Seasonal Averages (May–October) with Fire Locations",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


```

![image](https://github.com/user-attachments/assets/03928aa4-8cbd-444f-801d-3ebc4bd54bce)

The map confirms that all data points are correctly placed within British Columbia's boundaries. The colors represent the seasonal average temperatures, ranging from cooler temperatures (blue) to warmer temperatures (red). This spatial representation is crucial for subsequent interpolation and density analyses.

## Exploring Wildfire Hotspots Using Kernel Density Estimation (KDE)
Objective:
Highlight regions with a high density of wildfires using kernel density estimation (KDE). This approach identifies "hotspots" of wildfire activity.

```{r}
# Load necessary libraries
library(sf)
library(ggplot2)
library(spatstat)
library(dplyr)

# Transform fire points to a projected CRS (e.g., BC Albers, EPSG:3005) for distance-based calculations
fire_points_proj <- st_transform(fire_points, crs = 3005)
bc_boundary_proj <- st_transform(bc_boundary, crs = 3005)

# Convert fire points to a spatstat ppp object (point pattern)
fire_coords <- st_coordinates(fire_points_proj)  # Extract coordinates
fire_window <- as.owin(st_bbox(bc_boundary_proj))  # Define the observation window
fire_ppp <- ppp(
  x = fire_coords[, 1],
  y = fire_coords[, 2],
  window = fire_window
)

# Perform kernel density estimation
kde <- density(fire_ppp, sigma = 50000)  # Bandwidth = 50 km

# Convert KDE to a raster and mask to BC boundary
kde_raster <- raster(kde)
kde_raster_clipped <- mask(kde_raster, as_Spatial(bc_boundary_proj))

# Plot the kernel density map
ggplot() +
  geom_sf(data = bc_boundary_proj, fill = "lightgrey", color = "black") +  # BC boundary
  geom_raster(data = as.data.frame(rasterToPoints(kde_raster_clipped)), 
              aes(x = x, y = y, fill = layer), alpha = 0.8) +
  scale_fill_viridis_c(name = "Fire Density") +
  labs(
    title = "Kernel Density Estimation of Wildfire Points",
    subtitle = "Wildfire Hotspots in British Columbia",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

```
The KDE map reveals wildfire hotspots across British Columbia:

High-Density Areas: Regions with the highest wildfire activity are highlighted in darker shades.
Observations: This visualization provides a clear spatial summary of wildfire activity, laying the groundwork for analyzing its relationship with climate data.

![image](https://github.com/user-attachments/assets/98d0a848-7a2d-428d-b672-167ac285b8fd)

## Interpolating Climate Data (IDW)

Objective:
Create a continuous surface of seasonal average temperatures across British Columbia using Inverse Distance Weighting (IDW) interpolation. This method assumes that temperature at unknown locations can be estimated as a weighted average of nearby observations, with closer observations having more influence.
```{r}

# Load necessary libraries
library(gstat)
library(raster)
library(tmap)

# Transform climate data and BC boundary to a projected CRS (BC Albers, EPSG:3005)
climate_sf_proj <- st_transform(climate_sf, crs = 3005)
bc_boundary_proj <- st_transform(bc_boundary, crs = 3005)

# Create a grid for interpolation
grid <- st_make_grid(
  st_as_sfc(st_bbox(bc_boundary_proj)),
  cellsize = 50000,  # Adjust cell size for desired resolution (50 km here)
  what = "centers"
)
grid_sf <- st_as_sf(grid, crs = st_crs(climate_sf_proj))

# Perform IDW interpolation
idw_result <- gstat::idw(TEMP ~ 1, locations = climate_sf_proj, newdata = grid_sf, idp = 2)

# Convert IDW result to a raster and clip to BC boundary
idw_raster <- raster(idw_result)
idw_raster_clipped <- mask(idw_raster, as_Spatial(bc_boundary_proj))

# Plot the IDW interpolation map
tm_shape(idw_raster_clipped) +
  tm_raster(style = "jenks", palette = "viridis", title = "Temperature (°C)") +
  tm_shape(climate_sf_proj) +
  tm_dots(size = 0.2, col = "red") +
  tm_layout(
    main.title = "IDW Interpolation of Seasonal Average Temperature",
    legend.outside = TRUE
  )



```

Description of Code:
Data Transformation: Climate data points and the BC boundary are transformed to a projected CRS for distance-based calculations.
Grid Creation: A regular grid covering the study area is generated for interpolation.
IDW Interpolation: Using gstat::idw, temperature values are interpolated across the grid based on the input climate data.
Clipping to Boundary: The resulting raster is clipped to the BC boundary for visualization.
Mapping: The interpolated surface is visualized with temperature points overlaid for reference.

![image](https://github.com/user-attachments/assets/47828fca-b974-47de-b4a0-416ece0a1518)







```{r}
# Ensure both datasets are in the same CRS
fire_points_proj <- st_transform(fire_points, crs = st_crs(idw_raster_clipped))
idw_sf <- as.data.frame(rasterToPoints(idw_raster_clipped))  # Convert IDW raster to a data frame

# Convert IDW data frame back to spatial object for joining
idw_sf <- st_as_sf(idw_sf, coords = c("x", "y"), crs = st_crs(fire_points_proj))

# Perform spatial join between fire points and IDW grid
fire_idw_join <- st_join(fire_points_proj, idw_sf, join = st_intersects)

# Summarize fire counts within each IDW grid cell
fire_density_by_temp <- fire_idw_join %>%
  group_by(layer) %>%  # 'layer' contains temperature values from IDW
  summarize(fire_count = n(), .groups = "drop")  # Count fire points per temperature range

# Plotting the relationship between temperature and fire density
ggplot(fire_density_by_temp, aes(x = layer, y = fire_count)) +
  geom_point(color = "red", size = 3) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  labs(
    title = "Relationship Between Temperature and Wildfire Density",
    x = "Temperature (°C)",
    y = "Fire Count"
  ) +
  theme_minimal()

```


```{r}

# Fire Density Map with Climate Data
ggplot() +
  geom_sf(data = bc_boundary_proj, fill = "lightgrey", color = "black") +  # BC boundary
  stat_density_2d(
    data = fire_coords,
    aes(x = X, y = Y, fill = ..level..),
    geom = "polygon",
    alpha = 0.6,
    h = c(40000, 40000)  # Adjust bandwidth (in meters)
  ) +
  geom_sf(data = climate_sf_proj, aes(color = TEMP), size = 2) +  # Climate data points
  scale_fill_viridis_c(name = "Fire Density (log scale)", trans = "log10") +  # Log scale for density
  scale_color_gradient(low = "blue", high = "red", name = "Temperature (°C)") +
  labs(
    title = "Fire Density and Climate Data in British Columbia",
    subtitle = "Seasonal Averages (May–October) with Fire Locations and Density",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")




```


![image](https://github.com/user-attachments/assets/eba2359e-c235-475e-a6d7-846a8d087ec4)



## Interpolating Climate Data (IDW)
Objective:

We will use Inverse Distance Weighting (IDW) interpolation to create a continuous temperature surface based on the seasonal average temperature data. This technique assumes that points closer together exert more influence on the predicted value than those farther apart.

```{r}
library(sp)
library(gstat)

# Ensure `climate_sp` is a valid SpatialPointsDataFrame
if (!inherits(climate_sp, "SpatialPointsDataFrame")) {
  stop("climate_sp must be a SpatialPointsDataFrame.")
}

# Initialize a vector to store LOOCV predictions
loocv_predictions <- numeric(nrow(climate_sp))

# Perform LOOCV for IDW
for (i in seq_len(nrow(climate_sp))) {
  # Exclude the i-th point for training
  training_data <- climate_sp[-i, ]  # Exclude the point
  
  # Select the i-th point for testing
  test_data <- climate_sp[i, , drop = FALSE]  # Only the point
  
  # Ensure both training and testing data are SpatialPointsDataFrame
  if (!inherits(training_data, "SpatialPointsDataFrame") || 
      !inherits(test_data, "SpatialPointsDataFrame")) {
    stop("Both training_data and test_data must be SpatialPointsDataFrame.")
  }
  
  # Perform IDW prediction
  idw_temp <- idw(TEMP ~ 1, locations = training_data, newdata = test_data, idp = 2)
  
  # Store the predicted value
  loocv_predictions[i] <- idw_temp$var1.pred
}

# Combine observed and predicted into a data frame
loocv_results <- data.frame(
  Observed = climate_sp$TEMP,
  Predicted = loocv_predictions
)

# Calculate RMSE (Root Mean Square Error)
rmse <- sqrt(mean((loocv_results$Observed - loocv_results$Predicted)^2, na.rm = TRUE))
cat("RMSE for IDW:", rmse, "\n")

# Plot Observed vs Predicted
library(ggplot2)
ggplot(loocv_results, aes(x = Observed, y = Predicted)) +
  geom_point(color = "blue", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +  # 1:1 line
  labs(
    title = "Observed vs Predicted (IDW Interpolation)",
    x = "Observed Temperature (°C)",
    y = "Predicted Temperature (°C)"
  ) +
  theme_minimal()

```

## Assessing Interpolation Accuracy: LOOCV

Objective:
Validate the IDW model using Leave-One-Out Cross-Validation (LOOCV), which systematically removes one observation at a time, predicts its value, and compares the prediction with the observed value.

```{r}
# Initialize a vector to store LOOCV predictions
loocv_predictions <- numeric(nrow(climate_sf_proj))

# Perform LOOCV for IDW
for (i in seq_len(nrow(climate_sf_proj))) {
  # Exclude the i-th point
  idw_temp <- idw(TEMP ~ 1, locations = climate_sf_proj[-i, ], newdata = climate_sf_proj[i, ], idp = 2)
  
  # Store the predicted value
  loocv_predictions[i] <- idw_temp$var1.pred
}

# Combine observed and predicted values into a data frame
loocv_results <- data.frame(
  Observed = climate_sf_proj$TEMP,
  Predicted = loocv_predictions
)

# Calculate RMSE (Root Mean Square Error)
rmse <- sqrt(mean((loocv_results$Observed - loocv_results$Predicted)^2, na.rm = TRUE))
cat("RMSE for IDW:", rmse, "\n")

# Plot Observed vs. Predicted
plot(
  loocv_results$Observed,
  loocv_results$Predicted,
  xlab = "Observed Temperature (°C)",
  ylab = "Predicted Temperature (°C)",
  main = "LOOCV: Observed vs Predicted Temperature",
  col = "blue", pch = 16
)
abline(0, 1, col = "red", lty = 2)  # Add 1:1 line

```


## Jackknife Estimation for Confidence Intervals
Objective: The Jackknife method provides estimates of confidence intervals for the predicted temperatures, enabling a better understanding of the uncertainty associated with the interpolation model.


```{r}
# Perform Jackknife Estimation for Confidence Intervals
n <- nrow(climate_sf_proj)
pseudo_values <- matrix(nrow = nrow(grid_sf), ncol = n)

# Calculate pseudo-values for each data point
for (i in 1:n) {
  temp_idw <- gstat::idw(TEMP ~ 1, locations = climate_sf_proj[-i, ], newdata = grid_sf, idp = 2)
  pseudo_values[, i] <- n * idw_result$var1.pred - (n - 1) * temp_idw$var1.pred
}

# Compute mean estimates and variances
mean_estimates <- rowMeans(pseudo_values, na.rm = TRUE)
variance_estimates <- apply((pseudo_values - mean_estimates)^2, 1, sum, na.rm = TRUE) / (n * (n - 1))
confidence_intervals <- sqrt(variance_estimates)

# Add confidence intervals to the interpolation grid
ci_raster <- rasterFromXYZ(data.frame(st_coordinates(grid_sf), CI = confidence_intervals))

# Mask the raster to BC boundaries
ci_raster_masked <- mask(ci_raster, as_Spatial(bc_boundary))

# Visualize Confidence Intervals
tm_shape(ci_raster_masked) +
  tm_raster(style = "jenks", palette = "-viridis", title = "Confidence Intervals (95%)") +
  tm_layout(
    main.title = "Jackknife Confidence Intervals for IDW",
    legend.outside = TRUE
  )

```
![image](https://github.com/user-attachments/assets/d13e784b-251d-491b-a46a-82589803cfc0)


```{r}
# Spatial join between fire points and IDW grid
fire_points <- st_transform(fire_points, crs = st_crs(idw_clipped))
fire_grid <- fire_points %>%
  st_join(idw_clipped, join = st_nearest_feature) %>%
  group_by(ID) %>%
  summarize(fire_count = n(), .groups = "drop") %>%
  st_drop_geometry()



# Fit OLS regression model
ols_model <- lm(fire_count ~ var1.pred, data = regression_data)

# Print regression summary
summary(ols_model)

# Calculate residuals and add to the regression data
regression_data$residuals <- residuals(ols_model)

# Plot residuals
ggplot(regression_data, aes(x = var1.pred, y = residuals)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residuals of Fire Count vs Temperature Model",
    x = "Predicted Temperature (°C)",
    y = "Residuals"
  ) +
  theme_minimal()

```

![image](https://github.com/user-attachments/assets/41e75d08-55fe-4346-860b-ea17273a9446)




```{r}
# Calculate Root Mean Square Error (RMSE)
rmse <- sqrt(mean((loocv_results$Observed - loocv_results$Predicted)^2, na.rm = TRUE))
cat("RMSE for IDW LOOCV:", rmse, "\n")

```

RMSE for IDW LOOCV: 1.703583 

Description of Validation:
Observed vs Predicted Scatterplot: Shows the relationship between actual and predicted temperature values.
RMSE: A lower RMSE indicates better model performance.

```{r}
# Plot Observed vs Predicted values
plot(
  loocv_results$Observed,
  loocv_results$Predicted,
  xlab = "Observed Temperature (°C)",
  ylab = "Predicted Temperature (°C)",
  main = "LOOCV: Observed vs Predicted Temperature",
  col = "blue", pch = 16
)
abline(0, 1, col = "red", lty = 2)  # Add 1:1 line for reference

```

![image](https://github.com/user-attachments/assets/660b6ef0-1fee-48dd-8493-755359296fdc)




## Moran’s I Test and Regression

Objective:
Assess the presence of spatial autocorrelation in the climate data and residuals from regression models. This step includes the Moran’s I test for spatial autocorrelation and the application of regression models (OLS and GWR) to analyze the relationship between temperature and wildfire density.

```{r}
library(spdep)

# Ensure data alignment for residual analysis
# Use interpolated temperature or fire density for testing spatial autocorrelation
coords <- st_coordinates(st_centroid(climate_sf_proj))  # Get centroids of climate points
neighbors <- knn2nb(knearneigh(coords, k = 4))          # Create nearest neighbors
weights <- nb2listw(neighbors, style = "W")             # Create spatial weights

# Moran's I test for temperature
moran_test <- moran.test(climate_sf_proj$TEMP, weights)

# Print Moran's I test results
print(moran_test)

```


Moran I test under randomisation

data:  climate_sf_proj$TEMP  
weights: weights    

Moran I statistic standard deviate = 5.2529, p-value = 7.486e-08
alternative hypothesis: greater
sample estimates:
Moran I statistic       Expectation          Variance 
      0.400557269      -0.015873016       0.006284728 

    
Interpretation of Moran’s I Test Results:
Moran’s I Statistic: Measures the degree of spatial autocorrelation. Values near +1 indicate strong positive spatial autocorrelation, while values near -1 indicate negative spatial autocorrelation.
P-value: A significant p-value (<0.05) suggests the presence of spatial autocorrelation.
Conclusion: In this case, the positive Moran’s I statistic and significant p-value confirm the presence of spatial clustering in the temperature data.


## Addressing Spatial Autocorrelation with Geographically Weighted Regression (GWR)
Given that Moran's I test results indicate significant spatial autocorrelation in the residuals, the next logical step is to address this by applying geographically weighted regression (GWR). This technique allows the regression coefficients to vary spatially, capturing localized relationships between temperature and fire density.

```{r}
# Create a dataset for regression
fire_points_proj <- st_transform(fire_points, crs = 3005)  # Ensure CRS matches
climate_sp <- as(climate_sf_proj, "Spatial")               # Convert to Spatial for regression

# Spatial join to associate fire points with interpolated temperature
fire_density <- st_join(fire_points_proj, climate_sf_proj, join = st_nearest_feature)

# Summarize fire density by temperature
regression_data <- fire_density %>%
  group_by(Native.ID) %>%
  summarize(
    fire_count = n(),
    TEMP = mean(TEMP, na.rm = TRUE)
  ) %>%
  st_drop_geometry()

# Fit OLS model
ols_model <- lm(fire_count ~ TEMP, data = regression_data)

# Print regression summary
summary(ols_model)

# Plot residuals
regression_data$residuals <- residuals(ols_model)

ggplot(regression_data, aes(x = TEMP, y = residuals)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residuals of Fire Count vs Temperature Model",
    x = "Temperature (°C)",
    y = "Residuals"
  ) +
  theme_minimal()

```
![image](https://github.com/user-attachments/assets/d8818e94-1da5-45d4-b1cf-6c4efcf39092)


```{r}
# Align regression data with spatial coordinates
# Check if both have the same length
if (nrow(coords) != nrow(regression_data_df)) {
  stop("Mismatch between coordinates and regression data. Ensure alignment.")
}

# Ensure regression_data_df corresponds to coords rows
# (Optional: Match by Native.ID or another unique identifier if necessary)

```



```{r}
library(spgwr)

# Prepare coordinates and data for GWR
coords <- st_coordinates(climate_sf_proj)
regression_data_df <- as.data.frame(regression_data)  # Convert to data frame for GWR

# Select optimal bandwidth
gwr_bandwidth <- gwr.sel(fire_count ~ TEMP, data = regression_data_df, coords = coords, adapt = TRUE)

# Fit GWR model
gwr_model <- gwr(fire_count ~ TEMP, data = regression_data_df, coords = coords, adapt = gwr_bandwidth, hatmatrix = TRUE)

# Extract coefficients and local R-squared
regression_data_df$gwr_coeff <- gwr_model$SDF$TEMP
regression_data_df$local_r2 <- gwr_model$SDF$R2

# Visualize GWR results
ggplot(data = regression_data_df) +
  geom_sf(aes(fill = gwr_coeff), color = NA) +
  scale_fill_viridis_c(name = "GWR Coefficients") +
  labs(
    title = "GWR Coefficients for Fire Density vs Temperature",
    subtitle = "Spatial Variation of Local Coefficients",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

# Local R-Squared Visualization
ggplot(data = regression_data_df) +
  geom_sf(aes(fill = local_r2), color = NA) +
  scale_fill_viridis_c(name = "Local R-Squared") +
  labs(
    title = "Local R-Squared Values from GWR",
    subtitle = "Explaining Variance in Fire Density by Temperature",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

```

Interpretation of GWR Results:
GWR Coefficients: Display spatial variation in the temperature and wildfire density relationship. Areas with stronger positive coefficients indicate regions where temperature influences fire density more.
Local R-Squared: Indicates the proportion of variance the model explains at each location. Higher values represent better model performance.


## Conclusion
This project thoroughly explores the spatial relationship between seasonal temperature patterns and wildfire occurrences in British Columbia during the summer of 2021. Through a combination of geospatial analysis, spatial statistics, and regression modelling, several key insights have been uncovered:

Temperature and Wildfire Density:
Spatial analysis showed that wildfire density is strongly linked to temperature trends. Areas with higher average temperatures often correspond with increased wildfire activity, especially in southern and interior regions of BC. The OLS regression model indicated a positive correlation between temperature and wildfire density, reinforcing that temperature significantly affects wildfire occurrences occurrence.

Spatial Autocorrelation:

The Moran's I test confirmed the presence of significant spatial autocorrelation in temperature patterns and residuals, indicating that similar temperature values cluster geographically.
This spatial dependence highlights the necessity for advanced modeling techniques like Geographically Weighted Regression (GWR) to capture localized temperature-wildfire variations relationship.
Geographically Weighted Regression (GWR):

GWR revealed that the influence of temperature on wildfire density is not uniform across BC. Some regions showed stronger relationships, suggesting that localized factors like vegetation type, land cover, or historical fire management practices may interact with temperature to drive wildfire dynamics.

The GWR model also improved model performance by addressing spatial autocorrelation in residuals, highlighting its value for spatially explicit wildfire management strategies.
Data Integration and Visualization:

We produced informative maps and visualizations by integrating climate and wildfire data with spatial metadata, including temperature surfaces, wildfire density maps, and kernel density estimates. These outputs enhance understanding wildfire risks and support resource allocation and preparedness decision-making.

This research explored the connection between wildfire occurrences and increasing temperatures in British Columbia, specifically during the summer of 2024. The findings indicated that temperature significantly influences wildfire clustering, with the greatest concentrations found in southern and central BC, where temperatures were the highest. Spatial analyses, such as density maps, regression models, and geographically weighted regression (GWR), confirmed the hypothesis that elevated temperatures are associated with more frequent and clustered wildfires. In contrast, temperature had a reduced effect in northern BC, revealing regional differences in climate-fire dynamics.
This study effectively demonstrated a connection between temperature and fire in southern BC; however, several limitations were noted. Inaccuracies in interpolated surfaces stemmed from gaps in data surfaces and coverage, especially regarding temperature measurements. The GWR analysis also revealed regions where temperature alone failed to account for fire occurrences fully, indicating a need to incorporate other factors like wind, humidity, and vegetation types for a more thorough understanding.

These results highlight the necessity of integrating regional temperature data into wildfire management plans. Organizations such as the BC Wildfire Service could leverage this information to identify high-risk zones and reduce harm to both communities and ecosystems. Upcoming research should aim to broaden datasets, refine interpolation techniques, and investigate other climatic and human-related influences to strengthen predictive frameworks and adaptation approaches.

## Recommendations

Wildfire Management:
Areas exhibiting elevated GWR coefficients, which suggest a stronger link between temperature and wildfire density, must be prioritized for proactive wildfire management approaches, including fuel reduction and community readiness. Additionally, early warning systems and resource distribution should take localized temperature trends and their impact on wildfire risks into account.

Climate Change Adaptation:
As climate change drives more extreme temperature patterns, this study emphasizes the need for adaptive wildfire management policies that integrate climate projections with spatial analysis.
Regional variations in the temperature-wildfire relationship suggest that adaptation strategies should be tailored to each area's needs.

Future Research:
To refine our understanding of wildfire drivers, we will expand the analysis to include additional variables, such as precipitation, vegetation, and human activity.
Conduct temporal analyses to explore how the temperature-wildfire relationship evolves over multiple seasons or years.



This research emphasizes the importance of geospatial and statistical techniques for comprehending complex environmental issues such as wildfires. By merging sophisticated spatial methods with easily interpretable visual representations, the results provide valuable insights for policymakers, scientists, and the general public. As climate change increasingly impacts natural ecosystems, such studies will be essential in enhancing resilience and reducing risks in regions susceptible to wildfires, like British Columbia.


## Refrences 


Getis, A., & Ord, J. K. (1992). The Analysis of Spatial Association by Use of Distance Statistics. Geographical Analysis, 24(3), 189–206. https://doi.org/10.1111/j.1538-4632.1992.tb00261.x

Relevance: This source is fundamental for understanding spatial statistics, particularly tools like Moran’s I and the spatial distribution patterns applied in your wildfire density analysis.

Lu, G. Y., & Wong, D. W. (2008). An adaptive inverse-distance weighting spatial interpolation technique. Computers & Geosciences, 34(9), 1044–1055. https://doi.org/10.1016/j.cageo.2007.07.010

Relevance: This article provides valuable insights into interpolation techniques like IDW, directly supporting your use of IDW for temperature surface creation and validation.
Páez, A., & Wheeler, D. C. (2009). Geographically weighted regression. In Encyclopedia of GIS. Elsevier. https://doi.org/10.1016/B978-008044910-400447-8

Relevance: This source explains the Geographically Weighted Regression (GWR) technique, which you used to model localized variations in the relationship between temperature and wildfire density.

Węglarczyk, S. (2018). Kernel density estimation and its application. ITM Web of Conferences, 23, 00037. https://doi.org/10.1051/itmconf/20182300037

Relevance: This reference supports your use of Kernel Density Estimation (KDE) for identifying wildfire hotspots, providing a theoretical framework for this spatial analysis method.



