---
title: "Geography 418 Final - Sam Whitten"
output: pdf_document
date: "2024-11-26"
---

## Introduction


British Columbia (BC) experiences frequent and intense wildfire seasons, particularly during the warmer months from May to October. These wildfires pose significant risks to ecosystems, communities, and infrastructure. With climate change driving increased temperatures, understanding the relationship between rising temperatures and wildfire density has become crucial for effective wildfire management and mitigation strategies.

This project investigates the spatial relationship between seasonal temperature patterns and wildfire occurrences across BC during the summer of 2021. Using geospatial and statistical methods, we will:

Analyze spatial patterns of wildfire density and temperature across BC.
Evaluate the influence of temperature on wildfire density using regression models.
Address spatial autocorrelation in residuals through advanced techniques like Geographically Weighted Regression (GWR).
By examining these relationships, this study seeks to provide insights into how temperature influences wildfire density and inform wildfire management policies in BC.


Why British Columbia?

BC’s diverse geography, ranging from temperate rainforests to arid plateaus, creates varying wildfire risks influenced by local climate conditions. In 2021, the province experienced one of its most severe wildfire seasons, exacerbated by record-breaking heat waves. These unique conditions make BC an ideal study area for exploring climate-wildfire relationships.


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


The first step in any geospatial analysis is to ensure the raw data is cleaned and prepared for analysis. This process includes importing data, checking for missing or erroneous values, and filtering or merging datasets as necessary. In this case, we aim to prepare climate data for British Columbia (BC) by calculating seasonal average temperatures for each weather station and combining this data with station metadata for spatial mapping.

The initial step involves cleaning and processing the raw climate data, including ASCII files and metadata. This ensures that the datasets are compatible for subsequent analysis and geospatial visualization.

### Code

We begin by importing ASCII files containing daily weather data. Each file represents a weather station, and the data includes variables such as precipitation, temperature, and snow depth. The goal is to filter the data for the May–October season and calculate average temperatures for each station.


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

In this step, we successfully combined climate data with station metadata to prepare a spatially aware dataset. The inclusion of latitude and longitude allows us to map the seasonal average temperatures across British Columbia. Cleaning the data ensures that all analyses are based on accurate and reliable information. This merged dataset is critical for producing meaningful visualizations and conducting spatial interpolation.

## Visualizing the Climate Data on a Map
Now that we have a cleaned and merged dataset containing seasonal average temperatures and geographic coordinates, we can visualize this data on a map. This step will help us identify spatial patterns in the climate data across British Columbia.


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


The seasonal average temperature distribution is consistent across British Columbia during the analyzed period (May–October). The close alignment between mean and median suggests that the temperatures are symmetrically distributed without significant outliers. Additionally:

The low variability (as indicated by the CV and standard deviation) confirms that temperature patterns do not differ drastically across regions.
The slight negative skewness might indicate that a few regions experience slightly lower average temperatures, pulling the tail slightly left.


## Preparing Climate Data for Spatial Analysis
In this section, we convert the cleaned climate data into a spatial format for mapping and interpolation. Spatial data allows us to analyze geographical trends, overlay datasets, and perform advanced geostatistical techniques.


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

We visualize the climate data points overlaid on the BC boundary to inspect their spatial distribution.


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





## Interpolating Climate Data (IDW)
Objective:

We will use Inverse Distance Weighting (IDW) interpolation to create a continuous temperature surface based on the seasonal average temperature data. This technique assumes that points closer to each other have more influence on the predicted value than those farther apart.


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





```{r}
# Calculate Root Mean Square Error (RMSE)
rmse <- sqrt(mean((loocv_results$Observed - loocv_results$Predicted)^2, na.rm = TRUE))
cat("RMSE for IDW LOOCV:", rmse, "\n")

```

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




```{r}
# Ensure grid_sf is properly formatted as SpatialPointsDataFrame
grid_sp <- as(grid_sf, "Spatial")

```



```{r}
# Initialize storage for pseudo-values
n <- nrow(climate_sp)
pseudo_values <- matrix(nrow = nrow(grid_sp), ncol = n)

for (i in 1:n) {
  # Remove the i-th point and predict using remaining data
  training_data <- climate_sp[-i, ]
  temp_idw <- idw(TEMP ~ 1, locations = training_data, newdata = grid_sp, idp = 2)
  
  # Store pseudo-values
  pseudo_values[, i] <- n * idw_result$var1.pred - (n - 1) * temp_idw$var1.pred
}

# Compute confidence intervals
mean_estimates <- rowMeans(pseudo_values, na.rm = TRUE)
variance_estimates <- apply((pseudo_values - mean_estimates)^2, 1, sum, na.rm = TRUE) / (n * (n - 1))
confidence_intervals <- sqrt(variance_estimates)

```



```{r}
# Create a raster from confidence intervals
ci_raster <- rasterFromXYZ(data.frame(st_coordinates(grid_sf), CI = confidence_intervals))

# Clip raster to BC boundaries
ci_raster_clipped <- mask(ci_raster, as_Spatial(bc_boundary_proj))

# Visualize Confidence Intervals
tm_shape(ci_raster_clipped) +
  tm_raster(style = "jenks", palette = "-viridis", title = "Confidence Intervals (95%)") +
  tm_layout(
    main.title = "Jackknife Confidence Intervals for IDW",
    legend.outside = TRUE
  )

```




```{r}
# Transform all spatial objects to the same CRS (BC Albers)
fire_points_proj <- st_transform(fire_points, crs = 3005)  # Fire points
bc_boundary_proj <- st_transform(bc_boundary, crs = 3005)  # BC boundary
climate_sf_proj <- st_transform(climate_sf, crs = 3005)    # Climate data

# Create a 2D density plot for fire locations
fire_coords <- as.data.frame(st_coordinates(fire_points_proj))

ggplot() +
  geom_sf(data = bc_boundary_proj, fill = "lightgrey", color = "black") +  # BC boundary
  stat_density_2d(
    data = fire_coords,
    aes(x = X, y = Y, fill = ..level..),
    geom = "polygon",
    alpha = 0.6,
    h = c(50000, 50000)  # Bandwidth for density
  ) +
  geom_sf(data = climate_sf_proj, aes(color = TEMP), size = 2) +  # Climate data points
  scale_fill_viridis_c(name = "Fire Density (log scale)", trans = "log10") +  # Fire density
  scale_color_gradient(low = "blue", high = "red", name = "Temperature (°C)") +  # Temperature
  labs(
    title = "Fire Density and Climate Data in British Columbia",
    subtitle = "Spatial Distribution of Fires with Seasonal Temperature Data",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

```




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




## Addressing Spatial Autocorrelation with Geographically Weighted Regression (GWR)
Given the Moran's I test results indicating significant spatial autocorrelation in the residuals, the next logical step is to address this by applying Geographically Weighted Regression (GWR). This technique allows the regression coefficients to vary spatially, capturing localized relationships between temperature and fire density.


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




