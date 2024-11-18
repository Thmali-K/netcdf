# netcdf
library(ncdf4)

# Specify the path to the NetCDF file
file <- "path_to_your_netcdf_file.nc"

# Open the NetCDF file
nc_file <- nc_open(file)

# Print file details
print(nc_file)

# Get the variable data from the file
data <- ncvar_get(nc_file, "variable_name")

# Dimensions of the data
dim(data)

# Get latitude, longitude, and time variables
latitudes <- ncvar_get(nc_file, "latitude")
longitudes <- ncvar_get(nc_file, "longitude")
time <- ncvar_get(nc_file, "time")

# Calculate the mean for each time step (over all latitudes and longitudes)
mean_data <- apply(data, 3, mean, na.rm = TRUE)

# Create a data frame with the years and corresponding data
years <- seq(start_year, end_year, by = 1)  # Replace start_year and end_year with actual years
data_df <- data.frame(Year = years, Data = mean_data)

# Remove specific years if needed (e.g., excluding first and last year)
data_filtered_df <- data_df[!data_df$Year %in% c(start_year, end_year), ]

# Calculate the 10th percentile
percentile_10 <- quantile(data_filtered_df$Data, 0.10)
print(percentile_10)

# Find years with data below the 10th percentile
driest_years <- data_filtered_df[data_filtered_df$Data <= percentile_10, ]
print(driest_years)

# Load necessary library for plotting
library(ggplot2)

# Histogram plot
ggplot(data_filtered_df, aes(x = Data)) +
  geom_histogram(binwidth = 50, fill = "slategrey", color = "black") +  
  geom_vline(xintercept = percentile_10, color = "red", linetype = "dashed", size = 0.5) +
  annotate("text", x = percentile_10, y = 5, label = paste(round(percentile_10, 1), "units"), color = "red", angle = 90, vjust = -0.5) +
  labs(title = "Mean Data Over Time",
       x = "Data Units",
       y = "Frequency") +
  scale_x_continuous(
    limits = c(min_value, max_value),  # Adjust min_value and max_value based on your data
    breaks = seq(min_value, max_value, by = step_size), 
    expand = c(0, 0)  
  ) +
  scale_y_continuous(
    limits = c(0, 10),  
    breaks = seq(0, 10, by = 2),  
    expand = c(0, 0) 
  ) +
  theme_minimal()

# Time series plot
ggplot(data_filtered_df, aes(x = Year, y = Data)) +
  geom_line(color = "black") +  
  geom_point(aes(color = Data <= percentile_10), size = 2) + 
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "slategrey"), guide = FALSE) +  
  geom_hline(yintercept = percentile_10, linetype = "dashed", color = "black") +  
  annotate("text", x = end_year, y = percentile_10, label = paste(round(percentile_10, 1), "units"), color = "red", vjust = -0.5) +
  labs(title = "Mean Data Over Time",
       x = "Year",
       y = "Data Units") +
  scale_x_continuous(
    limits = c(start_year, end_year + 1),  
    breaks = seq(start_year, end_year, by = 5), 
    expand = c(0, 0)  
  ) +
  scale_y_continuous(
    limits = c(min_value, max_value),  
    breaks = seq(min_value, max_value, by = step_size),  
    expand = c(0, 0) 
  ) +
  theme_minimal()

# To extract data for a specific location (latitude, longitude)

# Specify location coordinates
latitude <- target_latitude  # Replace with target latitude
longitude <- target_longitude  # Replace with target longitude

# Find the nearest latitude and longitude indices
lat_index <- which.min(abs(latitudes - latitude))
lon_index <- which.min(abs(longitudes - longitude))

cat("Nearest latitude index: ", lat_index, " Latitude: ", latitudes[lat_index], "\n")
cat("Nearest longitude index: ", lon_index, " Longitude: ", longitudes[lon_index], "\n")

# Extract data for the specific location
data_location <- data[lat_index, lon_index, ]

# Print extracted data
print(data_location)

# For raster extraction
library(raster)

# Load the NetCDF file as a RasterBrick
raster_data <- brick(file)

# Coordinates for the location
target_latitude <- target_latitude  # Replace with target latitude
target_longitude <- target_longitude  # Replace with target longitude

# Extract data for the specific location
extracted_data <- extract(raster_data, cbind(target_longitude, target_latitude))
extracted_data <- as.vector(extracted_data)

# Create a data frame with the years and corresponding extracted data
extracted_data_df <- data.frame(Year = years, Data = extracted_data)

# View the resulting data frame
print(extracted_data_df)

# Remove specific years if needed
extracted_data_filtered_df <- extracted_data_df[!extracted_data_df$Year %in% c(start_year, end_year), ]

# Calculate the 10th percentile for the extracted data
percentile_10_extracted <- quantile(extracted_data_filtered_df$Data, 0.10)
print(percentile_10_extracted)

# Plot extracted data
ggplot(extracted_data_filtered_df, aes(x = Data)) +
  geom_histogram(binwidth = 50, fill = "slategrey", color = "black") +  
  geom_vline(xintercept = percentile_10_extracted, color = "red", linetype = "dashed", size = 0.5) +
  annotate("text", x = percentile_10_extracted, y = 5, label = paste(round(percentile_10_extracted, 1), "units"), color = "red", angle = 90, vjust = -0.5) +
  labs(title = "Extracted Data at Specific Location",
       x = "Data Units",
       y = "Frequency") +
  scale_x_continuous(
    limits = c(min_value, max_value),  
    breaks = seq(min_value, max_value, by = step_size), 
    expand = c(0, 0)  
  ) +
  scale_y_continuous(
    limits = c(0, 10),  
    breaks = seq(0, 10, by = 2),  
    expand = c(0, 0) 
  ) +
  theme_minimal()

# Time series plot for the extracted data
ggplot(extracted_data_filtered_df, aes(x = Year, y = Data)) +
  geom_line(color = "black") +  
  geom_point(aes(color = Data <= percentile_10_extracted), size = 2) + 
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "slategrey"), guide = FALSE) +  
  geom_hline(yintercept = percentile_10_extracted, linetype = "dashed", color = "black") +  
  annotate("text", x = end_year, y = percentile_10_extracted, label = paste(round(percentile_10_extracted, 1), "units"), color = "red", vjust = -0.5) +
  labs(title = "Extracted Data Over Time",
       x = "Year",
       y = "Data Units") +
  scale_x_continuous(
    limits = c(start_year, end_year + 1),  
    breaks = seq(start_year, end_year, by = 5), 
    expand = c(0, 0)  
  ) +
  scale_y_continuous(
    limits = c(min_value, max_value),  
    breaks = seq(min_value, max_value, by = step_size),  
    expand = c(0, 0) 
  ) +
  theme_minimal()
