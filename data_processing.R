
# Set Working Directory 
setwd("/home/danielolds/code/CS_670/cs670_fungal_project")

# Load Packages
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(reshape2)
library(signal)
library(TSstudio)
library(waveslim)

# File Path
file_path = "/home/danielolds/code/CS_670/cs670_fungal_project/Ghost Fungi Omphalotus nidiformis.txt"

# Function: Load Data and Perform basic Preprocessing
read_voltage_data <- function(file_path) {
  # Read the data from the txt file
  data <- read_delim(file_path, delim = "\t", 
                     col_names = c("Differential 1 - 2 Ave. (mV)", "Differential 3 - 4 Ave. (mV)", "Differential 5 - 6 Ave. (mV)", "Differential 7 - 8 Ave. (mV)", 
                                   "Differential 9 - 10 Ave. (mV)", "Differential 11 - 12 Ave. (mV)", "Differential 13 - 14 Ave. (mV)"))
  # Convert to numeric with dplyr
  data <- mutate_all(data[1:7], function(x) as.numeric(as.character(x)))
  
  # Generate the time column with one-second intervals
  start_time <- as.POSIXct("2024-06-01 00:00:00")  # Define the start time
  time_sequence <- seq(from = start_time, by = "1 sec", length.out = nrow(data))
  
  # Format the time column to exclude the date
  # time_sequence_formatted <- format(time_sequence, format = "%H:%M:%S")
  
  # Append the formatted time column to the data frame
  data$time <- time_sequence
  
  # Remove First Row 
  data <- data[-1, ]
  
  # Omit Missing Values
  data <- na.omit(data)

  # Return Data Frame 
  return(as.data.frame(data))
}


# Data Transformations: 
# Data Frame: 
df <- read_voltage_data(file_path)
# Reorder columns with time as the first column
df <- df %>% select(time, everything())

# Data Frame: Long 
df_long <- df %>%
  pivot_longer(cols = starts_with("Differential"),
               names_to = "Differential",
               values_to = "Voltage")

# Split Data Frames By Channel: 
sub_dataframes <- lapply(names(df)[-1], function(col_name) {
  sub_df <- df[, c("time", col_name), drop = FALSE]
  # sub_df$time <- as.POSIXct(sub_df$time, format="%H:%M:%S")
  return(sub_df)
})

# assign names from original data frame to generate sub dfs: 
names(sub_dataframes) <- names(df)[-1]




