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


################################################################################
## DATA PROCESSING ## 

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

# Simple Data Transformations: 
# Data Frame: 
df <- read_voltage_data(file_path)
# Reorder columns with time as the first column
df <- df %>% select(time, everything())

# Split Data Frames By Channel: 
sub_dataframes <- lapply(names(df)[-1], function(col_name) {
  sub_df <- df[, c("time", col_name), drop = FALSE]
  # sub_df$time <- as.POSIXct(sub_df$time, format="%H:%M:%S")
  return(sub_df)
})

# assign names from original data frame to generate sub dfs: 
names(sub_dataframes) <- names(df)[-1]

## PLOTTING SUB DATAFRAMES ## 
# Test Plot
ts_plot(as.data.frame(sub_dataframes[3]))

# Generate All plots of Sub Data Frames: 
for (i in 1:7) {
  ts_plot(as.data.frame(sub_dataframes[i]))
}

# save plots to tiff
for (i in 1:7) {
  file_name = paste("differential", i, ".tiff", sep="")
  tiff(file_name)
  print(plot_list[[i]])
  dev.off()
}

################################################################################
# TODO: ADD DESCRIPTIVE STATISTICS



# Remove Time for correlation test: 
numeric_df <- df[, sapply(df, is.numeric)]

# Calculate correlation matrix
cor_matrix <- cor(numeric_df)

print(cor_matrix)
# Visualize the correlation matrix using a heatmap
heatmap(cor_matrix, symm = TRUE, cexRow = .7, cexCol = .3)

## CHANNEL SUBSELECTION ## 
# Channel 1: 
channel_1 = as.data.frame(sub_dataframes[1]) 
colnames(channel_1) <- c('time', 'voltage avg.') 
channel_1$`voltage avg.` <- as.numeric(channel_1$`voltage avg.`)
# Channel 2: 
channel_2 = as.data.frame(sub_dataframes[2]) 
colnames(channel_2) <- c('time', 'voltage avg.') 
channel_2$`voltage avg.` <- as.numeric(channel_2$`voltage avg.`)
# Channel 3: 
channel_3 = as.data.frame(sub_dataframes[3]) 
colnames(channel_3) <- c('time', 'voltage avg.') 
channel_3$`voltage avg.` <- as.numeric(channel_3$`voltage avg.`)




## POWER SPECTRAL DENSITY ## 
## Channel 1: 
# Power Spectral Density Channel 1: 
psd_1 <- spectrum(channel_1$`voltage avg.`, plot = FALSE)

#Extract Frequency
freq <- psd_1$freq
spec <- psd_1$spec

# Create a data frame for plotting
channel_1_psd <- data.frame(Frequency = freq, SpectralDensity = spec)


plot(channel_1_psd$Frequency, channel_1_psd$SpectralDensity, type = "l", col = "blue", lwd = 2,
     xlab = "Frequency", ylab = "Spectral Density",
     main = "Channel 1: Power Spectral Density",
     ylim = c(0,.0025),
     xlim = c(0, .3))

## Channel 2: 
# Power Spectral Density Channel 2: 
psd_2 <- spectrum(channel_2$`voltage avg.`, plot = FALSE)

#Extract Frequency
freq <- psd_2$freq
spec <- psd_2$spec

# Create a data frame for plotting
channel_2_psd <- data.frame(Frequency = freq, SpectralDensity = spec)

# Plot PSD of Channel 2: 
plot(channel_2_psd$Frequency, channel_2_psd$SpectralDensity, type = "l", col = "blue", lwd = 2,
     xlab = "Frequency", ylab = "Spectral Density",
     main = "Channel 2: Power Spectral Density",
     ylim = c(0,.004),
     xlim = c(0, .3))

## Channel 3: 
# Power Spectral Density Channel 3: 
psd_3 <- spectrum(channel_3$`voltage avg.`, plot = FALSE)

#Extract Frequency
freq <- psd_3$freq
spec <- psd_3$spec

# Create a data frame for plotting
channel_3_psd <- data.frame(Frequency = freq, SpectralDensity = spec)

# Plot PSD of Channel 3: 
plot(channel_3_psd$Frequency, channel_3_psd$SpectralDensity, type = "l", col = "blue", lwd = 2,
     xlab = "Frequency", ylab = "Spectral Density",
     main = "Channel 3: Power Spectral Density",
     ylim = c(0,.004),
     xlim = c(0, .3))


################################################################################
# FILTERING FUNCTIONS ## 

# Channel 1 Filter
# Lower and Upper Cutoff Frequencies based on PSD 
lowcut <- 0.025 # Define the cutoff frequency
highcut <- .15 # 
sampling_rate <- 1 # 1 sample per second

butter_low <- butter(2, W = c(lowcut, highcut)/(sampling_rate/2), type = "pass")
df_filter <- as.data.frame(filtfilt(butter_low, channel_1$`voltage avg.`))
df_filter$time <- channel_1$time # Add back the time column

# par(mfrow = c(1, 2))
# Original Channel Data: 
ts_plot(channel_1)
# Channel 1 Filtered Data: df_filter
ts_plot(df_filter)

# Channel 3 
# Define cutoff frequencies
lowcut <- 0.01
highcut <- 0.15

# Sampling rate (assumed to be 1 sample per second)
sampling_rate <- 1

# Design Butterworth bandpass filter
butter_bandpass_3 <- butter(2, c(lowcut, highcut) / (sampling_rate / 2), type = "pass")
filtered_3 <- as.data.frame(filtfilt(butter_bandpass_3, channel_3$`voltage avg.`))
filtered_3$time <- channel_3$time

ts_plot(filtered_3)
################################################################################
## SPIKE DETECTION ## 

# Rename function to spike_annotation: 
spike_detection <- function(data, window_size, threshold, distance) {
  spikes <- c()
  s_vector <- logical(length(data))  # Initialize a logical vector
  n <- length(data)
  
  # Calculate local average and detect spikes
  for (i in (2*window_size + 1):(n - 2*window_size)) {
    neighborhood <- data[(i - 2*window_size):(i + 2*window_size)]
    local_avg <- sum(neighborhood) / (4 * window_size)
    
    # print(paste("local avg:", local_avg))
    # print(paste("data", data[i]))
    
    if (abs(data[i] - local_avg) > threshold) {
      spikes <- c(spikes, i)
      s_vector[i] <- TRUE
    }
  }
  
  return(s_vector)
}

# Parameters for Ghost Fungi
window_size <- 50
threshold <- 0.03
distance <- 100

# Generate Annotated Spike Column
subset_3 <- head(filtered_3, n = 100000) # Keeps the first 100 rows
vector_annotation <- spike_detection(subset_3$`filtfilt(butter_bandpass_3, channel_3$\`voltage avg.\`)`, window_size, threshold, distance)
subset_3$spike <- vector_annotation

# Print The Spike Index 
which(subset_3$spike == TRUE)

# Interval Detection 
interval <- 0
findInterval <- function(spikes) {
  interval[1] = 0
  for (i in 2:length(spikes)) {
    interval[i] <- spikes[i] - spikes[i-1]
  }
  return(interval)
}

interval <- findInterval(subset_3$spike)
print(interval)
subset_3$`filtfilt(butter_bandpass_3, channel_3$\`voltage avg.\`)`


## Discrete Wavelet Transform 
# Function to find the nearest power of 2 and pad the data
nearest_power_of_2 <- function(n) {
  return(2^floor(log2(n)))
}

X <- filtered_3$`filtfilt(butter_bandpass_3, channel_3$\`voltage avg.\`)`
nearest_length <- nearest_power_of_2(length(X))

# Perform DWT on padded data
trimmed_data <- X[1:nearest_length]

#Discete Wave Transform 
result <- dwt(trimmed_data, wf = "la8", n.levels = 4, boundary = "periodic")
summary(result)

# Assuming 'result' is the DWT result object
par(mfrow = c(5, 1), mar = c(2, 2, 2, 1))  # Set up the plotting area to have 5 rows and 1 column

# Plot the detail coefficients at each level
plot(result$d1, type = "l", main = "Detail Coefficients (Level 1)", xlab = "Index", ylab = "Amplitude")
plot(result$d2, type = "l", main = "Detail Coefficients (Level 2)", xlab = "Index", ylab = "Amplitude")
plot(result$d3, type = "l", main = "Detail Coefficients (Level 3)", xlab = "Index", ylab = "Amplitude")
plot(result$d4, type = "l", main = "Detail Coefficients (Level 4)", xlab = "Index", ylab = "Amplitude")

# Plot the approximation coefficient at the highest level
plot(result$s4, type = "l", main = "Approximation Coefficient (Level 4)", xlab = "Index", ylab = "Amplitude")

# Reset the plotting area to default
par(mfrow = c(1, 1))
################################################################################
## WAVEFORM EXTRACTION ## 
## Remove Duplicates
filter_spikes <- function(spikes) {
  # Filter out spikes that are too close to each other
  filtered_spikes <- c()
  if (length(spikes) > 0) {
    filtered_spikes <- spikes[1]
    for (i in 2:length(spikes)) {
      if ((spikes[i] - filtered_spikes[length(filtered_spikes)]) > distance) {
        filtered_spikes <- c(filtered_spikes, spikes[i])
      }
    }
  }
}

# Function to extract waveforms around detected spikes
extract_waveforms <- function(data, window_size) {
  # Initialize an empty list to store waveforms
  waveforms <- list()
  
  # Loop through the dataframe to find spike indices
  for (i in 1:nrow(data)) {
    if (data$spike[i]) {
      # Define the start and end index for the waveform extraction
      start_idx <- max(1, i - window_size)
      end_idx <- min(nrow(data), i + window_size)
      
      # Extract the waveform
      waveform <- data[start_idx:end_idx, ]
      
      # Add the extracted waveform to the list
      waveforms[[length(waveforms) + 1]] <- waveform
    }
  }
  
  return(waveforms)
}


# Waveform Extraction Example: 
window_size <- 10 # Number of samples before and after the spike
waveforms <- extract_waveforms(subset_3, window_size)


# Loop through the list and create individual data frames
for (i in seq_along(waveforms)) {
  assign(paste0("df", i), waveforms[[i]])
}


# Display the first extracted waveform
mean(waveforms[[1]]$`filtfilt(butter_bandpass_3, channel_3$\`voltage avg.\`)`)


# Generate All plots of Sub Data Frames: 
for (i in 1:7) {
  ts_plot(as.data.frame(waveforms[i]))
}


# Assuming 'waveforms' 
extracted_values <- list()

# Iterate through the list and extract the 'value' column
for (i in 1:length(waveforms)) {
  # Extract the 'value' column and name it as 'value_i' where i is the index
  values <- waveforms[[i]]$`filtfilt(butter_bandpass_3, channel_3$\`voltage avg.\`)`
  extracted_values[[i]] <- values
}

#combine values 
combined_values <- do.call(cbind, extracted_values)
# Convert the combined values to a data frame and set column names
combined_values_df <- as.data.frame(combined_values)
colnames(combined_values_df) <- paste0("value_", 1:length(waveforms))


# Print the combined data frame
print(combined_values_df)
