# Install Packages
library(signal)


# Spike Detection: Most Recent Filter Function 
channel_1 = as.data.frame(sub_dataframes[1]) 
colnames(channel_1) <- c('time', 'voltage avg.') 


# Lower and Upper Cutoff Frequencies based on PSD 
lowcut <- 0.025 # Define the cutoff frequency
highcut <- .15 # 
sampling_rate <- 1 # 1 sample per second

butter_low <- butter(2, W = c(lowcut, highcut)/(sampling_rate/2), type = "pass")
df_filter <- as.data.frame(filtfilt(butter_low, channel_1$`voltage avg.`))
df_filter$time <- channel_1$time # Add back the time column

# par(mfrow = c(1, 2))
# ts_plot(channel_1)
ts_plot(df_filter)

# Spike Detection Neighborhood Algorithm: 


# calculate_sum <- function(data, i, window_size) {
#   sum_val <- 0
#   for (j in max(1, i - 2*window_size):(min(length(data), i + 2*window_size))) {
#     sum_val <- sum_val + data[j]
#   }
#   return(sum_val)
# }

# Function to detect spikes
detect_spikes <- function(data, window_size, threshold, distance) {
  
  spikes <- c()
  
  # Loop through the data
  for (i in seq(window_size + 1, length(data) - window_size)) {
    if (i - 2*window_size > 0 && i + 2*window_size <= length(data)){
    neighborhood <- data[(i-2*window_size):(i+2*window_size)] # Compute Neighborhood
    # Calculate the local average
    local_avg <- ((4*window_size)^-1)*sum(neighborhood, na.rm = TRUE) # Sum divided by window*4
    
    # Check if the current value is a spike
    if (abs(data[i] - local_avg) > threshold) {
      spikes <- c(spikes, i)
      }
    }
  }
  
  # Filter out spikes that are too close to each other
  filtered_spikes <- logical(length(data))
  last_spike <- -distance  # Initialize to a value that ensures the first spike will be included
  for (i in which(spikes)) {
    if ((i - last_spike) > distance) {
      filtered_spikes[i] <- TRUE
      last_spike <- i
    }
  }
  
  # # Filter out spikes that are too close to each other
  # filtered_spikes <- c(spikes[1]) # Always include the first spike
  # for (i in 2:length(spikes)) {
  #   if ((spikes[i] - filtered_spikes[length(filtered_spikes)]) > distance) {
  #     filtered_spikes <- c(filtered_spikes, spikes[i])
  #   }
  # }
  
  return(filtered_spikes)
}

detect_spikes_1 <- function(data, window_size, threshold, distance) {
  
  spikes <- logical(length(data))  # Initialize a logical vector
  
  # Loop through the data
  for (i in seq(window_size + 1, length(data) - window_size)) {
    if (i - 2 * window_size > 0 && i + 2 * window_size <= length(data)) {
      neighborhood <- data[(i - 2 * window_size):(i + 2 * window_size)] # Compute Neighborhood
      # Calculate the local average
      local_avg <- ((4 * window_size)^-1) * sum(neighborhood, na.rm = TRUE) # Sum divided by window*4
      
      # Check if the current value is a spike
      if (abs(data[i] - local_avg) > threshold) {
        spikes[i] <- TRUE
      }
    }
  }
  
  # Filter out spikes that are too close to each other
  filtered_spikes <- logical(length(data))
  last_spike <- -distance  # Initialize to a value that ensures the first spike will be included
  for (i in which(spikes)) {
    if ((i - last_spike) > distance) {
      filtered_spikes[i] <- TRUE
      last_spike <- i
    }
  }
  
  return(filtered_spikes)
}

# Parameters for Ghost Fungi
window_size <- 50
threshold <- 0.003
distance <- 100

subset_df <- head(df_filter, n = 100000) # Keeps the first 100 rows
spike_annotation <- detect_spikes_1(subset_df$`filtfilt(butter_low, channel_1$\`voltage avg.\`)`, window_size, threshold, distance)

subset_df$spike <- spike_annotation


true <- which(spike_annotation)
print(paste("True at", true))



detect_spikes_2 <- function(data, w, delta, d) {
  n <- length(data)
  spikes <- numeric(n)
  
  for (i in (2*w+1):(n-2*w)) {
    if (i - 2*w > 0 && i + 2*w <= n) {
      neighborhood <- data[(i-2*w):(i+2*w)]
      a_i <- (4 * w)^-1 * sum(neighborhood, na.rm = TRUE)
      if (abs(data[i] - a_i) > delta) {
        spikes[i] <- data[i]
      }
    }
  }

  
  # # Filter false spikes
  # valid_spikes <- which(spikes != 0)
  # true_spikes <- numeric(length(valid_spikes))
  # 
  # if (length(valid_spikes) > 1) {
  #   j <- 1
  #   true_spikes[j] <- valid_spikes[1]
  #   for (i in 2:length(valid_spikes)) {
  #     if (valid_spikes[i] - true_spikes[j] > d) {
  #       j <- j + 1
  #       true_spikes[j] <- valid_spikes[i]
  #     }
  #   }
  #   true_spikes <- true_spikes[true_spikes != 0]
  # } else {
  #   true_spikes <- valid_spikes
  # }
  
  return(spikes)
}


w <- 100
delta <- 0.003 
d <- 100

spikes_detected <- detect_spikes_2(df_filter$`filtfilt(butter_low, channel_1$\`voltage avg.\`)`[1:1000], w, delta, d)
print(spikes_detected)




# # Spike Detection
# detect_spikes <- function(data, w, delta, d) {
# 
#   n <- length(data)
#   spikes <- numeric(n)
# 
#   for (i in (2*w+1):(n-2*w)) {
#     if (i - 2*w > 0 && i + 2*w <= n){
#     neighborhood <- data[(i-2*w):(i+2*w)]
#     a_i <- (4 * w)^-1 * sum(neighborhood)
# 
#     if (!is.na(a_i) && abs(data[i] - a_i) > d){
#       spikes[i] <- data[i]
#       }
#     }
#   }
#   
#   print(spikes)
# 
#   # Filter false spikes
#   valid_spikes <- which(spikes != 0)
#   true_spikes <- numeric(length(valid_spikes))
# 
#   # Filter out spikes that are too close to each other
#   filtered_spikes <- c()
#   for (i in seq_along(spikes)) {
#     if (i == 1 || (spikes[i] - spikes[i - 1] > d)) {
#       filtered_spikes <- c(filtered_spikes, spikes[i])
#     }
#   }
# 
#   return(filtered_spikes)
# }
# 
# # Parameters for S. commune
# w <- 50
# delta <- 0.003
# d <- 100
# 
# spikes_detected <- detect_spikes(df_filter$`filtfilt(butter_low, channel_1$\`voltage avg.\`)`[1:100000], w, delta, d)
# print(spikes_detected)

detect_spikes_3 <- function(data, window_size, threshold, distance) {
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
  
  # # Filter out spikes that are too close to each other
  # filtered_spikes <- c()
  # if (length(spikes) > 0) {
  #   filtered_spikes <- spikes[1]
  #   for (i in 2:length(spikes)) {
  #     if ((spikes[i] - filtered_spikes[length(filtered_spikes)]) > distance) {
  #       filtered_spikes <- c(filtered_spikes, spikes[i])
  #     }
  #   }
  # }
  
  return(s_vector)
}

# Parameters for Ghost Fungi
window_size <- 50
threshold <- 0.1
distance <- 100

# Generate Annotated Spike Column
subset_df <- head(df_filter, n = 100000) # Keeps the first 100 rows
vector_annotation <- detect_spikes_3(df_filter$`filtfilt(butter_low, channel_1$\`voltage avg.\`)`, window_size, threshold, distance)
df_filter$spike <- vector_annotation

# Print The Spike Index 
which(df_filter$spike == TRUE)



# Interval Detection 
interval <- 0
findInterval <- function(spikes) {
  interval[1] = 0
  for (i in 2:length(spikes)) {
    interval[i] <- spikes[i] - spikes[i-1]
  }
  return(interval)
}

interval <- findInterval(spikes)

