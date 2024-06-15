

# KNN from ChatGPT
# Step 1: Normalize the Data (if necessary)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

normalized_df <- as.data.frame(lapply(combined_values_df, normalize))

# Step 2: Determine the Number of Clusters (e.g., using the Elbow Method)
wss <- (nrow(normalized_df)-1)*sum(apply(normalized_df, 2, var))
for (i in 2:15) wss[i] <- sum(kmeans(normalized_df, centers=i)$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# Step 3: Perform K-Means Clustering
set.seed(123)
k <- 3  # Assume we decided on 3 clusters from the Elbow method
kmeans_result <- kmeans(normalized_df, centers=k)

# Step 4: Add Cluster Information to the Dataframe
combined_values_df$cluster <- as.factor(kmeans_result$cluster)

# Performing PCA to reduce dimensions for visualization
pca_result <- prcomp(normalized_df, scale. = TRUE)
pca_df <- data.frame(pca_result$x[,1:2], cluster = combined_values_df$cluster)

ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) + 
  geom_point() + 
  labs(title = "K-Means Clustering with PCA", x = "Principal Component 1", y = "Principal Component 2")

# Step 5: Visualize the Clusters
library(ggplot2)
ggplot(combined_values_df, aes(x = value_1, y = value_2, color = cluster)) +  # Replace V1 and V2 with your actual column names
  geom_point() +
  labs(title = "K-Means Clustering", x = "Feature 1", y = "Feature 2")



###############################################################################
## Z-SCORE ALGORITHM ## 
detect_spikes_zscore <- function(data, threshold = 3) {
  # Calculate the mean and standard deviation
  data_mean <- mean(data, na.rm = TRUE)
  data_sd <- sd(data, na.rm = TRUE)
  
  # Calculate z-scores
  z_scores <- (data - data_mean) / data_sd
  
  # Detect spikes where z-score exceeds the threshold
  spikes <- which(abs(z_scores) > threshold)
  
  return(spikes)
}

# Example usage:
data <- channel_1$`voltage avg.` # Replace with your actual data column
threshold <- .003 # Adjust based on your specific case

spikes <- detect_spikes_zscore(subset_df$`filtfilt(butter_low, channel_1$\`voltage avg.\`)`, threshold)
print(spikes)


ThresholdingAlgo <- function(y,lag,threshold,influence) {
  
  signals <- rep(0,length(y))
  filteredY <- y[0:lag]
  avgFilter <- NULL
  stdFilter <- NULL
  
  avgFilter[lag] <- mean(y[0:lag], na.rm=TRUE)
  stdFilter[lag] <- sd(y[0:lag], na.rm=TRUE)
  
  for (i in (lag+1):length(y)){
    if (abs(y[i]-avgFilter[i-1]) > threshold*stdFilter[i-1]) {
      if (y[i] > avgFilter[i-1]) {
        signals[i] <- 1;
      } else {
        signals[i] <- -1;
      }
      filteredY[i] <- influence*y[i]+(1-influence)*filteredY[i-1]
    } else {
      signals[i] <- 0
      filteredY[i] <- y[i]
    }
    avgFilter[i] <- mean(filteredY[(i-lag):i], na.rm=TRUE)
    stdFilter[i] <- sd(filteredY[(i-lag):i], na.rm=TRUE)
  }
  return(list("signals"=signals,"avgFilter"=avgFilter,"stdFilter"=stdFilter))
}

lag       <- 6000
threshold <- 15
influence <- 1

result <- ThresholdingAlgo(filtered_3$`filtfilt(butter_bandpass_3, channel_3$\`voltage avg.\`)`,lag,threshold,influence)






## RANDOM NOTES ## 
detect_spikes <- function(data, window_size, delta, distance) {
  spikes <- c()
  n <- length(data)
  
  for (i in (2 * window_size + 1):(n - 2 * window_size)) {
    neighborhood <- data[(i - 2 * window_size):(i + 2 * window_size)]
    local_avg <- sum(neighborhood) / (4 * window_size)
    
    if (abs(data[i] - local_avg) > delta) {
      spikes <- c(spikes, i)
    }
  }
  
  filtered_spikes <- c()
  if (length(spikes) > 0) {
    filtered_spikes <- spikes[1]
    for (i in 2:length(spikes)) {
      if (spikes[i] - spikes[i - 1] > distance) {
        filtered_spikes <- c(filtered_spikes, spikes[i])
      }
    }
  }
  
  return(filtered_spikes)
}

# Example usage
# Assuming `data` is your data frame and you are working with a specific column of it, e.g., `data$voltage_avg`
spike_indices <- detect_spikes(df_filter$`filtfilt(butter_low, channel_1$\`voltage avg.\`)`[1:50000], window_size = 50, delta = 0.003, distance = 200)


# if (i - 2 * window_size > 0 && i + 2 * window_size <= length(subset_df)) {
#   neighborhood <- subset_df[(i - 2 * window_size):(i + 2 * window_size)] # Compute Neighborhood
#   # Calculate the local average
#   local_avg <- ((4 * window_size)^-1) * sum(neighborhood, na.rm = TRUE) # Sum divided by window*4
#   print(local_avg)
#   # Check if the current value is a spike
#   if (abs(subset_df[i] - local_avg) > threshold) {
#     spikes[i] <- TRUE
#   }
# }



