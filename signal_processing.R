
## CHANNEL SUBSELECTION ## 
# Channel 1: 
# Power Spectral Density 
channel_1 = as.data.frame(sub_dataframes[1]) 
colnames(channel_1) <- c('time', 'voltage avg.') 
channel_1$`voltage avg.` <- as.numeric(channel_1$`voltage avg.`)

# Power Spectral Density: 
psd_result <- spectrum(channel_1$`voltage avg.`, plot = FALSE)

#Extract Frequency
freq <- psd_result$freq
spec <- psd_result$spec

# Create a data frame for plotting
psd_1 <- data.frame(Frequency = freq, SpectralDensity = spec)


plot(psd_1$Frequency, psd_1$SpectralDensity, type = "l", col = "blue", lwd = 2,
     xlab = "Frequency", ylab = "Spectral Density",
     main = "Channel 1: Power Spectral Density",
     ylim = c(0,.0025),
     xlim = c(0, .3))


# # Plot the PSD
# ggplot(psd_1, aes(x = Frequency, y = SpectralDensity)) +
#   geom_line(color = "blue") +
#   theme_minimal() +
#   labs(title = "Power Spectral Density", x = "Frequency", y = "Spectral Density")


# Define filter parameters
lowcut <- 0.01
highcut <- .30
sampling_rate <- 1 # 1 sample per second

# Create bandpass filter
bf <- butter(2, W = c(lowcut, highcut) / (sampling_rate / 2), type = "pass")

# Apply filter to each differential column
filtered_df <- as.data.frame(lapply(df[, -1], function(x) filtfilt(bf, x))) # Exclude the time column
filtered_df$time <- df$time # Add back the time column






# Visualize filtered data
library(ggplot2)

ggplot(melt(filtered_df, id.vars = 'time'), aes(x = time, y = value, colour = variable)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Filtered Time Series Data", x = "Time", y = "Voltage (mV)")


# Min/Max 
for (col in names(df[, -1])) {
  print(paste("Column:", col))
  print(paste("min:", min(df[col])))
  print(paste("max:", max(df[col])))
}





# Compute the power spectral density
psd <- spectrum(df$`Differential 1 - 2 Ave. (mV)`, plot=FALSE)
freq <- psd$freq
spec <- psd$spec
plot(freq, spec, type="l", main="Power Spectral Density", xlab="Frequency (Hz)", ylab="Power")

# Function to convert the data frame to a dictionary
convert_to_dict <- function(data) {
  dict <- list()
  for (i in 1:nrow(data)) {
    time <- as.character(data$time[i])
    values <- as.list(data[i, -1])  # Exclude the time column
    dict[[time]] <- values
  }
  return(dict)
}


# Function to convert the data frame to a dictionary
convert_to_dict <- function(data) {
  dict <- list()
  for (i in 1:nrow(data)) {
    time <- as.character(data$time[i])
    values <- as.numeric(data[i, -which(names(data) == "time")])  # Exclude the time column and convert to numeric
    dict[[time]] <- values
  }
  return(dict)
}

# Apply the function
df_dict <- convert_to_dict(df_long[1:1000, ])

# Print the first few elements of the dictionary to check
print(head(df_dict))

