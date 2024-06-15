

# Function to plot and save each spike waveform
plot_waveforms <- function(data, spikes, window_size, output_dir) {
  # Create the output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  for (i in 1:length(spikes)) {
    spike_index <- spikes[i]
    
    # Extract the waveform around the spike
    start_index <- max(1, spike_index - window_size)
    end_index <- min(nrow(data), spike_index + window_size)
    waveform <- data[start_index:end_index, ]
    
    # Create a plot
    p <- ggplot(waveform, aes(x = time, y = `filtfilt(butter_low, channel_1$voltage avg.)`)) +
      geom_line() +
      labs(title = paste("Spike at index", spike_index),
           x = "Time",
           y = "Voltage") +
      theme_minimal()
    
    # Save the plot as an image file
    ggsave(filename = paste0(output_dir, "/spike_", i, ".png"), plot = p)
  }
}

# Usage
spikes <- which(df_filter$spike == TRUE)
window_size <- 10  # Adjust the window size as needed
output_dir <- "spike_waveforms"

plot_waveforms(df_filter, spikes, window_size, output_dir)




# Test Plot
ts_plot(as.data.frame(sub_dataframes[1]))

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


# Assuming your dataframe is named df
# Define the voltage and time range you want to zoom in on
low_voltage_limit <- -2
high_voltage_limit <- 2
start_time <- "00:00:01"
end_time <- "00:01:00"

# Filter the dataframe based on the time range
df_filtered <- df[df$time >= start_time & df$time <= end_time, ]

# Melt the dataframe to long format for ggplot
df_long <- melt(df_filtered, id.vars = "time", variable.name = "Differential", value.name = "Voltage")

# Create the plot
ggplot(df_long, aes(x = time, y = Voltage, color = Differential)) +
  geom_line() +
  ylim(low_voltage_limit, high_voltage_limit) +
  labs(title = "Zoomed-In Voltage Plot", x = "Time", y = "Voltage (mV)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ts_plot(time_filtered_df, type = "multiple")


time_filtered_df <- subset(df, time >= "00:00:01" & time <= "00:01:00")

# Convert the 'time' column to a proper time format
time_filtered_df$time <- as.POSIXct(time_filtered_df$time, format="%H:%M:%S")

ts_plot(time_filtered_df)
# Melt the dataframe for easy plotting with ggplot2
library(reshape2)
melted_df <- melt(time_filtered_df, id.vars = "time")

# Plotting with ggplot2
ggplot(melted_df, aes(x = time, y = value, color = variable)) +
  geom_line() +
  labs(title = "Voltage Time Series", x = "Time (sec)", y = "Potential (mV)") +
  scale_x_datetime(date_labels = "%H:%M:%S", date_breaks = "10 sec") +
  theme_minimal()



# Melted Plots
# # Melt the data frame to long format
melted_data <- melt(df_sub, id.vars = "time")

ggplot(melted_data, aes(x = time, y = value, color = variable)) +
  geom_line() +
  labs(title = "Voltage Over Time",
       x = "Time (s)",
       y = "Voltage (mV)") +
  theme_minimal()
# 
# # Create the plot using ggplot