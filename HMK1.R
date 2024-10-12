# Load required libraries
install.packages("tidyverse")
library(tidyverse)
install.packages("readr")
library(readr)
pacman::p_load(tidyverse, readr, scales, ggplot2)

# Set the working directory (if needed)
# setwd("path/to/your/project")

# Create and save the histogram of danceability
danceability_plot <- ggplot(TS, aes(x = danceability)) +
  geom_histogram(binwidth = 0.05, fill = "lightblue", color = "violet") +
  geom_vline(aes(xintercept = danceability_mean), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = danceability_sd), color = "blue", linetype = "solid", size = 1) +
  annotate("text", x = danceability_mean + 0.05, y = 20, label = paste("Mean =", round(danceability_mean, 2)), color = "red") +
  annotate("text", x = danceability_sd - 0.05, y = 18, label = paste("SD =", round(danceability_sd, 2)), color = "blue") +
  labs(title = "Distribution of Danceability in Taylor Swift's Songs",
       x = "Danceability",
       y = "Count") +
  theme_minimal()

# Save the histogram plot
ggsave("danceability_histogram.png", plot = danceability_plot, width = 10, height = 6)

# Create and save the boxplot of valence by album
valence_boxplot <- TS %>% 
  ggplot(aes(x = album, y = valence, fill = album)) + 
  geom_boxplot() + 
  scale_fill_brewer(palette = "Set3") + 
  theme_minimal() +
  labs(title = "Distribution of Valence by Album", 
       x = "Album", 
       y = "Valence") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Save the boxplot
ggsave("valence_boxplot.png", plot = valence_boxplot, width = 10, height = 6)

# Create and save the scatter plot of loudness vs energy
loudness_energy_plot <- TS %>%
  ggplot(aes(x = loudness, y = energy)) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method = "lm") +  
  facet_wrap(~version) +     
  labs(x = "Loudness", y = "Energy", title = "Loudness vs Energy by Taylor's Version and Original Version", 
       caption = "There seems to be a general but weak association between the loudness of a song and the energy of the song in both versions of Taylor's music. However, in Taylor's versions, the association starts weak but strengthens over time.") +
  theme_minimal() + 
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),  
    plot.title = element_text(size = 14, face = "bold"),  
    plot.caption = element_text(size = 8, hjust = 0) 
  )

# Save the loudness vs energy plot
ggsave("loudness_energy_plot.png", plot = loudness_energy_plot, width = 10, height = 6)

# Create and save the plot for Superbowl ads over time
ads_line_plot <- ads %>%
  count(year) %>%  
  ggplot(aes(x = year, y = n)) +  
  geom_line() + 
  labs(x = "Year", y = "# of Superbowl Ads", title = "Number of Superbowl Ads Over Time") +
  theme_minimal()  

# Save the Superbowl ads plot
ggsave("superbowl_ads_over_time.png", plot = ads_line_plot, width = 10, height = 6)

# Create and save the relationship plot for funny and dangerous ads
funny_dangerous_plot <- summary_data_funny %>%
  ggplot(aes(x = funny, y = Count, fill = danger)) +
  geom_bar(stat = "identity", position = "dodge", color = "blue") +
  labs(title = "Relationship between the independence between being funny and dangerous",
       x = "Funny ads",
       y = "Proportion",
       fill = "Danger") +
  facet_wrap(~danger) +
  theme_minimal() +
  scale_fill_manual(values = c("TRUE" = "green", 
                               "FALSE" = "orange")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the funny vs dangerous ads plot
ggsave("funny_dangerous_ads_plot.png", plot = funny_dangerous_plot, width = 10, height = 6)

# Create and save the plot for fastest Mario Kart tracks
mario_kart_plot <- ggplot(fastest_records, aes(x = date, y = time, color = track, shape = shortcut, size = record_duration)) +
  geom_point(alpha = 0.8) +  
  geom_smooth(se = FALSE, method = "loess", aes(group = interaction(track, shortcut)), linetype = "dashed", size = 0.5) +
  scale_y_reverse() + 
  facet_grid(system_played ~ type, scales = "free_y") + 
  labs(
    title = "Evolution of Mario Kart 64 Records for the Three Fastest Tracks",
    subtitle = paste("Tracks: ", paste(fastest_tracks, collapse = ", ")),
    x = "Date",
    y = "Record Time (seconds)",
    color = "Track",
    shape = "Shortcut",
    size = "Record Duration (days)"
  ) +
  scale_shape_manual(values = c(16, 17)) +  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right",
    panel.grid.major = element_line(size = 0.2),
    panel.grid.minor = element_blank()
  ) +
  geom_text(data = subset(fastest_records, time == min(time)), 
            aes(label = track), vjust = -1, color = "black", size = 3) 

# Save the Mario Kart plot
ggsave("mario_kart_records_plot.png", plot = mario_kart_plot, width = 10, height = 6)




# Set the working directory (if needed)
# setwd("path/to/your/project")

# Create and save the histogram of danceability
danceability_plot <- ggplot(TS, aes(x = danceability)) +
  geom_histogram(binwidth = 0.05, fill = "lightblue", color = "violet") +
  geom_vline(aes(xintercept = danceability_mean), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = danceability_sd), color = "blue", linetype = "solid", size = 1) +
  annotate("text", x = danceability_mean + 0.05, y = 20, label = paste("Mean =", round(danceability_mean, 2)), color = "red") +
  annotate("text", x = danceability_sd - 0.05, y = 18, label = paste("SD =", round(danceability_sd, 2)), color = "blue") +
  labs(title = "Distribution of Danceability in Taylor Swift's Songs",
       x = "Danceability",
       y = "Count") +
  theme_minimal()

# Save the histogram plot
ggsave("danceability_histogram.png", plot = danceability_plot, width = 10, height = 6)

# Create and save the boxplot of valence by album
valence_boxplot <- TS %>% 
  ggplot(aes(x = album, y = valence, fill = album)) + 
  geom_boxplot() + 
  scale_fill_brewer(palette = "Set3") + 
  theme_minimal() +
  labs(title = "Distribution of Valence by Album", 
       x = "Album", 
       y = "Valence") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Save the boxplot
ggsave("valence_boxplot.png", plot = valence_boxplot, width = 10, height = 6)

# Create and save the scatter plot of loudness vs energy
loudness_energy_plot <- TS %>%
  ggplot(aes(x = loudness, y = energy)) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method = "lm") +  
  facet_wrap(~version) +     
  labs(x = "Loudness", y = "Energy", title = "Loudness vs Energy by Taylor's Version and Original Version", 
       caption = "There seems to be a general but weak association between the loudness of a song and the energy of the song in both versions of Taylor's music. However, in Taylor's versions, the association starts weak but strengthens over time.") +
  theme_minimal() + 
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),  
    plot.title = element_text(size = 14, face = "bold"),  
    plot.caption = element_text(size = 8, hjust = 0) 
  )

# Save the loudness vs energy plot
ggsave("loudness_energy_plot.png", plot = loudness_energy_plot, width = 10, height = 6)

# Create and save the plot for Superbowl ads over time
ads_line_plot <- ads %>%
  count(year) %>%  
  ggplot(aes(x = year, y = n)) +  
  geom_line() + 
  labs(x = "Year", y = "# of Superbowl Ads", title = "Number of Superbowl Ads Over Time") +
  theme_minimal()  

# Save the Superbowl ads plot
ggsave("superbowl_ads_over_time.png", plot = ads_line_plot, width = 10, height = 6)

# Create and save the relationship plot for funny and dangerous ads
funny_dangerous_plot <- summary_data_funny %>%
  ggplot(aes(x = funny, y = Count, fill = danger)) +
  geom_bar(stat = "identity", position = "dodge", color = "blue") +
  labs(title = "Relationship between the independence between being funny and dangerous",
       x = "Funny ads",
       y = "Proportion",
       fill = "Danger") +
  facet_wrap(~danger) +
  theme_minimal() +
  scale_fill_manual(values = c("TRUE" = "green", 
                               "FALSE" = "orange")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the funny vs dangerous ads plot
ggsave("funny_dangerous_ads_plot.png", plot = funny_dangerous_plot, width = 10, height = 6)

# Create and save the plot for fastest Mario Kart tracks
mario_kart_plot <- ggplot(fastest_records, aes(x = date, y = time, color = track, shape = shortcut, size = record_duration)) +
  geom_point(alpha = 0.8) +  
  geom_smooth(se = FALSE, method = "loess", aes(group = interaction(track, shortcut)), linetype = "dashed", size = 0.5) +
  scale_y_reverse() + 
  facet_grid(system_played ~ type, scales = "free_y") + 
  labs(
    title = "Evolution of Mario Kart 64 Records for the Three Fastest Tracks",
    subtitle = paste("Tracks: ", paste(fastest_tracks, collapse = ", ")),
    x = "Date",
    y = "Record Time (seconds)",
    color = "Track",
    shape = "Shortcut",
    size = "Record Duration (days)"
  ) +
  scale_shape_manual(values = c(16, 17)) +  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right",
    panel.grid.major = element_line(size = 0.2),
    panel.grid.minor = element_blank()
  ) +
  geom_text(data = subset(fastest_records, time == min(time)), 
            aes(label = track), vjust = -1, color = "black", size = 3) 

# Save the Mario Kart plot
ggsave("mario_kart_records_plot.png", plot = mario_kart_plot, width = 10, height = 6)


