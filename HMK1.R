install.packages("tidyverse")
library (tidyverse)
install.packages("readr")
library(readr)
getwd()


danceability_mean <- mean(TS$danceability, na.rm = TRUE)
danceability_sd <- sd(TS$danceability, na.rm = TRUE)
danceability_median <- median(TS$danceability, na.rm = TRUE)

ggplot(TS, aes(x = danceability)) +
  geom_histogram(binwidth = 0.05, fill = "lightblue", color = "violet") +
  geom_vline(aes(xintercept = danceability_mean), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = danceability_sd), color = "blue", linetype = "solid", size = 1) +
  annotate("text", x = danceability_mean + 0.05, y = 20, label = paste("Mean =", round(danceability_mean, 2)), color = "red") +
  annotate("text", x = danceability_sd- 0.05, y = 18, label = paste("SD =", round(danceability_sd, 2)), color = "blue") +
  labs(title = "Distribution of Danceability in Taylor Swift's Songs",
       x = "Danceability",
       y = "Count") +
  theme_minimal()

TS %>% 
  arrange(desc (duration)) %>% 
  slice(2)


print(TS)

mean(TS$duration)
sd(TS$duration)



TS <- TS %>%
  mutate(version = ifelse(album %in% c("Red", "Fearless"), "Taylor's Version", "Original Version"))

# Part 1: B 
TS %>% 
  ggplot(aes(x = album, y = valence, fill = album)) + 
  geom_boxplot() + 
  scale_fill_brewer(palette = "Set3") + 
  theme_minimal() +
  labs(title = "Distribution of Valence by Album", 
       x = "Album", 
       y = "Valence",) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#Part 1:  C
TS <- TS %>%
  mutate(version = ifelse(album %in% c("Red", "Fearless"), "Taylor's Version", "Original Version"))


TS %>%
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

#Part 2 the avengers 
D1 <- xtabs(~ death + return, data = avengers)
D1_prop <- prop.table(D1) #basically do the mutliplaction rule for you 1/proportion times another one.
D1_rounded <- round(D1_prop * 100, 2)
print(D1_rounded)



#3 SUperbowl ads A
ads %>%
  count(year) %>%  
  ggplot(aes(x = year, y = n)) +  
  geom_line() + 
  labs(x = "Year", y = "# of Superbowl Ads", title = "Number of Superbowl Ads Over Time") +
  theme_minimal()  


#Part 3: C

head(ads)

summary_data <- ads %>%
  group_by(patriotic, celebrity) %>%
  summarise(Count = n()) %>%
  ungroup()

summary_data <- summary_data %>%
  group_by(patriotic) %>%
  mutate(Proportion = Count / sum(Count))

print(summary_data)

ggplot(summary_data, aes(x = patriotic, y = Proportion, fill = celebrity)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportion of Patriotic Ads with Celebrity Endorsements",
       x = "Patriotic Ads",
       y = "Count",
       fill = "Celebrity") +
  theme_minimal() +
  scale_fill_manual(values = c("TRUE" = "#FF9999", 
                               "FALSE" = "#99CCFF")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Part B 
ads_wo <- ads %>%
  filter(animals == F)
ads_w <- ads %>%
  filter(animals == T)

count(ads_wo)
median(ads_w)
count(ads)

quantile(ads_wo$likes,.90)
quantile(ads_w$likes, 0.90)

median(ads_w$views)

sd(ads_w$dislikes)



DS <- xtabs(~patriotic + celebrity, data = ads)
prop.table(DS)

head(ads)

#part C


ads %>%
  ggplot(aes(x = brand, fill = animals)) + 
  geom_bar() +  
  labs(x = "Brand", y = "Count of Ads", 
       title = "Count of Ads Featuring Animals by Brand") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

glimpse(ads)

#Part 3:E
DS <- xtabs(~danger + funny, data = ads)
prop.table(DS)

summary_data_funny <- ads %>%
  group_by(funny, danger) %>%
  summarise(Count = n()) %>%
  ungroup()



print(summary_data_funny)


ggplot(summary_data_funny, aes(x = funny, y = Count, fill = danger), ) +
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


#Part 4
records <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv')
drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/drivers.csv')

fastest_tracks <- records %>%
  group_by(track) %>%
  summarise(avg_time = mean(time)) %>%
  arrange(avg_time) %>%
  slice(1:3) %>%  
  pull(track)


fastest_records <- records %>%
  filter(track %in% fastest_tracks)


ggplot(fastest_records, aes(x = date, y = time, color = track, shape = shortcut, size = record_duration)) +
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

