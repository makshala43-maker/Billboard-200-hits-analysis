install.packages("tidyverse")
install.packages("patchwork")

library(patchwork)
library(tidyverse)

music_data_v2 <- read_csv("C:/Users/772jo/Desktop/r_moje/music_cleaned_v3.csv")

music_trends_v2 <- music_data_v2 %>%
  filter(release_year >=1970) %>%
  group_by(release_year) %>%
  summarise(avg_energy = mean(energy, na.rm = TRUE))

energy_plot <- ggplot(data = music_trends_v2, aes(x = release_year, y = avg_energy)) +
  geom_smooth(method = "loess", color = "blue", se = FALSE, linewidth = 1.5) +
  geom_area(fill = "green", alpha = 0.15) +
  coord_cartesian(ylim = c(0.45, 0.8)) +
  geom_line(color = "green", linewidth = 2 ) +
  geom_point(color = "black", size = 2) +         
  theme_minimal() +
  labs(
   title = "How energetic top hits were throughout the decades",
   x = "Year",
   y = "mean energy"
   ) +
  theme(
    plot.background = element_rect(fill = alpha("black", 0.75), color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    axis.text = element_text(color = "gray80"),
    axis.title = element_text(color = "white", face = "bold"),
    plot.title = element_text(color = "white", face = "bold", size = 16),
    legend.background = element_rect(fill = "transparent"), 
    legend.title = element_text(color = "white", face = "bold"),
    legend.text = element_text(color = "white"), 
    panel.grid.major = element_line(color = "gray40"),
    panel.grid.minor = element_blank()
  )


#i noticed a trend reverse in the early 2010s. my hypothesis is that
# the more popular hip hop songs took over the scene with less energetic themes
genre_analysis <- music_data_v2 %>%
  filter(release_year > 2003) %>%
  mutate(
   mainstream = case_when(
     str_detect(genres, "rap|hip-hop|trap|drill") ~ "Rap / Hip-Hop",
     str_detect(genres, "pop") ~ "Pop",
     TRUE ~ "Other") 
  ) %>%
  filter(mainstream != "Other")

genre_energy <- genre_analysis %>%
  group_by(release_year, mainstream) %>%
  summarise(mean_energy = mean(energy, na.rm = TRUE))


rap_pop_plot <- ggplot(data = genre_energy, aes(x = release_year, y = mean_energy, color = mainstream)) +
  geom_line(linewidth = 1.5) +
  geom_point(color = "white", size = 2) +
  scale_color_manual(values = c("Pop" = "green", "Rap / Hip-Hop" = "red")) +
  labs(
    title = "Is rap less energetic than pop?",
    subtitle = "Average energy of rap and pop in last 22 years",
    x = "Release year",
    y = "Average energy",
    color = "Genre:" 
  ) +
  theme(
    plot.background = element_rect(fill = alpha("black", 0.75), color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    axis.text = element_text(color = "gray80"),
    axis.title = element_text(color = "white", face = "bold"),
    plot.title = element_text(color = "white", face = "bold", size = 16),
    legend.background = element_rect(fill = "transparent"), 
    legend.title = element_text(color = "white", face = "bold"),
    legend.text = element_text(color = "white"), 
    panel.grid.major = element_line(color = "gray40"),
    panel.grid.minor = element_blank()
  )
#My initial hypothesis posited that the overall decline 
#in music energy was driven by a shift in dominant genres — specifically,
#pop being replaced by a presumably calmer rap. 
#However, the data completely refuted this assumption,
#revealing that rap is, on average, just as energetic as pop,
#and often even more so. 
#the actual driver behind the energy drop was a global, 
#industry-wide trend. Around 2010, artists across both 
#genres began producing significantly calmer and more melancholic music,
#taking for example adele, billie eilish, travis scott or drake, lead artists of both genres in 2010s
#that stylistic shift persisted until roughly 2020.

#last thing to do is to check if hip-hop in fact became more popular in 2010s

genre_count <- genre_analysis %>%
  group_by(release_year, mainstream) %>%
  summarize(song_count = n(), .groups = 'drop' )

genres_plot <- ggplot(data = genre_count, aes(x = release_year, y = song_count, fill = mainstream)) +
         geom_col(position = "dodge", alpha = 0.8) +
           scale_fill_manual(values = c("Pop" = "green", "Rap / Hip-Hop" = "red")) +
           labs(
            title = "Evolution of mainstream genres - Pop vs Rap",
            subtitle = "Amount of songs of each genre in the last 22 years",
            x = "Release year",
            y = "Amount of songs",
            fill = "Genre:"
            ) +
            theme(
                plot.background = element_rect(fill = alpha("black", 0.7)),
                panel.background = element_rect(fill = "transparent"),
                axis.text = element_text(color = "gray80"),
                axis.title = element_text(color = "white", face = "bold"),
                plot.title = element_text(color = "white", face = "bold", size = 16),
                legend.background = element_rect(fill = "transparent"), 
                legend.title = element_text(color = "white", face = "bold"),
                legend.text = element_text(color = "white"),
                panel.grid.major.y = element_line(color = "black"),
                panel.grid.major.x = element_blank(),
                panel.grid.minor = element_blank()
                )
#Data proved my initial hypothesis - average count of hit hip hop songs increased in 2010s signifincantly,
#Whats more interesting, in that time period, there is a negative correlation between 
#these genres in 2010s - the more hit hip-hop songs came out, the less hit pop songs appeared.
#This suggests a fierce competition for mainstream spotlight - whenever 
#general audience was shifting towards rap it was at the cost of pop songs




music_trends_duration_v2 <- music_data_v2 %>%
  filter(release_year >=1970) %>%
  group_by(release_year) %>%
  summarise(avg_duration = mean(track_duration_s, na.rm = TRUE))

time_plot <- ggplot(data = music_trends_duration_v2, aes(x = release_year, y = avg_duration)) +
  geom_line(color = "blue", linewidth = 2 ) +
  geom_area(fill = "blue", alpha = 0.15) +
  coord_cartesian(ylim = c(175, 275)) +
  geom_point(color = "darkblue", size = 2) +
  labs(
  title = "Top hits` duration throughout the decades",
  x = "Year",
  y = "average song duration"
  ) +
  theme(
    plot.background = element_rect(fill = alpha("black", 0.7)),
    panel.background = element_rect(fill = "transparent"),
    axis.text = element_text(color = "gray80"),
    axis.title = element_text(color = "white", face = "bold"),
    plot.title = element_text(color = "white", face = "bold", size = 16),
    legend.background = element_rect(fill = "transparent"), 
    legend.title = element_text(color = "white", face = "bold"),
    legend.text = element_text(color = "white"),
    panel.grid.major.y = element_line(color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
 

(energy_plot + rap_pop_plot) / (genres_plot + time_plot)


ggsave("music_trends_v2.png", width = 16, height = 9, dpi = 300)