# import libraries
library(tidyverse)

# import data
songs <- read.csv("/Users/Amanda/Library/CloudStorage/OneDrive-ChapmanUniversity/junior/junior spring/mgsc 310/datasets/Spotify_Youtube.csv")

# data cleaning and exploring
songs %>% glimpse()
songs %>% is.na %>% sum()

# drop missing values
songs_clean <- songs %>% drop_na() # drop 1169 rows with missing values
songs_clean %>% is.na %>% sum()
songs_clean %>% dim() # still have 19549 observations

# drop variables that are irrelevant to our model
songs_clean <- songs_clean %>% select(-c(Description, Url_spotify, Uri))

# factor binary variables
songs_clean <- songs_clean %>% mutate(Artist = factor(Artist),
                                      Licensed = as.logical(Licensed),
                                      official_video = as.logical(official_video),
                                      ViewsM = Views/1000000)

songs_clean %>% glimpse()
songs_clean %>% summary()

# make dummy variable for top10 artists
topArtist <- songs_clean %>% 
  group_by(Artist) %>% 
  summarize(ViewsM = n()) %>% 
  top_frac(0.1) %>% 
  mutate(topArtist = 1) %>% 
  select(-ViewsM)

songs_clean <- songs_clean %>% 
  left_join(topArtist, by = "Artist") %>% 
  mutate(topArtist = replace_na(topArtist, 0))

songs_clean <- songs_clean %>% 
  mutate(logDuration = log(Duration_ms),
         logViewsM = log(ViewsM))

songs_clean %>% glimpse()

write.csv(songs_clean, "/Users/Amanda/Library/CloudStorage/OneDrive-ChapmanUniversity/junior/junior spring/mgsc 310/datasets/songs_clean.csv", row.names=FALSE)

# RANDOM METRICS FOR WRITE UP 
# portion of observations that have less than 1 billion views
sum(songs_clean$ViewsM < 1000) / sum(songs_clean$ViewsM > 0)

# effect of official_video on Views
songs_clean %>% filter(official_video == TRUE) %>% summarize(mean(ViewsM))
songs_clean %>% filter(official_video == FALSE) %>% summarize(mean(ViewsM))

# portion of observations where official_video == TRUE
mean(songs_clean$official_video)
# correlation matrix of variables
library(ggcorrplot)

# correlation matrix
cor_table <- cor(songs_clean %>% select_if(is.numeric))
cor_table

ggcorrplot(cor_table)

# data visualization

ggplot(songs_clean, aes(x = Danceability)) + geom_histogram(fill = "#9a0331ff") + 
  theme_minimal() + 
  labs(y = "Observations", title = "Danceability")

ggplot(songs_clean, aes(x = Loudness)) + geom_histogram(fill = "#9a0331ff") + 
  theme_minimal() + 
  labs(y = "Observations", title = "Loudness")

ggplot(songs_clean, aes(x = Energy)) + geom_histogram(fill = "#9a0331ff") + 
  theme_minimal() + 
  labs(y = "Observations", title = "Energy")

ggplot(songs_clean, aes(x = Speechiness)) + geom_histogram(fill = "#9a0331ff") + 
  theme_minimal() + 
  labs(y = "Observations", title = "Speechiness")

ggplot(songs_clean, aes(x = Acousticness)) + geom_histogram(fill = "#9a0331ff") + 
  theme_minimal() + 
  labs(y = "Observations", title = "Acousticness")

ggplot(songs_clean, aes(x = Duration_ms)) + geom_histogram(fill = "#9a0331ff") + 
  theme_minimal() + 
  labs(y = "Observations", x = "Duration (milliseconds)", title = "Duration (milliseconds)")

ggplot(songs_clean, aes(x = Valence)) + geom_histogram(fill = "#9a0331ff") + 
  theme_minimal() + 
  labs(y = "Observations", title = "Valence")

ggplot(songs_clean, aes(x = Liveness)) + geom_histogram(fill = "#9a0331ff") + 
  theme_minimal() + 
  labs(y = "Observations", title = "Liveness")

ggplot(songs_clean, aes(x = Stream)) + geom_histogram(fill = "#9a0331ff") + 
  theme_minimal() + 
  labs(y = "Observations", title = "Streams")

ggplot(songs_clean, aes(x = Key)) + geom_histogram(fill = "#9a0331ff") + 
  theme_minimal() + 
  labs(y = "Observations", title = "Key")


ggplot(songs_clean, aes(x = official_video, y = ViewsM)) + geom_boxplot(fill = "#9a0331ff") + 
  theme_minimal() + 
  labs(y = "Views (Millions)", x = "Official Video", title = "Official Video")

# distribution of views and likes
ggplot(songs_clean, aes(x = ViewsM)) + 
  geom_histogram(bins = 20, fill = "#9a0331ff", color = "#9a0331ff") + 
  theme_minimal() + 
  labs(x = "Views (Millions)", y = "Observations", title = "Distribution of Views")

# me trying to figure out what the outlier is... it's despacito
songs_popular <- songs_clean %>% arrange(-Views) %>% slice(1:50)
summary(songs_popular)


