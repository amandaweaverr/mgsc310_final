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
                                      official_video = as.logical(official_video))

songs_clean %>% glimpse()
songs_clean %>% summary()

write.csv(songs_clean, "/Users/Amanda/Library/CloudStorage/OneDrive-ChapmanUniversity/junior/junior spring/mgsc 310/datasets/songs_clean.csv", row.names=FALSE)

# correlation matrix of variables
library(ggcorrplot)

cor_table <- cor(songs_clean %>% select_if(is.numeric))
cor_table

ggcorrplot(cor_table)

## strong positive correlation between Likes and Views, Comments, & Stream
## strong negative correlation between Acousticness and Energy & Loudness
## strong positive correlation between Energy and Loudness
## Views isn't strongly correlated to anything other tahn Likes, Comments, and Stream

# data visualization
# Danceability vs Views
ggplot(songs_clean, aes(x = Danceability, y = Views, color = Likes)) + 
  geom_point() + 
  labs(title = "Danceability vs Views") + 
  theme_minimal()

# Energy vs Views
ggplot(songs_clean, aes(x = Energy, y = Views, color = Likes)) + 
  geom_point() + 
  labs(title = "Energy vs Views") + 
  theme_minimal()

# distribution of views and likes
ggplot(songs_clean, aes(x = Views)) + 
  geom_histogram(bins = 20, fill = "light pink", color = "pink") + 
  theme_minimal()

ggplot(songs_clean, aes(x = Likes)) + 
  geom_histogram(bins = 20, fill = "light pink", color = "pink") + 
  theme_minimal()

# distribution of different predictor variables
ggplot(songs_clean, aes(x = Danceability)) + 
  geom_histogram(bins = 20, fill = "light pink", color = "pink") + 
  theme_minimal()

ggplot(songs_clean, aes(x = Loudness)) + 
  geom_histogram(bins = 20, fill = "light pink", color = "pink") + 
  theme_minimal()

ggplot(songs_clean, aes(x = Acousticness)) + 
  geom_histogram(bins = 20, fill = "light pink", color = "pink") + 
  theme_minimal()

ggplot(songs_clean, aes(x = Energy)) + 
  geom_histogram(bins = 20, fill = "light pink", color = "pink") + 
  theme_minimal()

# me trying to figure out what the outlier is... it's despacito
songs_popular <- songs_clean %>% arrange(-Views) %>% slice(1:50)
summary(songs_popular)


