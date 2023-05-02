# linear regression with 80/20 train test split

# import libraries
library(tidyverse)
library(rsample)

# import cleaned dataset
songs_clean <- read.csv("/Users/Amanda/Library/CloudStorage/OneDrive-ChapmanUniversity/junior/junior spring/mgsc 310/datasets/songs_clean.csv")

songs_clean %>% glimpse()

# splitting the data set
set.seed(310)
songs_split <- initial_split(songs_clean, prop = 0.8)
songs_train <- training(songs_split)
songs_test <- testing(songs_split)

dim(songs_split)

# train linear model
mod1 <- lm(Views ~ Danceability + Energy + Key + Loudness + Speechiness + 
               Acousticness + Instrumentalness + Liveness + Valence + Tempo + Duration_ms + 
               Likes + Licensed + official_video,
             songs_train)

summary(mod1)

## Significant at a 0.001 level: Speechiness, Valence, Duration_ms, Likes, LicensedTRUE
## At 0.01 level: None
## At 0.05 level: Danceability
## Not significant: Energy, Key, Acousticness, Instrumentalness, Liveness, Tempo, official_videoTRUE

# train new model with only significant variables
mod2 <- lm(Views ~ Speechiness + Valence + Duration_ms + Likes + Licensed + Danceability,
           songs_train)

summary(mod2)
