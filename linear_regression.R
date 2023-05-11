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

# train linear model ORIGINAL
mod1 <- lm(ViewsM ~ Danceability + Energy + Key + Loudness + Speechiness + 
               Acousticness + Instrumentalness + Valence + Tempo + Duration_ms + 
               Licensed + official_video + Stream,
             songs_train)

summary(mod1)

## POST PRESENTATION EDITS
# train linear model POST PRESENTATION
mod1new <- lm(logViewsM ~ Danceability + Energy + Key + Loudness + Speechiness + 
                Acousticness + Instrumentalness + Liveness + Valence + Tempo + logDuration + 
                Licensed + official_video + topArtist,
              songs_train)

summary(mod1new)

# train new model with only ** and *** significant variables ORIGINAL
mod2 <- lm(ViewsM ~ Danceability + Loudness + Acousticness + Valence + Duration_ms + Licensed + Stream,
           songs_train)

summary(mod2)

# edited linear regression model POST PRESENTATION
mod2new <- lm(logViewsM ~ Danceability + Energy + Loudness + Speechiness + 
                Instrumentalness + Tempo + logDuration + Licensed + official_video + topArtist,
              songs_train)

summary(mod2new)

# THE FIRST MODEL (mod1new) PERFORMED BETTER SO GO WITH THAT ONE
