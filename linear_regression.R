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

library(caret)
# metrics for mod2
pred2_train <- predict(mod2, newdata = songs_train)
pred2_test <- predict(mod2, newdata = songs_test)

(train_rmse_2 <- RMSE(pred2_train, songs_train$ViewsM))
(test_rmse_2 <- RMSE(pred2_test, songs_test$ViewsM))


# metrics for mod1new
pred_train <- predict(mod1new, newdata = songs_train)
pred_test <- predict(mod1new, newdata = songs_test)

train_rmse <- RMSE(pred_train, songs_train$logViewsM)
test_rmse <- RMSE(pred_test, songs_test$logViewsM)

train_rmse
test_rmse

exp(train_rmse)
exp(test_rmse)

