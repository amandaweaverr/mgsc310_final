library(rsample)
library(MASS)
library(tidyverse)
library(ISLR)
library(ggplot2)
library(ISLR2)
library(forcats)
library(caret)
library(coefplot)

songs_clean <- read.csv("Data Sets/songs_clean.csv") #change file path 

songs_clean %>%  glimpse()
# splitting the data set
set.seed(310)
songs_split <- initial_split(songs_clean, prop = 0.8)
songs_train <- training(songs_split)
songs_test <- testing(songs_split)

#creating the model
mod1 <- lm(ViewsM ~ Danceability + Energy + Key + Loudness + Speechiness + 
             Acousticness + Instrumentalness + Liveness + Valence + Tempo + Duration_ms + 
             Licensed + official_video + Stream,
           songs_train)

summary(mod1)

## Significant at a 0.001 level: Danceability Loudness LicensedTRUE
## At 0.01 level: Energy
## At 0.05 level: Speechiness official_videoTRUE
## Not significant: Key Acousticness Instrumentalness Liveness valence Tempo Duration_ms

# train new model with only significant variables
mod2 <- lm(ViewsM ~ Danceability + Loudness + Acousticness + Valence + Duration_ms + Licensed + Stream,
           songs_train)

summary(mod2)

#model predictions and accuracy ; additions to model code

#mod2
mod2_preds_train <- predict(mod2, newdata = songs_train)
mod2_preds_test <- predict(mod2, newdata = songs_test)


mod2_preds_train_results <- songs_train %>% mutate(views_pred = mod2_preds_train)
mod2_preds_test_results <- songs_test %>% mutate(views_pred = mod2_preds_test)


mod2_preds_train %>%  head()
mod2_preds_test %>% head()


mod2_train_rmse <- RMSE(mod2_preds_train, songs_train$ViewsM)
mod2_test_rmse <- RMSE(mod2_preds_test, songs_test$ViewsM)

mod2_train_rmse
mod2_test_rmse

'''
Creates dataframe and plots real vs predicted values, unused

train_residual_df <- data.frame(
  real = songs_train$ViewsM,
  predicted = mod2_preds_train)
)

ggplot(train_residual_df, aes(x = real, y = predicted)) +
  geom_point(alpha = 0.1) +
  theme_minimal()
'''

#creating RMSE data frame for mod2 
rmse_df <- data.frame(
  scores = c(mod2_train_rmse, mod2_test_rmse)
)

rmse_df %>% glimpse()

ggplot(rmse_df, aes(x = scores, y = scores, fill = "#9a0331ff")) + 
  geom_bar(stat = "identity")+ # sets height of bars equal to their value
  theme_minimal() +
  labs(x = 'Train & Test sets', y ='RMSE Value (millions)', title = 'Comparison of RMSE for Mod2') +
  geom_text(aes(label= scores), vjust=0) # creates value labels 
