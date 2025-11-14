# Packages
install.packages(c("tidyverse","caret","randomForest","Metrics"))
library(tidyverse)
library(tidyr)
library(caret)
library(randomForest)
library(Metrics)
library(dplyr)


# Prepare data: use merged_df, filter complete cases and keep 1990+
df <- merged_df %>% filter(Year >= 1990) %>%
  select(Country, Year, Yield, Temperature, Rainfall) %>%
  drop_na()

# scale numeric predictors for models that benefit from scaling
df <- df %>% mutate(
  Temperature_s = scale(Temperature),
  Rainfall_s = scale(Rainfall)
)

# Train/Test split (time-blocked or random)
set.seed(42)
train_index <- createDataPartition(df$Yield, p = 0.75, list = FALSE)
train <- df[train_index, ]
test  <- df[-train_index, ]

# Linear regression model
lm_model <- lm(Yield ~ Temperature + Rainfall, data = train)
summary(lm_model)

# Predict & evaluate on test
pred_lm <- predict(lm_model, newdata = test)
rmse_lm <- rmse(test$Yield, pred_lm)
r2_lm   <- cor(test$Yield, pred_lm)^2

