# Load in the cleaning data
source("ProjectDataCleaning.R")

# Add necessary packages
library(tidyverse) 
library(tidymodels)
library(randomForest)
library(xgboost)
library(doParallel)
library(caret)

# Split the Hall of Fame data for batters into a training and testing
# set
set.seed(1)
data_split <- initial_split(hof_batting_stats, prop = 0.70,
                            strata = POS)
batters_training <- training(data_split)
batters_testing <- testing(data_split)

# Random Forest Model for Hall of Fame batters
rf_batters_model <- 
  rand_forest(trees = tune(),
              mtry = tune(),
              min_n = tune()) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")
# Random Forest recipe for Hall of Fame Batters
rf_batters_recipe <- 
  recipe(inducted ~ ., data = batters_training) %>%
  step_rm(playerID, GS, InnOuts, SH, SF, GIDP, GS, InnOuts) %>% 
  step_impute_bag(all_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal(), -all_outcomes())
# Random Forest workflow for Hall of Fame batters
rf_batters_wf <- 
  workflow() %>% 
  add_recipe(rf_batters_recipe) %>% 
  add_model(rf_batters_model)

set.seed(1)
# 10 Fold cross fold validation
rf_folds <- vfold_cv(batters_training, v = 10)

rf_grid <- grid_latin_hypercube(
  trees(range = c(25, 150)),
  mtry(range = c(5, 25)),
  min_n(range = c(5, 15)),
  size = 100
)

registerDoParallel(cores = detectCores())

rf_tune_res <- tune_grid(
  rf_batters_wf,
  resamples = rf_folds,
  grid = rf_grid,
  control = control_grid(save_pred = TRUE),
  metrics = metric_set(roc_auc, accuracy)
)

rf_batters_model <- 
  finalize_model(rf_batters_model, select_best(rf_tune_res, "accuracy")[, 1:3])

rf_batters_wf <- 
  workflow() %>% 
  add_recipe(rf_batters_recipe) %>% 
  add_model(rf_batters_model)

rf_batters_crossval <- 
  rf_batters_wf %>% 
  fit_resamples(resamples = rf_folds,
                metrics = metric_set(roc_auc, accuracy))

rf_batters_crossval %>% 
  collect_metrics()

set.seed(1)
rf_batters_fit <- 
  rf_batters_wf %>% 
  fit(data = batters_training)
# Random Forest predictions for Hall of Fame Batters
rf_batters_predictions <- 
  rf_batters_fit %>% 
  predict(batters_testing) %>% 
  cbind(batters_testing$inducted, batters_testing %>% select(playerID))

# Accuracy of the Random Forest Predictions
confusionMatrix(table(rf_batters_predictions$.pred_class, batters_testing$inducted))



## Pitchers

# Split data 
set.seed(1)
data_split <- initial_split(hof_pitching_stats, prop = 0.70,
                            strata = POS)
pitchers_training <- training(data_split)
pitchers_testing <- testing(data_split)

# Random Forest Model for Hall of Fame pitchers
rf_pitchers_model <- 
  rand_forest(trees = tune(),
              mtry = tune(),
              min_n = tune()) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")
# Random Forest recipe for Hall of Fame pitchers
rf_pitchers_recipe <- 
  recipe(inducted ~ ., data = pitchers_training) %>%
  step_rm(playerID) %>% 
## issue with imputing with bagging: problem with cbind() in imputing method
  step_impute_knn(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal(), -all_outcomes())
# Random Forest workflow for Hall of Fame pitchers
rf_pitchers_wf <- 
  workflow() %>% 
  add_recipe(rf_pitchers_recipe) %>% 
  add_model(rf_pitchers_model)

set.seed(1)
# 10 Fold cross fold validation
rf_folds <- vfold_cv(pitchers_training, v = 10)

rf_grid <- grid_latin_hypercube(
  trees(range = c(25, 150)),
  mtry(range = c(5, 25)),
  min_n(range = c(5, 15)),
  size = 100
)

registerDoParallel(cores = detectCores())

rf_tune_res <- tune_grid(
  rf_pitchers_wf,
  resamples = rf_folds,
  grid = rf_grid,
  control = control_grid(save_pred = TRUE),
  metrics = metric_set(roc_auc, accuracy)
)

rf_pitchers_model <- 
  finalize_model(rf_pitchers_model, select_best(rf_tune_res, "accuracy")[, 1:3])

rf_pitchers_wf <- 
  workflow() %>% 
  add_recipe(rf_pitchers_recipe) %>% 
  add_model(rf_pitchers_model)

rf_pitchers_crossval <- 
  rf_pitchers_wf %>% 
  fit_resamples(resamples = rf_folds,
                metrics = metric_set(roc_auc, accuracy))

rf_pitchers_crossval %>% 
  collect_metrics()

# Final fit
set.seed(1)
rf_pitchers_fit <- 
  rf_pitchers_wf %>% 
  fit(data = pitchers_training)

#RF predictions with testing set
rf_pitchers_prediction <- 
  rf_pitchers_fit %>% 
  predict(pitchers_testing) %>% 
  cbind(pitchers_testing %>% select(playerID))

# Accuracy of the Random Forest Predictions
confusionMatrix(table(rf_pitchers_prediction$.pred_class, pitchers_testing$inducted))

## Prediction of current player to make Hall of Fame
active_pitchers_prediction <-
  rf_pitchers_fit %>% 
  predict(active_pitching_stats) %>% 
  cbind(active_pitching_stats %>% select(playerID))

active_pitchers_prediction %>%
  filter(.pred_class == 1) %>%
  select(playerID) %>% 
  inner_join(People, by = "playerID") %>%
  select(nameFirst, nameLast)
