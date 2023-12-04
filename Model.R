# Load in the cleaning data
source("Data Cleaning.R")

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
  size = 250
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
rf_conf_mat <- 
  confusionMatrix(table(rf_batters_predictions$.pred_class, 
                        batters_testing$inducted), positive = "1")
# Players whose classifications were incorrect
rf_incorrect <-
  rf_batters_predictions %>% 
  filter(.pred_class != `batters_testing$inducted`)
# Stats for players with incorrect predictions
hof_batting_stats %>% 
  semi_join(rf_incorrect, by = "playerID")

boost_batters_fit %>% 
  predict(active_batting_stats)

# Gradient Boosted model for Hall of Fame Batters
boost_batters_model <- 
  boost_tree(
    trees = tune(),
    mtry = tune(),
    min_n = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune(),
    sample_size = c(1),
    stop_iter = c(5)
  ) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

# Boosted tree for batters recipe
boost_batters_recipe <- 
  recipe(inducted ~ ., data = batters_training) %>%
  step_rm(playerID, GS, InnOuts, SH, SF, GIDP) %>% 
  step_impute_bag(all_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal(), -all_outcomes())
# Boosted tree workflow for Hall of Fame batters
boost_batters_wf <- 
  workflow() %>% 
  add_recipe(boost_batters_recipe) %>% 
  add_model(boost_batters_model)

set.seed(1)
# 10 Fold cross fold validation
boost_folds <- vfold_cv(batters_training, v = 10)

boost_grid <- grid_latin_hypercube(
  trees(range = c(25, 150)),
  mtry(range = c(5, 25)),
  min_n(range = c(5, 15)),
  tree_depth(range = c(5, 15)),
  learn_rate(),
  loss_reduction(),
  size = 250
)

registerDoParallel(cores = detectCores())

boost_tune_res <- tune_grid(
  boost_batters_wf,
  resamples = boost_folds,
  grid = boost_grid,
  control = control_grid(save_pred = TRUE),
  metrics = metric_set(roc_auc, accuracy)
)

boost_batters_model <- 
  finalize_model(boost_batters_model, select_best(boost_tune_res, "accuracy")[, 1:6])

boost_batters_wf <- 
  workflow() %>% 
  add_recipe(boost_batters_recipe) %>% 
  add_model(boost_batters_model)

boost_batters_crossval <- 
  boost_batters_wf %>% 
  fit_resamples(resamples = boost_folds,
                metrics = metric_set(roc_auc, accuracy))

boost_batters_crossval %>% 
  collect_metrics()

set.seed(1)
boost_batters_fit <- 
  boost_batters_wf %>% 
  fit(data = batters_training)
# Random Forest predictions for Hall of Fame Batters
boost_batters_predictions <- 
  boost_batters_fit %>% 
  predict(batters_testing) %>% 
  cbind(batters_testing$inducted, batters_testing %>% select(playerID))
# Accuracy of the Random Forest Predictions
boost_conf_mat <- 
  confusionMatrix(table(boost_batters_predictions$.pred_class, 
                        batters_testing$inducted), positive = "1")

boost_incorrect <-
  boost_batters_predictions %>% 
  filter(.pred_class != `batters_testing$inducted`)

hof_batting_stats %>% 
  semi_join(boost_incorrect, by = "playerID")
# Boosted model Active player hall of fame predictions
boost_batters_fit %>% 
  predict(active_batting_stats) %>% 
  cbind(active_batting_stats %>% select(playerID))
