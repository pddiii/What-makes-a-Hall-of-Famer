# Load in the cleaning data
source("Data Cleaning.R")

# Add necessary packages
library(tidyverse) # For data cleaning purposes
library(tidymodels) # For model creation
library(randomForest) # For Random Forest
library(xgboost) # For boosted trees
library(doParallel) # For parallel processing
library(caret) # For diagnostics of models
library(vip) # For variable importance

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
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")

# Random Forest recipe for Hall of Fame Batters
rf_batters_recipe <- 
  recipe(inducted ~ ., data = batters_training) %>%
  step_rm(playerID, GS, InnOuts, SH, SF, GIDP, GS, InnOuts, E) %>% 
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
  size = 500
)

registerDoParallel(cores = detectCores())

rf_tune_res <- tune_grid(
  rf_batters_wf,
  resamples = rf_folds,
  grid = rf_grid,
  control = control_grid(save_pred = TRUE),
  metrics = metric_set(roc_auc, accuracy)
)

# Update the Random Forest models with the ideal hypertuned parameters
rf_batters_model <- 
  finalize_model(rf_batters_model, select_best(rf_tune_res, "accuracy")[, 1:3])

# Update the workflow with new parameters
rf_batters_wf <- 
  workflow() %>% 
  add_recipe(rf_batters_recipe) %>% 
  add_model(rf_batters_model)

# Performing 10 fold cross validation
rf_batters_crossval <- 
  rf_batters_wf %>% 
  fit_resamples(resamples = rf_folds,
                metrics = metric_set(roc_auc, accuracy))

# Check the accuracy and roc_auc
rf_crossval_metrics <- rf_batters_crossval %>% 
  collect_metrics()

# Fit the workflow to the training data
set.seed(1)
rf_batters_fit <- 
  rf_batters_wf %>% 
  fit(data = batters_training)

# Random Forest predictions on the testing data 
rf_batters_predictions <- 
  rf_batters_fit %>% 
  predict(batters_testing) %>% 
  cbind(batters_testing$inducted, batters_testing %>% select(playerID))

# Accuracy of the Random Forest Predictions on testing data
rf_conf_mat <- 
  confusionMatrix(table(rf_batters_predictions$.pred_class, 
                        batters_testing$inducted), positive = "1")

# Players whose classifications were incorrect from testing data
rf_incorrect <-
  rf_batters_predictions %>% 
  filter(.pred_class != `batters_testing$inducted`) %>% 
  rename(Inducted = .pred_class)

# Add the statistics, and names of batters who were classified incorrectly
rf_incorrect <- hof_batting_stats %>% 
  semi_join(rf_incorrect, by = "playerID") %>% 
  left_join(People %>% select(playerID, nameFirst, nameLast), by = "playerID") %>% 
  relocate(c(nameFirst, nameLast), .after = playerID)

# Random Forest Active Batters Hall of Fame Predictions
rf_active_batters <- 
  rf_batters_fit %>% 
  predict(active_batting_stats) %>% 
  cbind(active_batting_stats %>% select(playerID))

# Active players predicted to be hall of famers in the future
rf_future_hof <- 
  rf_active_batters %>% 
  filter(.pred_class == 1) %>% 
  rename(Inducted = .pred_class)

# Attaching the active players batting stats and names predicted to be Hall of
# Famers some day
rf_future_hof <- active_batting_stats %>% 
  semi_join(rf_future_hof, by = "playerID") %>% 
  left_join(People %>% select(playerID, nameFirst, nameLast), by = "playerID") %>% 
  relocate(c(nameFirst, nameLast), .after = playerID)

# Feature importance for Random Forest Model
rf_batters_feature_imp <- 
  rf_batters_fit %>% 
  extract_fit_parsnip() %>% 
  vip(geom = "point") +
  labs(title = "Batter's Random Forest variable importance")

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
  step_rm(playerID, GS, InnOuts, SH, SF, GIDP, E) %>% 
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

# Boosted tuning grid
boost_grid <- grid_latin_hypercube(
  trees(range = c(25, 150)),
  mtry(range = c(5, 25)),
  min_n(range = c(5, 15)),
  tree_depth(range = c(5, 15)),
  learn_rate(),
  loss_reduction(),
  size = 500
)

registerDoParallel(cores = detectCores())

# Results of the boost tuning
boost_tune_res <- tune_grid(
  boost_batters_wf,
  resamples = boost_folds,
  grid = boost_grid,
  control = control_grid(save_pred = TRUE),
  metrics = metric_set(roc_auc, accuracy)
)

# Update the boosted batters model
boost_batters_model <- 
  finalize_model(boost_batters_model, 
                 select_best(boost_tune_res, "accuracy")[, 1:6])

# Finalize the boosted batters workflow
boost_batters_wf <- 
  workflow() %>% 
  add_recipe(boost_batters_recipe) %>% 
  add_model(boost_batters_model)

# Perform 10-fold cross validation on batters
boost_batters_crossval <- 
  boost_batters_wf %>% 
  fit_resamples(resamples = boost_folds,
                metrics = metric_set(roc_auc, accuracy))

# Collect the cross validation metrics for boost model
boost_crossval_metrics <- 
  boost_batters_crossval %>% 
  collect_metrics()

# Fit boosted model to the training data
set.seed(1)
boost_batters_fit <- 
  boost_batters_wf %>% 
  fit(data = batters_training)

# Boost predictions for Hall of Fame Batters
boost_batters_predictions <- 
  boost_batters_fit %>% 
  predict(batters_testing) %>% 
  cbind(batters_testing$inducted, batters_testing %>% select(playerID))

# Confusion Matrix of the Boost Predictions
boost_conf_mat <- 
  confusionMatrix(table(boost_batters_predictions$.pred_class, 
                        batters_testing$inducted), positive = "1")

# Boost Incorrect Hall of Fame classification
boost_incorrect <-
  boost_batters_predictions %>% 
  filter(.pred_class != `batters_testing$inducted`) %>% 
  rename(Inducted = .pred_class)

boost_incorrect <- hof_batting_stats %>% 
  semi_join(boost_incorrect, by = "playerID") %>% 
  left_join(People %>% select(playerID, nameFirst, nameLast), by = "playerID") %>% 
  relocate(c(nameFirst, nameLast), .after = playerID)

# Boosted model Active player hall of fame predictions
boost_active_batters <- 
  boost_batters_fit %>% 
  predict(active_batting_stats) %>% 
  cbind(active_batting_stats %>% select(playerID))

# Boosted models prediction of future Hall of Fame batters
boost_future_hof <- 
  boost_active_batters %>% 
  filter(.pred_class == 1) %>% 
  rename(Inducted = .pred_class)

# Attach the names and statistics to the predicted future hall of famers
boost_future_hof <- active_batting_stats %>% 
  semi_join(boost_future_hof, by = "playerID") %>% 
  left_join(People %>% select(playerID, nameFirst, nameLast), by = "playerID") %>% 
  relocate(c(nameFirst, nameLast), .after = playerID)

# Feature imporance for the Boosted Model
boost_batters_feature_imp <- 
  boost_batters_fit %>% 
  extract_fit_parsnip() %>% 
  vip(geom = "point") +
  labs(title = "Batter's Random Forest variable importance")