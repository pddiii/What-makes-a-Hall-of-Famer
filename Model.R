# Load in the cleaning data
source("Data Cleaning.R")

# Add necessary packages
suppressPackageStartupMessages(library(tidyverse)) # For data cleaning purposes
suppressPackageStartupMessages(library(tidymodels)) # For model creation
suppressPackageStartupMessages(library(randomForest)) # For Random Forest
suppressPackageStartupMessages(library(xgboost)) # For boosted trees
suppressPackageStartupMessages(library(doParallel)) # For parallel processing
suppressPackageStartupMessages(library(caret)) # For diagnostics of models
suppressPackageStartupMessages(library(vip)) # For variable importance

### Batting

# Split the Hall of Fame data for batters into a training and testing
# set
set.seed(1)
data_split <- initial_split(fg_hof_batting, prop = 0.70,
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
  step_mutate(XBH = `2B` + `3B` + HR) %>% 
  step_rm(playerID, fg_playerID, GIDP, GS, InnOuts, PO, A, E, DP, PB, WP,
          G, AB, PA, `1B`, `2B`, `3B`, BB, IBB, SO, Name, Spd, CS, 
          HBP, SF, SH, GDP, R) %>% 
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
rf_batters_cv_metrics <- rf_batters_crossval %>% 
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
rf_batters_conf_mat <- 
  confusionMatrix(table(rf_batters_predictions$.pred_class, 
                        batters_testing$inducted), positive = "1")

# Players whose classifications were incorrect from testing data
rf_incorrect_batters <-
  rf_batters_predictions %>% 
  filter(.pred_class != `batters_testing$inducted`) %>% 
  rename(Inducted = .pred_class)

# Add the statistics, and names of batters who were classified incorrectly
rf_incorrect_batters <- fg_hof_batting %>% 
  semi_join(rf_incorrect_batters, by = "playerID") %>% 
  select(-playerID, -fg_playerID)

# Random Forest Active Batters Hall of Fame Predictions
rf_active_batters <- 
  rf_batters_fit %>% 
  predict(fg_active_batting) %>% 
  cbind(fg_active_batting %>% select(playerID))

# Active players predicted to be hall of famers in the future
rf_future_hof_batters <- 
  rf_active_batters %>% 
  filter(.pred_class == 1) %>% 
  rename(Inducted = .pred_class)

# Attaching the active players batting stats and names predicted to be Hall of
# Famers some day
rf_future_hof_batters <- fg_active_batting %>% 
  semi_join(rf_future_hof_batters, by = "playerID") %>% 
  select(-c(2:6)) %>% 
  relocate(c(WAR, AVG, all_star_appearances, H, wOBA), .before = GIDP) %>% 
  arrange(desc(WAR))

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
  step_mutate(XBH = `2B` + `3B` + HR) %>% 
  step_rm(playerID, fg_playerID, GIDP, GS, InnOuts, PO, A, E, DP, PB, WP,
          G, AB, PA, `1B`, `2B`, `3B`, BB, IBB, SO, Name, Spd, CS, 
          HBP, SF, SH, GDP, R) %>% 
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
boost_batters_cv_metrics <- 
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
boost_batters_conf_mat <- 
  confusionMatrix(table(boost_batters_predictions$.pred_class, 
                        batters_testing$inducted), positive = "1")

# Boost Incorrect Hall of Fame classification
boost_incorrect_batters <-
  boost_batters_predictions %>% 
  filter(.pred_class != `batters_testing$inducted`) %>% 
  rename(Inducted = .pred_class)

boost_incorrect_batters <- fg_hof_batting %>% 
  semi_join(boost_incorrect_batters, by = "playerID") %>% 
  select(-playerID, -fg_playerID)

# Boosted model Active player hall of fame predictions
boost_active_batters <- 
  boost_batters_fit %>% 
  predict(fg_active_batting) %>% 
  cbind(fg_active_batting %>% select(playerID))

# Boosted models prediction of future Hall of Fame batters
boost_future_hof_batters <- 
  boost_active_batters %>% 
  filter(.pred_class == 1) %>% 
  rename(Inducted = .pred_class)

# Attach the names and statistics to the predicted future hall of famers
boost_future_hof_batters <- fg_active_batting %>% 
  semi_join(boost_future_hof_batters, by = "playerID")

# Feature imporance for the Boosted Model
boost_batters_feature_imp <- 
  boost_batters_fit %>% 
  extract_fit_parsnip() %>% 
  vip(geom = "point") +
  labs(title = "Batter's Boosted Tree variable importance")

## Pitchers

###### Random Forest

# Split data 
set.seed(1)
data_split <- initial_split(fg_hof_pitching, prop = 0.70,
                            strata = POS)
pitchers_training <- training(data_split)
pitchers_testing <- testing(data_split)

# Random Forest Model for Hall of Fame pitchers
rf_pitchers_model <- 
  rand_forest(trees = tune(),
              mtry = tune(),
              min_n = tune()) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")
# Random Forest recipe for Hall of Fame pitchers
rf_pitchers_recipe <- 
  recipe(inducted ~ ., data = pitchers_training) %>%
  step_rm(Name, playerID, fg_playerID, PO, A, E, DP, G, GS, CG, ShO, SV, 
          IP, TBF, H, R, ER, HR, BB, IBB, HBP, WP, W, L, `300_wins`) %>%  
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
  size = 500
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

rf_pitchers_cv_metrics <- rf_pitchers_crossval %>% 
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
rf_pitchers_conf_mat <-
  confusionMatrix(table(rf_pitchers_prediction$.pred_class, 
                        pitchers_testing$inducted), positive = "1")

# Boost Incorrect Hall of Fame classification
rf_incorrect_pitchers <-
  rf_pitchers_prediction %>% 
  filter(.pred_class != pitchers_testing$inducted) %>% 
  rename(Inducted = .pred_class)

rf_incorrect_pitchers <- fg_hof_pitching %>% 
  semi_join(rf_incorrect_pitchers, by = "playerID")

## Prediction of current player to make Hall of Fame
active_pitchers_prediction <-
  rf_pitchers_fit %>% 
  predict(fg_active_pitching) %>% 
  cbind(fg_active_pitching %>% select(playerID))

rf_future_hof_pitchers <- active_pitchers_prediction %>% 
  filter(.pred_class == 1) %>% 
  rename(Inducted = .pred_class)

rf_future_hof_pitchers <- fg_active_pitching %>% 
  semi_join(rf_future_hof_pitchers, by = "playerID") %>% 
  relocate(c(WAR, WHIP, ERA, `HR/9`, SO), .before = play_era) %>% 
  arrange(desc(WAR)) %>% 
  select(-inducted, -play_era, -playerID, -fg_playerID)

rf_pitchers_feature_import <-
  rf_pitchers_fit %>%
  extract_fit_parsnip() %>%
  vip(geom = "point") + 
  labs(title = "Pitcher's Random forest variable importance") 


###### Boosted Tree

boost_pitchers_model <- 
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

# Boosted tree pitchers recipe
boost_pitchers_recipe <- 
  recipe(inducted ~ ., data = pitchers_training) %>%
  step_rm(Name, playerID, fg_playerID, PO, A, E, DP, G, GS, CG, ShO, SV, 
          IP, TBF, H, R, ER, HR, BB, IBB, HBP, WP, W, L, `300_wins`) %>%  
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal(), -all_outcomes())
# Boosted tree workflow Hall of Fame pitchers
boost_pitchers_wf <- 
  workflow() %>% 
  add_recipe(boost_pitchers_recipe) %>% 
  add_model(boost_pitchers_model)

set.seed(1)
# 10 Fold cross fold validation
boost_folds <- vfold_cv(pitchers_training, v = 10)

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

boost_tune_res <- tune_grid(
  boost_pitchers_wf,
  resamples = boost_folds,
  grid = boost_grid,
  control = control_grid(save_pred = TRUE),
  metrics = metric_set(roc_auc, accuracy)
)

boost_pitchers_model <- 
  finalize_model(boost_pitchers_model, select_best(boost_tune_res, "accuracy")[, 1:6])

boost_pitchers_wf <- 
  workflow() %>% 
  add_recipe(boost_pitchers_recipe) %>% 
  add_model(boost_pitchers_model)

boost_pitchers_crossval <- 
  boost_pitchers_wf %>% 
  fit_resamples(resamples = boost_folds,
                metrics = metric_set(roc_auc, accuracy))

boost_pitchers_cv_metrics <- boost_pitchers_crossval %>% 
  collect_metrics()

set.seed(1)
boost_pitchers_fit <- 
  boost_pitchers_wf %>% 
  fit(data = pitchers_training)
# Boosted Tree predictions for Hall of Fame Pitchers
boost_pitchers_predictions <- 
  boost_pitchers_fit %>% 
  predict(pitchers_testing) %>% 
  cbind(pitchers_testing$inducted, pitchers_testing %>% select(playerID))
# Accuracy of the Boosted Tree Predictions
boost_pitchers_conf_mat <- 
  confusionMatrix(table(boost_pitchers_predictions$.pred_class, 
                        pitchers_testing$inducted), positive = "1")

# Boosted model Active player hall of fame predictions
boost_active_pitchers_prediction <- 
  boost_pitchers_fit %>% 
  predict(fg_active_pitching) %>% 
  cbind(fg_active_pitching %>% select(playerID))

boost_future_hof_pitchers <- boost_active_pitchers_prediction %>% 
  filter(.pred_class == 1) %>% 
  rename(Inducted = .pred_class)

boost_future_hof_pitchers <- fg_active_pitching %>% 
  semi_join(boost_future_hof_pitchers, by = "playerID") %>% 
  relocate(c(WAR, WHIP, ERA, `HR/9`, SO), .before = play_era) %>% 
  arrange(desc(WAR)) %>% 
  select(-inducted, -play_era, -playerID, -fg_playerID)

# Boosted Tree Feature Importance
boost_pitchers_feature_import <- 
  boost_pitchers_fit %>% 
  extract_fit_parsnip() %>% 
  vip(geom = "point") +
  labs(title = "Pitcher's Boosted Tree variable importance")
