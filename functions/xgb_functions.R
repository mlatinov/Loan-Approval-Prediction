

#### XGB functions #####
xgb_function <- function(data_training,
                         data_testing,
                         data_validation,
                         xgb_aov_size = 30,
                         grid_resolution = 50,
                         mtry_lower_fct = 0.5,
                         mtry_upper_fct = 1.5,
                         trees_lower_fct = 0.5,
                         trees_upper_fct = 1.5,
                         min_n_lower_fct = 0.5,
                         min_n_upper_fct = 1.5,
                         tree_depth_lower_fct = 0.5,
                         tree_depth_upper_fct = 1.5
                         ) {
  
  # Recipe
  recipe_result <- gb_recipe_rose_boruta_mrmr(data = data_training)
  recipe_xgb <- recipe_result$recipe
  data_preproc <- recipe_result$data

  # Message
  message("Recipe Passed...")
  
  # Model 
  xgb_model <- boost_tree(
    mtry = tune(),       # Randomly Selected Predictors
    trees = tune(),      # Trees
    min_n = tune(),      # Minimal Node Size
    tree_depth = tune()) %>% # Tree Depth
    set_engine("xgboost") %>%
    set_mode("classification")
  
  # Workflow 
  xgb_workflow <- workflow() %>%
    add_model(xgb_model) %>%
    add_recipe(recipe_xgb)
  
  # Resamples
  resamples <- vfold_cv(data = data_validation,v = 3)
  
  # Params 
  param_xgb <- parameters(
    mtry(range = c(1, 20)),         
    trees(range = c(100, 1000)),                    # More practical trees range
    min_n(range = c(1, 20)),                        # More common min_n range
    tree_depth(range = c(3, 15))                    # Balanced tree depth
  )
  
  # Message
  message("Tune Race Start...")
  
  # Light Tune Tune Race Anova
  xgb_aov <- tune_race_aov(
    workflow = xgb_workflow,
    burn_in = 2,
    resamples = resamples,
    size = xgb_aov_size,
    params = param_xgb
    )
  
  # Message
  message("Tune Race Ends....")
  
  # PSO 
  # Message
  message("PSO Start")
  
  # Optimize the param space
  xgb_optim_range <-  xgb_optim_range_maximinESE_LHS(
    best_params = xgb_aov$best_model_params,
    grid_resolution = grid_resolution,
    mtry_lower_fct = mtry_lower_fct,
    mtry_upper_fct = mtry_upper_fct,
    trees_lower_fct = trees_lower_fct,
    trees_upper_fct = trees_upper_fct,
    min_n_lower_fct = min_n_lower_fct,
    min_n_upper_fct = min_n_upper_fct,
    tree_depth_lower_fct = tree_depth_lower_fct,
    tree_depth_upper_fct = tree_depth_upper_fct
  )
  
  # Message 
  message("PSO End")
  
  # Message 
  message("MBO Start")
  
  # Warm Started MBO 
  mbo <- mbo_function(
    workflow = xgb_workflow,
    resamples = resamples,
    no_improve = 20,
    param_info = xgb_optim_range$param_info,
    design = xgb_optim_range$final_design
  )
  # Message
  message("MBO End")
  
  # Message
  message("Collect params...")
  
  # Collect the best params
  mbo_best_params <- mbo$best_mbo_params
  mbo_results <- mbo$mbo_results
  mbo_store <- mbo$mbo_store
  
  # Message
  message("Finalize Workflow ...")
  
  # Finalize workflow  
  xgb_workflow <- finalize_workflow(xgb_workflow, mbo_best_params)
  
  # Message
  message("Fit the model ...")
  
  # Fit the model 
  xgb_fit <- fit(xgb_workflow, data = data_training)
  
  # Message
  message("Predictions...")
  
  # Predict class probabilities on test data
  prob_preds <- predict(xgb_fit, data_testing, type = "prob")
  
  # Compute AUC on the test data
  roc_data <- data_testing %>%
    select(loan_status) %>% 
    bind_cols(prob_preds)
  
  auc_test_data <- roc_auc(
    roc_data,
    truth = loan_status,
    .pred_0,
    event_level = "first"  
  )
  
  # Return results 
  return(list(
    mbo = mbo,
    mbo_store = mbo_store,
    auc_test_data = auc_test_data,
    mbo_results = mbo_results,
    xgb_fit = xgb_fit,
    predictions_test_data = prob_preds,
    mbo_best_params = mbo_best_params,
    preproc_data = data_preproc
  ))
  
}