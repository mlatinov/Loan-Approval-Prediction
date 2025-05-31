

# Random Forest ---------------------

random_forest_func <- function(data_train,
                               data_test,
                               data_validation,
                               aov_size = 40,
                               pso_mtry_lower_fct = 0.9,
                               pso_mtry_up_fct = 1.1,
                               pso_min_n_lower_fct = 0.9,
                               pso_min_n_up_fct = 1.1,
                               pso_trees_lower_fct = 0.5,
                               pso_trees_up_dct = 1.5) {
  
  # Preprocessing recipe 
  recipe_result <- recipe_tree_smotenc_infgain(data = data_train)
  preproc_data <- recipe_result$data
  recipe <- recipe_result$recipe
  
  # Message 
  message("Recipe pass")
  
  # Model 
  model <- rand_forest(
    mtry = tune(),
    trees = tune(),
    min_n = tune()
  ) %>%
    set_mode("classification") %>%
    set_engine("ranger")
  
  # Workflow
  workflow <- workflow() %>%
    add_model(model) %>%
    add_recipe(recipe)
  
  # Resamples 
  resamples <- vfold_cv(data = data_validation, v = 5)
  
  # Light tune params range
  params <- parameters(list(
    mtry(range = c(5, 10)),
    min_n(range = c(2, 60)),
    trees(range = c(100, 500))
  ))
  
  # Message
  message("Tune Race Start")
  
  # Light tune 
  light_tune <- tune_race_aov(
    workflow = workflow,
    burn_in = 3,
    resamples = resamples,
    size = aov_size,
    params = params
  )
  # Message
  message("Tune Race End")
  
  # Message
  message("PSO Start")
  
  # Parameter space optimization
  pso <- rf_op_range_min_max_sa(
    best_param = light_tune$best_model_params,
    mtry_lower_fct = pso_mtry_lower_fct,
    mtry_up_fct = pso_mtry_up_fct,
    min_n_lower_fct = pso_min_n_lower_fct,
    min_n_up_fct = pso_min_n_up_fct,
    trees_lower_fct = pso_trees_lower_fct,
    trees_up_dct = pso_trees_up_dct
  )
  
  # Message 
  message("PSO End")
  
  # Message 
  message("MBO Start")
  
  # MBO
  mbo <- mbo_function(
    workflow = workflow,
    resamples = resamples,
    no_improve = 20,
    param_info = pso$param_info,
    design = pso$final_design
  )
  
  # Message
  message("MBO End")
  
  # Message
  message("Collect params...")
  
  # Collect the best params
  mbo_best_params <- mbo$best_mbo_params
  mbo_results <- mbo$mbo_results
  
  # Message
  message("Finalize Workflow ...")
  
  # Finalize workflow  
  random_forest_workflow <- finalize_workflow(workflow, mbo_best_params)
  
  # Message
  message("Fit the model ...")
  
  # Fit the model 
  random_forest_fit <- fit(random_forest_workflow, data = data_train)
  
  # Message
  message("Predictions...")
  
  # Predict class probabilities on test data
  prob_preds <- predict(random_forest_fit, data_test, type = "prob")
  
  # Compute AUC on the test data
  roc_data <- data_test %>%
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
    auc_test_data = auc_test_data,
    mbo_results = mbo_results,
    random_forest_fit = random_forest_fit,
    predictions_test_data = prob_preds,
    mbo_best_params = mbo_best_params,
    preproc_data = preproc_data
  ))
}





