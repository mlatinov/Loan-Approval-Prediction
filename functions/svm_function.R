


##### Linear SVM ####
svm_linear_function <- function(data_train,
                                data_test,
                                data_validation,
                                wl_size = 20
                                ){
  
  
  # Preproc Recipe 
  recipe_results <- svm_recipe_smotenc(data = data_train)
  recipe <- recipe_results$recipe
  preproc_data <- recipe_results$prepoc_data
  preproc_data_prep <- recipe_results$preproc_data_prep
  
  # Message
  message("Recipe Pass...")
  
  # Model Spec
  linear_svm <- svm_linear(
    cost = tune()) %>%
    set_mode("classification") %>%
    set_engine("kernlab")
  
  # Workflow 
  linear_svm_workflow <- workflow()%>%
    add_model(linear_svm) %>%
    add_recipe(recipe)
  
  # Parameters 
  svm_parameters <- parameters(
    cost(range = c(-10,5))
  )
  
  # Resamples 
  resamples <- vfold_cv(data = data_validation,v = 3)
  
  # Message
  message("Start Tune WL...")
  
  # Tune Race WL
  tune_race_wl <- tune_race_wl(
    workflow = linear_svm_workflow,
    burn_in = 2,
    resamples = resamples,
    size = wl_size,
    params = svm_parameters
    )
  
  # Message
  message("Collect params...")
  
  # Collect the best params
  best_params <- tune_race_wl$best_model_params
  results <- tune_race_wl$performance_cv
  
  # Message
  message("Finalize Workflow ...")
  
  # Finalize workflow  
  svm_linear_workflow <- finalize_workflow(linear_svm_workflow, best_params)
  
  # Message
  message("Fit the model ...")
  
  # Fit the model 
  svm_linear_fit <- fit(linear_svm_workflow, data = data_train)
  
  # Message
  message("Predictions...")
  
  # Predict class probabilities on test data
  prob_preds <- predict(svm_linear_fit, data_test, type = "prob")
  
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
    tune_race_wl = tune_race_wl,
    auc_test_data = auc_test_data,
    results = results,
    svm_fit = svm_linear_fit,
    predictions_test_data = prob_preds,
    best_params = best_params,
    preproc_data = preproc_data
  ))
}