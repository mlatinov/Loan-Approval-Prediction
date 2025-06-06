


#### Glmnet Functions ####

glmnet_function <- function(data_training,
                            data_testing,
                            data_validation,
                            WL_size = 20
                            ) {
  
  
  # Recipe 
  recipe_results <- log_r_recipe_rose_cor(data =data_training )
  recipe <- recipe_results$recipe
  data_preproc <- recipe_results$data
  
  # Message
  message("Recipe Pass")
  
  # Model 
  glmnet <- logistic_reg(
    penalty = tune(),
    mixture = tune()) %>%
    set_engine("glmnet")
  
  # Resamples
  resamples <- vfold_cv(data = data_validation,v = 5)
  
  # Workflow 
  glmnet_workflow <- workflow() %>%
    add_recipe(recipe)%>%
    add_model(glmnet)
  
  # Parameters
  glmnet_params <- parameters(
    penalty(range = c(-10,0)),
    mixture(range = c(0,1))
  )
  
   # Message
   message("Tune Race WL Start")
  
  # Tune Race
  tune_race_wl <- tune_race_wl(
    workflow = glmnet_workflow,
    burn_in = 3,
    resamples = resamples,
    size = WL_size,
    params = glmnet_params
    )
  
   # Message
   message("Collect params...")
  
   # Collect the best params
   best_params <- tune_race_wl$best_model_params
   results <- tune_race_wl$performance_cv
   
   # Message
   message("Finalize Workflow ...")
   
   # Finalize workflow  
   glmnet_workflow <- finalize_workflow(glmnet_workflow, best_params)
   
   # Message
   message("Fit the model ...")
   
   # Fit the model 
   glmnet_fit <- fit(glmnet_workflow, data = data_training)
   
   # Message
   message("Predictions...")
   
   # Predict class probabilities on test data
   prob_preds <- predict(glmnet_fit, data_testing, type = "prob")
   
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
     tune_race_wl = tune_race_wl,
     auc_test_data = auc_test_data,
     results = results,
     glmnet_fit = glmnet_fit,
     predictions_test_data = prob_preds,
     best_params = best_params,
     preproc_data = data_preproc
   ))
    
}