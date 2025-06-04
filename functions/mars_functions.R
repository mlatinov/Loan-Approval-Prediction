

#### MARS Function ####
mars_function <- function(data_train,
                          data_test,
                          data_validation,
                          aov_size = 20,
                          grid_resolution = 30,
                          num_terms_lower_fct = 0.5,
                          num_terms_upper_fct = 2,
                          prod_degree_lower_fct = 0.5,
                          prod_degree_upper_fct = 2
                          ) {
  
  # Recipe 
  recipe_results <- mars_recipe_rose_cor_mrmr(data = data_train)
  mars_recipe <- recipe_results$recipe_mars
  preproc_data <- recipe_results$data
  
  # Message 
  message("Recipe pass")
  
  # Model 
  mars_model <- mars(
    num_terms = tune(),
    prod_degree = tune()) %>%
    set_engine("earth") %>%
    set_mode("classification")
  
  # Workflow
  mars_workflow <- workflow() %>%
    add_recipe(mars_recipe) %>%
    add_model(mars_model)

  # Resamples 
  resamples <- vfold_cv(data = data_validation,v = 5)
  
  # Light tune params range
  mars_param_range <- parameters(
    num_terms(range = c(5,100)),
    prod_degree(range = c(1,5))
  )
  
  # Message
  message("Tune Race Start")
  
  # Start Light Tuning with Tune Race Anova
   mars_aov <- tune_race_aov(
     workflow = mars_workflow,
     burn_in = 3,
     resamples = resamples,
     size = aov_size,
     params = mars_param_range
     )
   
   # Message
   message("Tune Race End")
  
   # Message
   message("PSO Start")
   
   # Optimize the param space
   mars_optim_range <-  mars_optim_range_maximinESE_LHS(
     best_params = mars_aov$best_model_params,
     grid_resolution = grid_resolution,
     num_terms_lower_fct = num_terms_lower_fct,
     num_terms_upper_fct = num_terms_upper_fct,
     prod_degree_lower_fct = prod_degree_lower_fct,
     prod_degree_upper_fct = prod_degree_upper_fct
     )
         
   # Message 
   message("PSO End")
   
   # Message 
   message("MBO Start")
  
   # Warm Started MBO 
    mbo <- mbo_function(
      workflow = mars_workflow,
      resamples = resamples,
      no_improve = 20,
      param_info = mars_optim_range$param_info,
      design = mars_optim_range$final_design
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
    mars_workflow <- finalize_workflow(mars_workflow, mbo_best_params)
    
    # Message
    message("Fit the model ...")
    
    # Fit the model 
    mars_fit <- fit(mars_workflow, data = data_train)
    
    # Message
    message("Predictions...")
    
    # Predict class probabilities on test data
    prob_preds <- predict(mars_fit, data_test, type = "prob")
    
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
      mars_fit = mars_fit,
      predictions_test_data = prob_preds,
      mbo_best_params = mbo_best_params,
      preproc_data = preproc_data
    ))
}

  