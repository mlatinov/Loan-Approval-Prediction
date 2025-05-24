
#### Tuning Functions #### ----------------------------

#### Light Tuning ####

## Tune Race Anova Function 

tune_race_aov <- function(workflow, grid = 10, burn_in = 3,resamples) {
  
  # Define control for ANOVA race
  control <- control_race(
    randomize = TRUE,
    burn_in = burn_in,
    save_workflow = TRUE,
    save_pred = TRUE
  )
  
  # Set metric set
  metric <- metric_set(roc_auc)
  
  # Perform tuning
  race_results <- tune_race_anova(
    object = workflow,
    resamples = resamples,
    grid = grid,
    metrics = metric,
    control = control
  )
  
  # Extract best results
  best_model_params <- select_best(race_results, metric = "roc_auc")
  best_model_metric <- show_best(race_results, metric = "roc_auc")
  
  # Return
  return(list(
    race_results = race_results,
    best_model_params = best_model_params,
    best_model_metric = best_model_metric
  ))
}

tune_race_aov(workflow = workfow,resamples = vfold_cv(data = data,v = 5))







