
#### Tuning Functions #### ----------------------------

#### Light Tuning ####

## Tune Race Anova Function 
tune_race_aov <- function(workflow, burn_in = 3,resamples,size = 20,params) {
  
  # Define control for ANOVA race
  control <- finetune::control_race(
    randomize = TRUE,
    burn_in = burn_in,
    save_workflow = TRUE,
    save_pred = TRUE
  )
  
  # Set metric set
  metric_aov <- metric_set(roc_auc)
  
  # Custom Grid based on the params
  custom_grid <- grid_random(params, size = size)
  
  # Perform tuning
  race_results <- tune_race_anova(
    object = workflow,
    resamples = resamples,
    grid = custom_grid,
    metrics = metric_aov,
    control = control
  )
  
  # Extract best results and Performance
  performance_cv <- collect_metrics(race_results)
  
  best_model_params <- race_results %>% 
    show_best(n = 1) %>%
    select(-.metric, -.estimator, -mean, -n, -std_err, -.config)
  
  # Return
  return(list(
    performance_cv = performance_cv,
    best_model_params = best_model_params
  ))
}

## Tune Race win or lose Function 
tune_race_wl <- function(workflow,burn_in,resamples,size=20,params){
  
  # Define control race
  control <- control_race(
    randomize = TRUE,
    burn_in = burn_in,
    save_workflow = TRUE, 
    save_pred = TRUE
  )
 
  # Set metric set
  metric_aov <- metric_set(roc_auc)
  
  # Custom Grid based on the params
  custom_grid <- grid_random(params, size = size) 
  
  # Perform tuning
  race_results <- tune_race_win_loss(
    object = workflow,
    resamples = resamples,
    grid = custom_grid,
    metrics = metric_aov,
    control = control
  )
  
  # Extract best results and Performance
  performance_cv <- collect_metrics(race_results)
  
  best_model_params <- race_results %>% 
    show_best(n = 1) %>%
    select(-.metric, -.estimator, -mean, -n, -std_err, -.config)
  
  # Return
  return(list(
    performance_cv = performance_cv,
    best_model_params = best_model_params
  ))
}


#### MBO Function ####

mbo_function <- function(workflow,
                         resamples,
                         no_improve = 12,
                         param_info,
                         design) {
  
  
  ## Set Up Bayesian Control Parameters
  bayes_control <- control_bayes(
    no_improve = 15,
    save_pred = TRUE,
    save_workflow = TRUE,
    seed = 123
  )
  
  ## Set up Tune grid control 
  grid_control <- control_grid(
    save_pred = TRUE,
    save_workflow = TRUE
  )
  
  # Set a metric
  metric <- metric_set(roc_auc)
  
  # Initial results from a tune_grid
  initial <- tune_grid(
    object = workflow,
    grid = design,
    resamples = resamples,
    metrics = metric,
    control = grid_control
    )
  
  
  # Metrics from initial 
  results_intial <- collect_metrics(initial)
  
  # MBO 
  mbo <- tune_bayes(
    object = workflow,
    initial = initial,
    param_info = param_info,
    resamples = resamples,
    metrics = metric,
    control = bayes_control
    )
  
  # Select best params from MBO
  best_mbo <- mbo %>% select_best()
  
  # Collect all results from MBO
  mbo_results <- collect_metrics(mbo)
  
  # Return 
  return(list(
    best_mbo_params = best_mbo,
    results = mbo_results,
    results_tune_grid = results_intial
    ))
}













