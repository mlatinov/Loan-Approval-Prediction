
#### Tuning Functions #### ----------------------------

#### Light Tuning ####

## Tune Race Anova Function 
tune_race_aov <- function(workflow, burn_in = 3,resamples,size = 20,params) {
  
  # Define control for ANOVA race
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
    best_model_params = best_model_params,
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







iris_recipe <- recipe(Species ~ ., data = iris) %>%
  step_normalize(all_numeric_predictors())

# 3. Model spec: random forest with default settings
rf_spec <- rand_forest(mtry = tune(),trees = tune()) %>%
  set_mode("classification") %>%
  set_engine("ranger")

# 4. Workflow
rf_workflow <- workflow() %>%
  add_recipe(iris_recipe) %>%
  add_model(rf_spec)

params <- parameters(list(
  mtry(range = c(1,3)),
  trees(range = c(100, 500))
))

resamples <- vfold_cv(data = iris, v = 4, strata = Species)
a<-tune_race_wl(workflow = rf_workflow,burn_in = 3,resamples = resamples,size = 10,params =params)

debug(tune_race_wl)

#### MBO Function ####















