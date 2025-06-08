

#### Staked Function ####
staked_models_function <- function(data_test,
                                   random_forest,
                                   mars,
                                   xgb,
                                   glm_net,
                                   linear_svm) {
  
  # Extract tuning results for each model type
  best_candidates_rf <- random_forest$mbo$results_tune_grid %>%
    arrange(desc(mean)) %>%
    head(5)
  
  best_candidates_mars <- mars$mbo$results_tune_grid %>%
    arrange(desc(mean)) %>%
    head(5)
  
  best_candidates_xgb <- xgb$mbo$results_tune_grid %>%
    arrange(desc(mean)) %>%
    head(5)
  
  # For glmnet racing results - extract from race_results
  best_candidates_glmnet <- glm_net$tune_race_wl$race_results %>%
    tune::collect_metrics() %>%
    arrange(desc(mean)) %>%
    head(5)
  
  # For glmnet racing results - extract from race_results
  best_candidates_linear_svm <-linear_svm$tune_race_wl$race_results %>%
    tune::collect_metrics() %>%
    arrange(desc(mean)) %>%
    head(5)
  
  # Stack the model 
  model_stacked <- stacks() %>%
    add_candidates(random_forest$random_forest_fit) %>%
    add_candidates(mars$mars_fit) %>%
    add_candidates(xgb$xgb_fit) %>%
    add_candidates(glm_net$glmnet_fit) %>%
    add_candidates()%>%
    blend_predictions() %>%
    fit_members()
  
  prob_preds <- predict(model_stacked, data_test, type = "prob")
  
  roc_data <- data_test %>%
    select(loan_status) %>% 
    bind_cols(prob_preds)
  
  auc_test_data <- roc_auc(
    roc_data,
    truth = loan_status,
    .pred_0,
    event_level = "first"
  )
  
  return(list(
    model_stacked = model_stacked,
    prob_preds = prob_preds,
    roc_data = roc_data,
    auc_test_data = auc_test_data
  ))
}