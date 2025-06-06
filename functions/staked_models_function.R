

#### Staked Function ####

staked_models_function <- function(data_test, ...) {
  
  models <- list(...)
  
  model_stacked <- stacks() %>%
    add_candidates(!!!models) %>%
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