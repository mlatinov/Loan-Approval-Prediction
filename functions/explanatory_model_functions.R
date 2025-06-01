

#### Model Explanations Functions ####

## Global Explanations
global_explain <- function(data, model, label) {
  
  # Convert loan_status factor to numeric 
  y_numeric <- as.numeric(as.character(data$loan_status))
  
  # Extract model from fitted workflow
  model_only <- extract_fit_parsnip(model)
  
  # Predict function for parsnip model
  predict_function <- function(model, newdata) {
    predict(model, new_data = newdata, type = "prob")$.pred_1
  }
  
  # Message
  message("Creating Explainer")
  
  # Create explainer
  explainer <- DALEX::explain(
    model = model_only,
    data = data %>% select(-loan_status),
    y = y_numeric,
    label = label,
    type = "classification",
    predict_function = predict_function,
    verbose = FALSE
  )
  
  # Message
  message("Model Evaluation")
  
  # Model-performance Measures
  model_eval <- model_performance(explainer = explainer)
  
  # Combine plot histogram and ROC Curve
  histogram <- plot(model_eval,geom = "histogram")
  roc_curve <- plot(model_eval,geom = "roc")
  
  model_performance_plot <- histogram + roc_curve
  
  # Message
  message("Variable importance")
  
  # Variable importance Measures
  vars <- names(data)
  
  # Permutation based imporance for all features
  importance_full <- model_parts(explainer = explainer,N = 50,variables = vars)
  
  importance_full_plot <- plot(importance_full) + 
    ggtitle("Mean variable-importance over 50 permutations", "")
  
  # Message
  message("PDP")
  
  ## PDP profiles
  
  # PDP age
  pdp_loan_intent_rate <- model_profile(explainer = explainer,variables = "loan_int_rate",type = "partial")
  pdp_loan_intent_rate_plot <- plot(pdp_loan_intent_rate, geom = "profiles") + 
    ggtitle("Ceteris-paribus and partial-dependence profiles for age")
  
  # PDP income
  pdp_income <- model_profile(explainer = explainer,variables = "person_income",type = "partial")
  pdp_income_plot <- plot(pdp_income, geom = "profiles") + 
    ggtitle("Ceteris-paribus and partial-dependence profiles for income")
  
  # PDP person_emp_length
  pdp_emp_length <- model_profile(explainer = explainer,variables = "person_emp_length",type = "partial")
  pdp_emp_lenght_plot <- plot(pdp_emp_length, geom = "profiles") + 
    ggtitle("Ceteris-paribus and partial-dependence profiles for EMP length")
  
  # Message
  message("Accumulated Local Effect ")
  
  ale_loan_pct_income <- model_profile(explainer = explainer,variables = "loan_percent_income",type = "accumulated")
  ale_loan_pct_income_plot <- plot(ale_loan_pct_income)+
    ggtitle("Accumulated-local profiles for Loan percent of Income")
  
  ale_age <- model_profile(explainer = explainer,variables = "person_age",type = "accumulated")
  ale_age_plot <- plot(ale_age)+
    ggtitle("Accumulated-local profiles for Age")
  
  # Return
  return(list(
    model_eval = model_eval,
    model_performance_plot = model_performance_plot,
    importance_full_plot = importance_full_plot,
    pdp_loan_intent_rate_plot = pdp_loan_intent_rate_plot,
    pdp_income_plot = pdp_income_plot,
    pdp_emp_lenght_plot = pdp_emp_lenght_plot,
    ale_loan_pct_income_plot = ale_loan_pct_income_plot,
    ale_age_plot = ale_age_plot
    
  ))
  
}

x <- global_explain(data = random_forest$preproc_data,model = random_forest$random_forest_fit,label = "Random Forest")

## Local Explanations 
local_explain <- function(data,model,instance,label){
  
  # Convert loan_status factor to numeric 
  y_numeric <- as.numeric(as.character(data$loan_status))
  
  # Extract model from fitted workflow
  model_only <- extract_fit_parsnip(model)
  
  # Predict function for parsnip model
  predict_function <- function(model, newdata) {
    predict(model, new_data = newdata, type = "prob")$.pred_1
  }
  
  # Message
  message("Creating Explainer")
  
  # Create explainer
  explainer <- DALEX::explain(
    model = model_only,
    data = data %>% select(-loan_status),
    y = y_numeric,
    label = label,
    type = "classification",
    predict_function = predict_function,
    verbose = FALSE
  )
  
  # Message
  message("Break Down Plots")
  
  # BD plots
  break_down <- predict_parts(explainer = explainer,
                              new_observation = instance,
                              type = "break_down")
  
  break_down_plot <- plot(break_down)+
    ggtitle("Break Down Plot")
  
  # Message
  message("Shapley Additive Explanations")
  
  # Shapley Additive Explanations 
  shap <- predict_parts(explainer = explainer,
                        new_observation = instance,
                        type = "shap")
  
  shap_plot <- plot(shap,show_boxplots = FALSE)+
    ggtitle("Shapley Additive Explanations")
  
  # Return
  return(list(
    break_down_plot = break_down_plot,
    shap_plot = shap_plot,
  ))
  
}

instance <- random_forest$preproc_data %>% slice(1)

y <- local_explain(data = random_forest$preproc_data,
                   model = random_forest$random_forest_fit,
                   label = "Random Forest",
                   instance = instance
                   )



