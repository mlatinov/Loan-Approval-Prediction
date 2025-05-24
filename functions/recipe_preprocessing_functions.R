
#### Custom Preprocecing With Recipe ####--------------------------------

#### Tree- Based Recipes ####

## Preproc Needs :
# - Some Sensitive to outliers and noise
# - Redundant features may hurt slightly

##### Recipe For Tree-Based models with SMOTENC for balancing and Inf_gain Filter ####

recipe_tree_smotenc_infgain <- function(data){
  
  recipe_tree <- recipe(loan_status ~ .,data = data) %>%
    
    # Step to Exclude Id from the preprocessing
    update_role(id, new_role = "Id") %>%  
    
    # Imputations with Synthetic Minority Over-sampling Technique for Nominal and Continuous features
    step_smotenc(loan_status,over_ratio = 1,seed = 123) %>%
    
    # Removing Near-Zero Variance Predictors
    step_nzv(all_predictors()) %>%
    
    # Information gain filter 
    step_select_infgain(all_numeric_predictors(),outcome = "loan_status",threshold = 0.2) %>%
    
    # Normalize skewed numeric data with Yeo-Johnson
    step_YeoJohnson(all_numeric_predictors()) %>% 
    
    # Encode Categorical variables
    step_dummy(all_nominal_predictors()) 
}
  
##### Recipe For Tree-Based models with ROSE for balancing and Boruta Shadows ####

recipe_tree_rose_boruta <- function(data){
  
  recipe_tree <- recipe(loan_status ~ .,data = data) %>%
    
    # Step to Exclude Id from the preprocessing
    update_role(id, new_role = "Id") %>%  
    
    # ROSE Random Over-Sampling Examples
    step_rose("loan_status",over_ratio = 1) %>%
    
    # Removing Near-Zero Variance Predictors
    step_nzv(all_predictors()) %>%
    
    # Boruta Shadows Filter 
    step_select_boruta(all_predictors(),outcome = "loan_status") %>%
    
    # Normalize skewed numeric data with Yeo-Johnson
    step_YeoJohnson(all_numeric_predictors()) %>% 
    
    # Encode Categorical variables
    step_dummy(all_nominal_predictors()) 
}

##### Recipe For Gradinet Boosting with ROSE for balancing and Boruta Shadows and mRMR ####

gb_recipe_rose_boruta_mrmr <- function(data){
  
  recipe_gb <- recipe(loan_status ~ .,data = data) %>%
    
    # Step to Exclude Id from the preprocessing
    update_role(id, new_role = "Id") %>%  
    
    # ROSE Random Over-Sampling Examples
    step_rose("loan_status",over_ratio = 1) %>%
    
    # Removing Near-Zero Variance Predictors
    step_nzv(all_predictors()) %>%
    
    # Boruta Shadows Filter 
    step_select_boruta(all_predictors(),outcome = "loan_status") %>%
    
    # Apply minimum Redundancy Maximum Relevance Feature Selection
    step_select_mrmr(all_predictors(), outcome = "loan_status",threshold = 0.3) %>% 
    
    # Normalize skewed numeric data with Yeo-Johnson
    step_YeoJohnson(all_numeric_predictors()) %>% 
    
    # Scale Numerical Features
    step_scale(all_numeric_predictors()) %>%
    
    # Encode Categorical variables
    step_dummy(all_nominal_predictors()) 
}

#### Linear-Based Recipes ####

## Specific Preproc needs:
# - Remove collinearity (to avoid multicollinearity issues)
# - Normalize/standardize numeric features (for coefficient interpretability and convergence)
# - Prefer fewer, independent features (to reduce overfitting and improve model stability)
# - Encode categorical variables properly (e.g., one-hot encoding)

##### Recipe For Logistic Regression with ROSE for balancing and Correlation Filter ####

log_r_recipe_rose_cor <- function(data){
  
  recipe <- recipe(loan_status ~ .,data = data) %>%
    
    # Step to Exclude Id from the preprocessing
    update_role(id,new_role = "Id") %>% 
    
    # ROSE Random Over-Sampling Examples
    step_rose("loan_status",over_ratio = 1) %>%
    
    # Removing Near-Zero Variance Predictors
    step_nzv(all_predictors()) %>%
    
    # Remove Correlated Features
    step_corr(all_numeric_predictors(), threshold = 0.8) %>%
  
    # Normalize skewed numeric data with Yeo-Johnson
    step_YeoJohnson(all_numeric_predictors()) %>% 
    
    # Scale Numerical Features
    step_scale(all_numeric_predictors()) %>%
    
    # Encode Categorical variables
    step_dummy(all_nominal_predictors()) 
}

#### MARS Recipes ####

## Preproc Needs :
# -Sensitive to irrelevant vars
# -Some sensitivity to scale
# -Multicollinearity issues

##### Recipe For MARS with ROSE for balancing and Correlation Filter and mRMR ####

mars_recipe_rose_cor_mrmr <- function(data){
  
  recipe_log <- recipe(loan_status ~ .,data = data) %>%
    
    # Step to Exclude Id from the preprocessing
    update_role(id,new_role = "Id") %>% 
    
    # ROSE Random Over-Sampling Examples
    step_rose("loan_status",over_ratio = 1) %>%
    
    # Removing Near-Zero Variance Predictors
    step_nzv(all_predictors()) %>%
    
    # Remove Correlated Features
    step_corr(all_numeric_predictors(), threshold = 0.8) %>%
    
    # Apply minimum Redundancy Maximum Relevance Feature Selection
    step_select_mrmr(all_predictors(), outcome = "loan_status",threshold = 0.3) %>% 
    
    # Normalize skewed numeric data with Yeo-Johnson
    step_YeoJohnson(all_numeric_predictors()) %>% 
    
    # Scale Numerical Features
    step_scale(all_numeric_predictors()) %>%
    
    # Encode Categorical variables
    step_dummy(all_nominal_predictors()) 
}





  