
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
  
  # Preproc the data 
  preroc_data_prep <- prep(x = recipe_tree,training = data)
  preproc_data <- bake(preroc_data_prep,data)
  
  # Retrun
  return(list(
    recipe = recipe_tree,
    data = preproc_data
    ))
}
  
##### Recipe For Tree-Based models with ROSE for balancing and Boruta Shadows ####

recipe_tree_rose_boruta <- function(data){
  
  # Make a recipe
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
  
  # Preproc the data 
  preroc_data_prep <- prep(x = recipe_tree,training = data)
  preproc_data <- bake(preroc_data_prep,data)
  
  # Retrun
  return(list(
    recipe = recipe_tree,
    data = preproc_data
  ))
  
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
  
  # Preproc the data 
  preroc_data_prep <- prep(x = recipe_gb,training = data)
  preproc_data <- bake(preroc_data_prep,data)
  
  # Retrun
  return(list(
    recipe = recipe_gb,
    data = preproc_data
  ))
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
  
  # Preproc the data 
  preroc_data_prep <- prep(x = recipe,training = data)
  preproc_data <- bake(preroc_data_prep,data)
  
  # Retrun
  return(list(
    recipe = recipe,
    data = preproc_data
  ))
}

#### MARS Recipes ####

## Preproc Needs :
# -Sensitive to irrelevant vars
# -Some sensitivity to scale
# -Multicollinearity issues

##### Recipe For MARS with ROSE for balancing and Correlation Filter and mRMR ####

mars_recipe_rose_cor_mrmr <- function(data){
  
  recipe_mars <- recipe(loan_status ~ .,data = data) %>%
    
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
  
  # Preproc the data 
  preroc_data_prep <- prep(x = recipe_mars,training = data)
  preproc_data <- bake(preroc_data_prep,data)
  
  # Retrun
  return(list(
    recipe_mars = recipe_mars,
    data = preproc_data
  ))
}

#### Neural Networks Recepies ####

# - Strongly affected by scale
# - Need dense, normalized inputs
# - Prefer no missing values

nn_recipe <- function(data){
  
  # Recipe Object 
  recipe_nn <- recipe(loan_status ~ .,data = data) %>%
    
    # Step id new role
    update_role(id ,new_role = "id") %>%
    
    # Scale all numerical features
    step_scale(all_numeric_predictors()) %>%
    
    # One-Hot encode all categorical features
    step_dummy(all_nominal_predictors(),one_hot = TRUE)
  
  # Bake the recipe
  preproc_data_prep <- prep(x = recipe_nn,training = data)
  prepoc_data <- bake(preproc_data_prep,data)
  
  # Return
  return(list(
    recipe = recipe_nn,
    prepoc_data = prepoc_data,
    preproc_data_prep = preproc_data_prep
  ))
  
}










  