
#### Libraries ####
library(tidymodels)
library(finetune)
library(DiceDesign)
library(colino)
library(tidyverse)
library(patchwork)
library(ggcorrplot)
library(DT)
library(targets)
library(tarchetypes)
library(themis)
library(DALEX)
library(stacks)

report_dir <- normalizePath("Report_documents")

# Set targets optionstar
tar_option_set(
  packages = c(
    "tidyverse",
    "patchwork",
    "ggcorrplot",
    "DT",
    "themis",
    "tidymodels",
    "finetune",
    "DiceDesign",
    "colino"
    ),
  seed = 123)

# Source functions
tar_source("functions/data_clean_fuction.R")
tar_source("functions/eda_functions.R")
tar_source("functions/recipe_preprocessing_functions.R")
tar_source("functions/tuning_functions.R")
tar_source("functions/pso_functions.R")
tar_source("functions/random_forest_function.R")
tar_source("functions/explanatory_model_functions.R")
tar_source("functions/mars_functions.R")
tar_source("functions/xgb_functions.R")
tar_source("functions/glmnet_functions.R")
tar_source("functions/staked_models_function.R")
tar_source("functions/svm_function.R")

# Workflows
list(
  
    #### Load the data ####
    tar_target(name = file_train,command = "Data/train.csv",format = "file"),
    tar_target(name = file_test,command = "Data/test.csv",format = "file"),
    tar_target(name = loan_train_data,command = read_csv(file = file_train)),
    tar_target(name = loan_test_data,command = read_csv(file = file_test)),

    #### EDA ####
    
    # Clean data & Prepare data for EDA
    tar_target(name = kaggle_test ,command = clean_eda(data = loan_test_data)),
    tar_target(name = loan_train_eda,command = clean_eda(data = loan_train_data)),
    
    # Write EDA Report
    tar_render(
      name = eda_report,
      path = file.path(report_dir, "Eda_report.Rmd"),
      output_file = file.path(report_dir, "Eda_report.html"),
      params = list(eda_data = loan_train_eda),
      quiet = FALSE
    ),
    
    #### Modeling ####
    
    ## Split the data
    tar_target(
      name = data_split,
      command = initial_validation_split(
        data = loan_train_eda,
        strata = "loan_status")),
    
    ## Get the data 
    
    # Model train data 
    tar_target(
      name = training_data,
      command = training(data_split)),
    
    # Model test data
    tar_target(
      name = testing_data,
      command = testing(data_split)),
    
    # Validation data
    tar_target(
      name = validation_data,
      command = validation(data_split)),
    
    ##### Random forest #### 
    tar_target(
      name = random_forest,
      command = random_forest_func(
        data_train = training_data,
        data_test = testing_data,
        data_validation = validation_data,
        aov_size = 30,
        pso_mtry_lower_fct = 0.5,
        pso_mtry_up_fct = 1.5,
        pso_min_n_lower_fct = 0.5,
        pso_min_n_up_fct = 1.5,
        pso_trees_lower_fct = 0.5,
        pso_trees_up_dct = 1.5
        )
      ),
    
    ##### MARS #### 
    tar_target(
      name = mars,
      command = mars_function(
        data_train = training_data,
        data_test = testing_data,
        data_validation = validation_data,
        aov_size = 30,
        grid_resolution = 30,
        num_terms_lower_fct = 0.5,
        num_terms_upper_fct = 2,
        prod_degree_lower_fct = 0.5,
        prod_degree_upper_fct = 2
        )
    ),
    
    ##### XBG ####
    tar_target(
      name = xgb,
      command = xgb_function(
        data_training = training_data,
        data_testing = testing_data,
        data_validation = validation_data,
        xgb_aov_size = 10,
        grid_resolution = 10,
        mtry_lower_fct = 0.5,
        mtry_upper_fct = 2.0,
        trees_lower_fct = 0.7,
        trees_upper_fct = 1.8,
        min_n_lower_fct = 0.5,
        min_n_upper_fct = 1.5,
        tree_depth_lower_fct = 0.5,
        tree_depth_upper_fct = 1.5
      )
    ),
  
  ##### Glmnet ####
  tar_target(
    name = glm_net,
    command = glmnet_function(
      data_training = training_data,
      data_testing = testing_data,
      data_validation = validation_data,
      WL_size = 20
    )
  ),
  
  ##### SVM ####
  tar_target(
    name = linear_svm,
    command = svm_linear_function(
      data_train = training_data,
      data_test = testing_data,
      data_validation = validation_data,
      wl_size = 20
      )
    ),
  
  ##### Stack Model ####
  tar_target(
    name = staked_models,
    command = staked_models_function(
      data_test = testing_data,
      rf = random_forest$mbo,
      mars = mars$mbo,
      xgb = xgb$mbo,
      glmnet = glm_net$tune_race_wl,
      linear_svm = linear_svm$tune_race_wl
      )
    ),
  
 #### Write Model Explanation Report ####
 
    # Report
    tar_render(
      name = model_explanations_report,
      path = file.path(report_dir, "Model_Explanatory_Analysis.Rmd"),
      output_file = file.path(report_dir, "Model-Explanatory-Analysis.html"),
      params = list(
        random_forest = random_forest,
        mars = mars,
        xgb = xgb,
        glm_net = glm_net,
        linear_svm = linear_svm
        ),
      quiet = FALSE
    )
)

#### Write csv predictions ####





