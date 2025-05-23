
#### Libraries ####
library(targets)

# Set targets options
tar_option_set(
  packages = c("tidyverse"),
  seed = 123)

# Source functions
tar_source("functions/data_clean_fuction.R")
tar_source("functions/eda_functions.R")

# Workflows
list(
  
    ## Load the data ##
    tar_target(name = file_train,command = "Data/train.csv",format = "file"),
    tar_target(name = file_test,command = "Data/test.csv",format = "file"),
    tar_target(name = loan_train_data,command = read_csv(file = file_train)),
    tar_target(name = loan_test_data,command = read_csv(file = file_test)),

    ## EDA ##
    # Prepare data for EDA
    tar_target(name = loan_train_eda,command = clean_eda(data = loan_train_data))
    
    # Write EDA Report
)

