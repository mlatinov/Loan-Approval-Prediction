
#### Libraries ####
library(targets)
library(tarchetypes)
report_dir <- normalizePath("Report_documents")

# Set targets options
tar_option_set(
  packages = c("tidyverse","patchwork","ggcorrplot","DT"),
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
    tar_target(name = loan_train_eda,command = clean_eda(data = loan_train_data)),
    
    # Write EDA Report
    tar_render(
      name = eda_report,
      path = file.path(report_dir, "Eda_report.Rmd"),
      output_file = file.path(report_dir, "Eda_report.html"),
      params = list(eda_data = loan_train_eda),
      quiet = FALSE
    )
)




