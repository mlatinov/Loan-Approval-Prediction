
clean_eda <- function(data){
  
  data <- data %>%
    # Change data types
    mutate(
      loan_intent = str_to_lower(loan_intent),
      person_home_ownership = str_to_lower(person_home_ownership),
      loan_status = factor(loan_status)
      )%>%
    
    # Convert all char into factors
    mutate(across(where(is.character), as.factor))
  
  return(data)
}

clean_eda_kaggle <- function(data){
  
  data <- data %>%
    # Change data types
    mutate(
      loan_intent = str_to_lower(loan_intent),
      person_home_ownership = str_to_lower(person_home_ownership)
    )%>%
    
    # Convert all char into factors
    mutate(across(where(is.character), as.factor))
  
  return(data)
}
