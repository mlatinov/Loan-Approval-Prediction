

#### Deep learning with KERAS ####

recipe_nn <- nn_recipe(data = training_data)

train_data <- recipe_nn$prepoc_data

test_data <- bake(recipe_nn$preproc_data_prep,new_data = testing_data)

val_data <- bake(recipe_nn$preproc_data_prep,new_data = validation_data)

table(train_data$loan_status)

compute_class_weights <- function(y) {
  tbl <- table(y)
  total <- sum(tbl)
  weights <- total / (length(tbl) * tbl)
  as.list(weights)
}
 case_weights <- compute_class_weights(training_data$loan_status)

# Separate Predictors and Target and convert to matrix

x_train <- train_data %>% select(-loan_status) %>% as.matrix()
y_train <- ifelse(train_data$loan_status == "1", 1, 0)

x_test <- test_data %>% select(-loan_status) %>% as.matrix()
y_test  <- ifelse(test_data$loan_status == "1", 1, 0)

x_val <- val_data %>% select(-loan_status) %>% as.matrix()
y_val   <- ifelse(val_data$loan_status == "1", 1, 0)

## Keras model

model <- keras_model_sequential() %>%
  
  # Hidden layers 
  layer_dense(units = 256,activation = "relu",input_shape = 27) %>%
  layer_dropout(rate = 0.3) %>%
  
  layer_dense(units = 128,activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  
  layer_dense(units = 64, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>% 
  
  layer_dense(units = 32, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  
  # Output layer for binary class
  layer_dense(units = 1,  activation = "sigmoid")

# Compiling the model
model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = metric_auc()
  )

# History
history <- model %>% fit(
  x_train,
  y_train,
  batch_size = 32,
  epochs = 200,
  validation_data = list(x_val, y_val),
  class_weight = case_weights
)














