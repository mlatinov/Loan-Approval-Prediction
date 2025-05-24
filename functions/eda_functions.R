
##### Univariate Analysis Functions ####

### Univariate Analysis for Numerical Variables

## Summary Table
summary_table_stat <- function(data){
  
  # Check the data type
  if (!is.data.frame(data)) {
    stop("data must be data.frame")
  }
  
  #  Handle NA Values
  if (anyNA(data)) {
    warning("Missing Values Found , rows with NA will be excluded")
    
    data <- na.omit(data)
  }
  
  data %>%
    select(-id) %>%
    select(where(is.numeric)) %>%
    summarise(
      across(
        everything(),
        list(
          mean = ~mean(.x),
          sd = ~sd(.x),
          min = ~min(.x),
          max = ~max(.x),
          median = ~median(.x)
        ),
        .names = "{.col}_{.fn}"
      )
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = c("Feature", "Statistic"),
      names_pattern = "^(.*)_(mean|sd|min|max|median)$",
      values_to = "Value"
    )
}

## Boxplot
boxplot_function <- function(data,col_names){
  
  # Check the data type 
  if (!is.data.frame(data)) {
    stop("Data must be data.frame")
  }
  
  # Check the col_names
  if (!is.character(col_names)) {
    stop("col_names must be a char vector")
  }
  
  # Check if the col_names exist in the data
  missing_names <- setdiff(x = col_names,names(data))
  
  if (length(missing_names)> 0) {
    
    stop("These columns are missing from the data " ,paste0(missing_names,collapse = ", "))
  }
  
  # Check if of the selected columns are numeric
  
  # Filter for wanted columns
  data <- data[col_names]
  
  if (!all(sapply(data,is.numeric))) {
    
    stop("All of the selected columns must be numeric")
  }
  
  # Check for NA 
  if (anyNA(data)) {
    warning("Missing Values Found , rows with NA will be excluded")
    
    data <- na.omit(data)
  }
  
  # Filter and Pivot the data
  data%>%
    pivot_longer(
      cols = everything(),
      names_to = "Features",   
      values_to = "Value"
    ) %>%
    
    ggplot(aes(x = Features, y = Value, fill = Features)) +
    
    # Boxplot
    geom_boxplot(outlier.color = "red") +
    
    # Violin plot
    geom_violin(alpha = 0.3, fill = "grey40") +
    
    # Scale the Colors with Viridis
    scale_fill_viridis_d(option = "A",begin = 0.2,end = 0.7)+
    
    coord_flip() +
    theme_minimal() +
    facet_wrap(~Features, scales = "free") +
    labs(
      title = "Boxplots with Background Density",
      y = NULL,
      x = NULL)+
    theme(
      axis.text.y = element_blank(),
      strip.text = element_text(face = "bold",size = 10))
}

# Univariate Analysis for Categorical Variables
bar_plot_function <- function(data,cols){
  
  # Check the data class
  if (!is.data.frame(data)) {
    stop("Data must be data.frame")
  }
  
  # Check the cols class 
  if (!is.character(cols)) {
    stop("cols must be a char vector")
  }
  
  # Check if the columns exist in the data 
  missing_cols <- setdiff(x = cols,y = names(data))
  
  if (length(missing_cols > 0)) {
    stop("These columns are missing ", paste0(missing_cols,collapse = " , "))
  }
  
  # Filter for the wanted cols
  data <- data[cols]

  # Check if all of the cols are factors
  if (!all(sapply(data,is.factor))) {
    stop("All columns in cols must be factors")
  }
  
  # Check for NAs
  if (anyNA(data)) {
    warning("Missing Values are Found , rows with NA will be excluded")
    
    data <- na.omit(data)
  }
  
  # Plot 
  data %>%
      pivot_longer(
        cols = everything(),
        names_to = "Feature",
        values_to = "Value"
      ) %>%
      ggplot(aes(y = fct_infreq(Value), fill = Value)) +
      geom_bar() +
      scale_fill_viridis_d(option = "A",begin = 0.2,end = 0.8)+
      facet_wrap(~Feature, scales = "free") +
      theme_minimal() +
      labs(
        y = NULL,
        x = "Count",
        title = "Frequency of Factor Levels by Feature"
      ) +
      theme(
        legend.position = "none",
        strip.text.x =element_text(face = "bold",size = 10))
}

##### Bivariate Analysis Functions ####

## Bivariate Analysis for Numerical Variables

## Scatter plot
scatter_plot <- function(data,feature_y,feature_x){
  
  # Check data class
  if (!is.data.frame(data)) {
    stop("Data must be a data.frame")
  }
  
  # Check if the features are char input
  if (!is.character(feature_y) & !is.character(feature_x)) {
    stop("Features have to be char in the input")
  }
  
  # Check if the features they exist in the data 
  col_vector <- c(feature_y,feature_x)
  missing_cols <- setdiff(col_vector,names(data))
  
  if (length(missing_cols)> 0) {
    stop("These columns are missing ", paste0(missing_cols,collapse = " , "))
  }
  
  # Check the class of the features in the data
  data <- data[col_vector]
  if (!all(sapply(data,is.numeric))){
    stop("All columns must be numeric")
  }
  
  # Check for NAs
  if (anyNA(data)) {
    warning("Missing Values Found , rows with missing values will be deleted")
    
    data <- na.omit(data)
  }
  
  # Plot
  plot<-data%>%
      ggplot(aes(y= data[[feature_y]],x = data[[feature_x]]))+
      geom_point(alpha = 0.2,colour = "grey30")+
      geom_smooth(method = "gam",colour = "blue")+
      geom_smooth(method = "lm",colour = "red")+
      theme_minimal()+
      labs(
        title = paste0("Scatter plot between " ,feature_y, " and ", feature_x ,""),
        x = feature_x,
        y = feature_y
      )
  
  # Correlation tests
  pearson_cor_test <- cor(data[[feature_y]],data[[feature_x]],method = "pearson")
  spearman_cor_test <- cor(data[[feature_y]],data[[feature_x]],method = "spearman")
  
  # Linear Model 
  lm_model <- summary(lm(data[[feature_y]]~data[[feature_x]],data = data))
  
  # Return list
  return(list(
    plot = plot,
    pearson_cor_test = pearson_cor_test,
    spearman_cor_test = spearman_cor_test,
    lm_model = lm_model)
  )
}

## Stacked Bar Plot
stacked_bar_plot <- function(data, feature, fill) {
  
  # Check if data is a data.frame
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.")
  }
  
  # Check if feature and fill exist in the data
  char_vector <- c(feature, fill)
  missing_cols <- setdiff(char_vector, names(data)) 
  
  if (length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste0(missing_cols, collapse = ", "))
  }
  
  # Subset only relevant columns
  df <- data[char_vector]
  
  # Check if both are factors
  if (!all(sapply(df, is.factor))) {
    stop("Both `feature` and `fill` must be factors.")
  }
  
  # Remove rows with NA 
  if (anyNA(df)) {
    warning("Missing values detected. Rows with NA will be removed.")
    df <- na.omit(df)
  }
  
  # Create the plot
  plot <- ggplot(df, aes(x = .data[[feature]], fill = .data[[fill]])) +
    geom_bar(position = "fill") +
    scale_fill_viridis_d(option = "A", begin = 0.2, end = 0.8) +
    theme_minimal() +
    labs(
      title = paste0("Stacked Bar Plot of ", feature, " vs ", fill),
      x = feature,
      y = "Proportion",
      fill = fill
    )
  
  # Chi-squared test
  chisq_test <- chisq.test(table(df[[feature]], df[[fill]]))
  
  return(list(
    plot = plot,
    chisq_test = chisq_test
  ))
}

## Bivariate Analysis for Categorical Variables

## Bar Pot of Means per Category & Boxplot

bar_plot_means_boxplot <- function(data,feature,category){
  
  # Check the class of the data
  if (!is.data.frame(data)) {
    stop("Data must be a data.frame")
  }
  # Check the class of feature ans category
  if (!is.factor(data[[category]])) {
    stop(paste(category, "must be a factor."))
  }
  
  if (!is.numeric(data[[feature]])) {
    stop(paste(feature, "must be numeric."))
  }
  
  # Barplot of Means 
 p1 <-data %>%
        group_by(.data[[category]])%>%
        summarise(
          mean_cat = mean(.data[[feature]])
        )%>%
        ggplot(aes(x = fct_reorder(.data[[category]],.x = mean_cat,.fun = mean),y = mean_cat,fill = .data[[category]]))+
        geom_col()+
        scale_fill_viridis_d(option = "A",begin = 0.2,end = 0.7)+
        coord_flip()+
        theme_minimal()+
        labs(
          title = paste0("Barplot Means of " , feature , "  per  " , category , " "),
          x = feature,
          y = category
        )
  
 # Boxplot per Category
 p2 <- data %>%
   ggplot(aes(x = fct_reorder(.data[[category]],.x = .data[[feature]],median), y = .data[[feature]], fill = .data[[category]])) +
   geom_boxplot() +
   scale_fill_viridis_d(option = "A", begin = 0.2, end = 0.7) +
   theme_minimal() +
   labs(
     title = paste0("Boxplot of " , feature , " per " , category ,""),
     x = category,
     y = feature,
   )+
   theme(
     legend.position = "none"
   )
 
 # Statistical test
 num_levels <- nlevels(data[[category]])
 
 if (num_levels > 2) {
   stat_test <- aov(as.formula(paste(feature, "~", category)), data = data)
 } else if (num_levels == 2) {
   stat_test <- t.test(as.formula(paste(feature, "~", category)), data = data)
 } else {
   stop("Error factor levels ")
 }
 
# Combine the plots
 plot_final <- p1/p2
 
 return(list(
   plot = plot_final,
   stat_test = stat_test)
   )
}

#### Multivariate Analysis Functions ####

## Heatmap
heatmap_function <- function(data,feature_amount,category_x,category_y){
  
  # Check the data.frame
  if (!is.data.frame(data)) {
    stop("Data must be data.frame")
  }
  
  # Check if the char exist in the data 
  char_vector <- c(feature_amount,category_x,category_y)
  missing_cols <- setdiff(char_vector, names(data)) 
  
  if (length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste0(missing_cols, collapse = ", "))
  }
  
  # Subset the data 
  data <- data[char_vector]
  
  # Check the categories must be factors
  if (is.factor(data[category_x] & is.factor(data[category_y]))) {
    stop("Categories mist be factors in the data")
  }
  # Check if the feature_amount is num 
  if (is.numeric(data[feature_amount])) {
    stop("Feature amount must be numerical in the data")
  }
  # Check for Na
  if (anyNA(data)) {
    warning("Missing values are found , rows with missing values will be deleted")
    
    data <- na.omit(data)
  }
  
  # Plot
  data %>%
      ggplot(aes(x = .data[[category_x]],.data[[category_y]],fill = .data[[feature_amount]]))+
      geom_tile()+
      guides(fill = guide_colourbar(barwidth = 0.5,barheight = 20))+
      scale_fill_viridis_c(option = "A",begin = 0.2,end = 0.7)+
      theme_minimal()+
      labs(
        title = paste0("Heatmap of " , feature_amount , "  between  " , category_x ,"  and  " , category_y , " "),
        x = category_x,
        y = category_y,
        fill = feature_amount
      )
}
## Correlation Heatmap
correlation_heat_map <- function(data,cols_vector){
  
  # Check the data must be data.frame
  if (!is.data.frame(data)) {
    stop("Data must the data.frame")
  }
  
  # Check if the cols_vector is char vector
  if (!is.character(cols_vector)) {
    stop("cols_vector Must be a char vector")
  }
  
  # Check col_vector have to exist in the data
  missing_cols <- setdiff(cols_vector, names(data)) 
  
  if (length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste0(missing_cols, collapse = ", "))
  }
  # Subset only relevant columns
  data <- data[cols_vector]
  
  # Check if both are numerical
  if (!all(sapply(data, is.numeric))) {
    stop("Both `feature` and `fill` must be factors.")
  }
  
  # Remove rows with NA 
  if (anyNA(data)) {
    warning("Missing values detected. Rows with NA will be removed.")
    data <- na.omit(data)
  }
  # Filter the data to include only columns from the char vector
  data <- data[,cols_vector]
  
  # Compute p_values
  p_mat <- cor_pmat(data)
  
  # Plot the Correlation Matrix
  plot<- data %>%
          cor()%>%
          ggcorrplot(
            hc.order = TRUE,
            type = "lower", p.mat = p_mat,
            sig.level = 0.05,
            lab = TRUE,
            insig = "blank"
            )
  
  return(list(
    plot = plot,
    p_matrix = p_mat)
  )
}

















