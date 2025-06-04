


#### Parameter Space optimization -------------------------------####

# Optim parameter space for RF using
# min max LHC SA and Adaptive param space based on tune race anova best param results
rf_op_range_min_max_sa <- function(best_param,
                                   mtry_lower_fct = 0.9,
                                   mtry_up_fct = 1.1,
                                   min_n_lower_fct = 0.9,
                                   min_n_up_fct = 1.1,
                                   trees_lower_fct = 0.9,
                                   trees_up_dct = 1.1 ) {
  
  # Define adaptive parameter ranges around the best known parameter values
  rf_param_space <- parameters(
    mtry(range = c(
      max(1, floor(best_param$mtry * mtry_lower_fct)),
      ceiling(best_param$mtry * mtry_up_fct)
    )),
    min_n(range = c(
      max(1, floor(best_param$min_n * min_n_lower_fct)),
      ceiling(best_param$min_n * min_n_up_fct)
    )),
    trees(range = c(
      max(1, floor(best_param$trees * trees_lower_fct)),
      ceiling(best_param$trees * trees_up_dct)
    ))
  )
  
  # Get numeric ranges for LHS sampling
  rf_ranges <- map(rf_param_space$object, ~ as.numeric(range_get(.x)))
  
  # Create a sequence grid for each param 
  df_param <- data.frame(
    mtry  = as.integer(seq(rf_ranges[[1]][1], rf_ranges[[1]][2], length.out = 20)),
    min_n = as.integer(seq(rf_ranges[[2]][1], rf_ranges[[2]][2], length.out = 20)),
    trees = as.integer(seq(rf_ranges[[3]][1], rf_ranges[[3]][2], length.out = 20))
  )
  
  # Heuristic function to check irregularity of coverage in param space
  heuristics_check <- function(df) {
    df_scaled <- df %>% mutate(across(everything(), rescale))
    dist_matrix <- dist(as.matrix(df_scaled))
    coverage <- sd(dist_matrix) / mean(dist_matrix)
    return(coverage)
  }
  
  # Initial coverage of the naive grid
  initial_coverage <- heuristics_check(df_param)
  
  # Generate initial Latin Hypercube Design in normalized space
  lhc_design <- lhsDesign(n = 20, dimension = 3, randomized = TRUE, seed = 123)$design
  
  # Optimize design with maximin Simulated Annealing
  optimized_lhc_rf <- maximinSA_LHS(
    design = lhc_design,
    T0 = 10,
    c = 0.97,
    it = 2000,
    profile = "GEOM_MORRIS"
  )
  
  # Map normalized optimized design to actual parameter values
  final_design <- data.frame(
    mtry = df_param$mtry[findInterval(optimized_lhc_rf$design[, 1], seq(0, 1, length.out = 21))],
    min_n = df_param$min_n[findInterval(optimized_lhc_rf$design[, 2], seq(0, 1, length.out = 21))],
    trees = df_param$trees[findInterval(optimized_lhc_rf$design[, 3], seq(0, 1, length.out = 21))]
  )
  
  # Coverage of the optimized design
  optimized_coverage <- heuristics_check(final_design)
  
  # Warn if optimization did not improve coverage
  if (initial_coverage < optimized_coverage) {
    warning("Initial coverage is better than the optimized coverage.")
  }
  
  # Create parameter info object with ranges from optimized design
  param_info <- parameters(
    mtry(range = range(final_design$mtry)),
    min_n(range = range(final_design$min_n)),
    trees(range = range(final_design$trees))
  )
  
  # Return param_info and coverage stats
  return(list(
    param_info = param_info,
    initial_coverage = initial_coverage,
    optimized_coverage = optimized_coverage,
    final_design = final_design
  ))
}

# Optim parameter space for MARS using
# Enhanced Stochastic Evolutionary optimization of LHS

mars_optim_range_maximinESE_LHS <- function(best_params,
                                            grid_resolution = 20,
                                            num_terms_lower_fct = 0.5,
                                            num_terms_upper_fct = 1.5,
                                            prod_degree_lower_fct = 0.5,
                                            prod_degree_upper_fct = 1.5
                                            ) {
  
  # Define adaptive parameter ranges around the best known parameter values
  
  mars_param_space <- parameters(
    
    # Scale num_terms by num_terms_lower_fct and num_terms_high_fct
    num_terms(range = c(
      max(1,floor(best_params$num_terms * num_terms_lower_fct)),
      ceiling(best_params$num_terms * num_terms_upper_fct)
    )),
    # Scale prod_degree by prod_degree_lower_fct and prod_degree_upper_fct
    prod_degree(range = c(
      max(1,floor(best_params$prod_degree * prod_degree_lower_fct)),
      ceiling(best_params$prod_degree * prod_degree_upper_fct)
    ))
  )
    # Get numeric ranges for LHS sampling
    mars_ranges <- map(mars_param_space$object, ~ as.numeric(range_get(.x)))
    
    # Create a sequence grid for each param 
    mars_df_params <- data.frame(
      num_terms = as.integer(seq(mars_ranges[[1]][1],mars_ranges[[1]][2],length.out = grid_resolution)),
      prod_degree = as.integer(seq(mars_ranges[[2]][1],mars_ranges[[2]][2],length.out = grid_resolution))
    )
    
    # Heuristic function to check irregularity of coverage in param space
    heuristics_check <- function(df) {
      df_scaled <- df %>% mutate(across(everything(), ~rescale(.x, c(0, 1))))
      dist_matrix <- dist(as.matrix(df_scaled))
      coverage <- sd(dist_matrix) / mean(dist_matrix)
      return(coverage)
    }
    
    # Initial coverage of the naive grid
    initial_coverage <- heuristics_check(mars_df_params)
  
    # Generate initial Latin Hypercube Design in normalized space
    lhc_design <- lhsDesign(n = grid_resolution, dimension = 2, randomized = TRUE, seed = 123)$design
    
   #  maximin LHS. ESE 
   optim_lch_mars <- 
     maximinESE_LHS(
       design = lhc_design,
       T0 = 0.1,
       inner_it = 15,
       J = 5,
       it = 50,
       p = 50
     )
   
   # Map normalized optimized design to actual parameter values
   final_design <- data.frame(
     num_terms = mars_df_params$num_terms[findInterval(optim_lch_mars$design[, 1], seq(0, 1, length.out = grid_resolution))],
     prod_degree = mars_df_params$prod_degree[findInterval(optim_lch_mars$design[, 2], seq(0, 1, length.out = grid_resolution))]
   )
   
   # Coverage of the optimized design
   optimized_coverage <- heuristics_check(final_design)
   
   # Warn if optimization did not improve coverage
   if (initial_coverage < optimized_coverage) {
     warning("Initial coverage is better than the optimized coverage.")
   }
   
   # Create parameter info object with ranges from optimized design
   param_info <- parameters(
     num_terms(range = range(final_design$num_terms)),
     prod_degree(range = range(final_design$prod_degree))
   )
   
   # Return param_info and coverage stats
   return(list(
     param_info = param_info,
     initial_coverage = initial_coverage,
     optimized_coverage = optimized_coverage,
     final_design = final_design
   ))
}







