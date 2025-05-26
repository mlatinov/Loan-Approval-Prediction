


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

x <- tibble(
  mtry = 2,
  min_n = 20,
  trees = 200
)
rf_op_range_min_max_sa(best_param = x)




