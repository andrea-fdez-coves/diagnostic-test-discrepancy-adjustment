###############################################################################
# SUMMARY AND VISUALIZATION FUNCTIONS FOR BIAS ANALYSIS
# Functions to generate summary tables and plots
###############################################################################

#' Generate Summary Table
#'
#' Creates a comprehensive summary table from the scenarios list.
#' Extracts beta distribution parameters, mean, standard deviation, mode, 
#' and 95% credible intervals for all scenarios, tests, and metrics.
#'
#' @param scenarios_list List containing all scenario results from the analysis
#'
#' @return Data frame with columns:
#'   \itemize{
#'     \item Scenario: Scenario name
#'     \item Test: Test name (SLNB or LY75)
#'     \item Metric: Sensitivity or Specificity
#'     \item Alpha: Beta distribution alpha parameter
#'     \item Beta: Beta distribution beta parameter
#'     \item Mean: Distribution mean
#'     \item SD: Distribution standard deviation
#'     \item Mode: Distribution mode
#'     \item CI_Lower: Lower bound of 95% credible interval (2.5th percentile)
#'     \item CI_Upper: Upper bound of 95% credible interval (97.5th percentile)
#'   }
#' @export
#'

generate_summary_table <- function(scenarios_list) {
  
  # Initialize empty data frame
  summary_df <- data.frame()
  
  # Extract relevant data from scenarios list
  for (scenario_name in names(scenarios_list)) {
    scenario <- scenarios_list[[scenario_name]]
    
    # Handle both list types (original and adjusted)
    if ("accuracy" %in% names(scenario)) {
      row_df <- data.frame(
        Scenario = scenario$name,
        Test = scenario$test_name,
        Metric = scenario$accuracy,
        Alpha = round(scenario$alpha, 3),
        Beta = round(scenario$beta, 3),
        Mean = round(scenario$mean, 3),
        SD = round(scenario$sd, 3),
        Mode = round(scenario$mode, 3),
        CI_Lower = round(scenario$ci_lower, 3),
        CI_Upper = round(scenario$ci_upper, 3),
        stringsAsFactors = FALSE
      )
      summary_df <- rbind(summary_df, row_df)
    }
  }
  
  # Sort by scenario order
  scenario_order <- c("Original Base Case", "Updated Base Case", 
                      "Scenario 1 (Underestimation - Relative)", 
                      "Scenario 2 (Underestimation - Absolute)",
                      "Scenario 3 (Overestimation - Relative)", 
                      "Scenario 4 (Overestimation - Absolute)")
  
  summary_df$Scenario <- factor(summary_df$Scenario, levels = scenario_order)
  summary_df <- summary_df[order(summary_df$Scenario, summary_df$Test, summary_df$Metric), ]
  
  return(summary_df)
}

#' Plot Beta Distributions
#'
#' Creates a faceted plot showing beta distributions for all scenarios.
#' Compares SLNB and LY75 tests with sensitivity (solid lines) and
#' specificity (dashed lines) across all six scenarios.
#'
#' @param scenarios_list List containing all scenario results from the analysis
#'
#' @return A ggplot2 object containing the faceted distribution plot
#' @export
#' @import ggplot2
#'

plot_distributions <- function(scenarios_list) {
  
  # Load required libraries if not already loaded
  if (!require(ggplot2, quietly = TRUE)) {
    stop("ggplot2 package is required for plotting")
  }
  
  # Create data frame for plotting
  plot_data <- data.frame()
  
  for (scenario_name in names(scenarios_list)) {
    scenario <- scenarios_list[[scenario_name]]
    
    if ("accuracy" %in% names(scenario) && !is.na(scenario$alpha) && !is.na(scenario$beta)) {
      # Generate beta distribution points
      x_vals <- seq(0.001, 0.999, length.out = 500)
      y_vals <- dbeta(x_vals, scenario$alpha, scenario$beta)
      
      temp_df <- data.frame(
        Scenario = scenario$name,
        Test = scenario$test_name,
        Metric = scenario$accuracy,
        x = x_vals,
        y = y_vals
      )
      plot_data <- rbind(plot_data, temp_df)
    }
  }
  
  # Set factor levels for proper ordering
  scenario_order <- c("Original Base Case", "Updated Base Case", 
                      "Scenario 1 (Underestimation - Relative)", 
                      "Scenario 2 (Underestimation - Absolute)",
                      "Scenario 3 (Overestimation - Relative)", 
                      "Scenario 4 (Overestimation - Absolute)")
  
  plot_data$Scenario <- factor(plot_data$Scenario, levels = scenario_order)
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = x, y = y, color = Test, linetype = Metric)) +
    geom_line(linewidth = 0.8) +
    facet_wrap(~ Scenario, ncol = 3) +
    scale_color_manual(values = c('SLNB' = '#1f77b4', 
                                  'LY75' = '#ff7f0e')) +
    scale_linetype_manual(values = c('Sensitivity' = 'solid', 
                                     'Specificity' = 'dashed')) +
    labs(title = 'Beta Distributions for Sensitivity and Specificity Across Scenarios',
         subtitle = 'Comparing SLNB and LY75 tests',
         x = 'Probability',
         y = 'Density',
         color = 'Test',
         linetype = 'Metric') +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = 'bold', size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      legend.position = 'bottom',
      panel.grid.major = element_line(color = 'grey80', linewidth = 0.2),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = 'grey90', color = NA),
      strip.text = element_text(face = 'bold')
    ) +
    ylim(0, 30) +
    xlim(0, 1)
  
  return(p)
}

#' Print Results Table
#'
#' Formats and prints a publication-ready summary table to the console.
#' Includes all scenarios with their beta parameters, mean, standard deviation,
#' mode, and 95% credible intervals.
#'
#' @param summary_table Data frame from generate_summary_table()
#'
#' @return NULL (invisible). Prints table to console.
#' @export
#'

print_results_table <- function(summary_table) {
  cat("\n")
  cat(rep("=", 130), sep = "")
  cat("\n")
  cat("SENSITIVITY AND SPECIFICITY DISTRIBUTION PARAMETERS\n")
  cat(rep("=", 130), sep = "")
  cat("\n\n")
  
  cat(sprintf("%-35s %-10s %-12s %-8s %-8s %-8s %-8s %-8s %-8s %-8s\n", 
              "Scenario", "Test", "Metric", "Alpha", "Beta", "Mean", "SD", "Mode", "CI Lower", "CI Upper"))
  cat(sprintf("%-35s %-10s %-12s %-8s %-8s %-8s %-8s %-8s %-8s %-8s\n", 
              paste(rep("-", 35), collapse = ""),
              paste(rep("-", 10), collapse = ""),
              paste(rep("-", 12), collapse = ""),
              paste(rep("-", 8), collapse = ""),
              paste(rep("-", 8), collapse = ""),
              paste(rep("-", 8), collapse = ""),
              paste(rep("-", 8), collapse = ""),
              paste(rep("-", 8), collapse = ""),
              paste(rep("-", 8), collapse = ""),
              paste(rep("-", 8), collapse = "")))
  
  for (i in 1:nrow(summary_table)) {
    row <- summary_table[i, ]
    
    cat(sprintf("%-35s %-10s %-12s %-8s %-8s %-8s %-8s %-8s %-8s %-8s\n",
                row$Scenario, row$Test, row$Metric, 
                sprintf("%.3f", row$Alpha), sprintf("%.3f", row$Beta),
                sprintf("%.3f", row$Mean), sprintf("%.3f", row$SD),
                sprintf("%.3f", row$Mode),
                sprintf("%.3f", row$CI_Lower), sprintf("%.3f", row$CI_Upper)))
  }
  
  cat("\n")
  cat(rep("=", 130), sep = "")
  cat("\n")
  cat("SUMMARY OF SCENARIOS (Section 2.2.1):\n")
  cat("- Original Base Case: Beta distributions calculated using 5-year melanoma-specific survival\n")
  cat("- Updated Base Case: Adjusted using full SLNB-derived discrepancy\n")
  cat("- Scenario 1 (Underestimation - Relative): Relative scaling based on test performance\n")
  cat("- Scenario 2 (Underestimation - Absolute): Full SLNB discrepancy applied\n")
  cat("- Scenario 3 (Overestimation - Relative): Relative scaling, subtract discrepancy\n")
  cat("- Scenario 4 (Overestimation - Absolute): Full SLNB discrepancy, subtract\n")
  cat(rep("=", 130), sep = "")
  cat("\n")
  invisible(NULL)
}