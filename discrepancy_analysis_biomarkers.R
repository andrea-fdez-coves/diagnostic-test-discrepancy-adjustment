###############################################################################
# DISCREPANCY TERM ANALYSIS FOR DIAGNOSTIC ACCURACY PARAMETERS
# Incorporating systematic bias into sensitivity and specificity distributions
#
# Author: Andrea Fernández Coves
# Publication: Pending
# Last updated: January 2026
# 
# Scenarios:
# - Original Base Case: Original beta distributions from survival data
# - Updated Base Case: Adjusted using full SLNB-derived discrepancy
# - Scenario 1 (Underestimation - Relative): Relative scaling based on test performance
# - Scenario 2 (Underestimation - Absolute): Full SLNB discrepancy applied
# - Scenario 3 (Overestimation - Relative): Relative scaling, subtract discrepancy
# - Scenario 4 (Overestimation - Absolute): Full SLNB discrepancy, subtract
#
# Outputs: 
# 1. Beta distribution parameters (α, β) for each scenario
# 2. Visualization of all distributions
# 3. Summary statistics table with mean, mode, and 95% credible intervals
###############################################################################

# Clear environment and set seed for reproducibility
rm(list = ls())
set.seed(123)
n_samples <- 100000

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

# Source external functions
source("discrepancy_functions.R")
source("summary_functions.R")

# ======================== Literature data ========================
# Source: Meta-analysis of melanoma patients by Valsecchi et al. (2011) 
# doi: 10.1200/JCO.2010.33.1884

# Sensitivity from literature
lit_sens <- list(
  mean = 0.701,
  ci_low = 0.671,
  ci_high = 0.73,
  se = calculate_se_from_ci(0.671, 0.73),
  logodds_mean = logit(0.701),
  var_logit = calculate_logit_variance(0.701, calculate_se_from_ci(0.671, 0.73))
)

# Specificity from literature
lit_spec <- list(
  mean = 0.9046933,
  ci_low = 0.8877387,
  ci_high = 0.9192617,
  se = calculate_se_from_ci(0.8877387, 0.9192617),
  logodds_mean = logit(0.9046933),
  var_logit = calculate_logit_variance(0.9046933, calculate_se_from_ci(0.8877387, 0.9192617))
)

# ======================== Original sensitivity data ========================
# Original beta distributions calculated using 5-year melanoma-specific survival (MSS)

# SLNB sensitivity from MSS data
CAU_sens <- list(
  name = "SLNB",
  alpha = 46.9710401462141,
  beta = 45.9925840173,
  mean = 0.505262575215404,
  ci_low = 0.404007198442151,
  ci_high = 0.60649470577451
)

CAU_sens$se <- calculate_se_from_ci(CAU_sens$ci_low, CAU_sens$ci_high)
CAU_sens$logodds_mean <- logit(CAU_sens$mean)
CAU_sens$var_logit <- calculate_logit_variance(CAU_sens$mean, CAU_sens$se)

# LY75 sensitivity from MSS data
LY75_sens <- list(
  name = "LY75",
  alpha = 21.634573389192,
  beta = 9.66543386282873,
  mean = 0.691200267622791,
  ci_low = 0.525349767749638,
  ci_high = 0.837681109107024
)

LY75_sens$se <- calculate_se_from_ci(LY75_sens$ci_low, LY75_sens$ci_high)
LY75_sens$logodds_mean <- logit(LY75_sens$mean)
LY75_sens$var_logit <- calculate_logit_variance(LY75_sens$mean, LY75_sens$se)

# ======================== Original specificity data ========================
# Original beta distributions calculated using 5-year melanoma-specific survival (MSS)

# SLNB specificity from MSS data
CAU_spec <- list(
  name = "SLNB",
  alpha = 390.778744186823,
  beta = 116.966928736266,
  mean = 0.769634809366493,
  ci_low = 0.731676010114158,
  ci_high = 0.80516707965936
)

CAU_spec$se <- calculate_se_from_ci(CAU_spec$ci_low, CAU_spec$ci_high)
CAU_spec$logodds_mean <- logit(CAU_spec$mean)
CAU_spec$var_logit <- calculate_logit_variance(CAU_spec$mean, CAU_spec$se)

# LY75 specificity from MSS data
LY75_spec <- list(
  name = "LY75",
  alpha = 67.3825833817361,
  beta = 8.11119702438661,
  mean = 0.892558075900399,
  ci_low = 0.815809658501487,
  ci_high = 0.952561016793793
)

LY75_spec$se <- calculate_se_from_ci(LY75_spec$ci_low, LY75_spec$ci_high)
LY75_spec$logodds_mean <- logit(LY75_spec$mean)
LY75_spec$var_logit <- calculate_logit_variance(LY75_spec$mean, LY75_spec$se)

# ======================== DISCREPANCY CALCULATIONS ========================
# Calculate discrepancies for sensitivity
CAU_disc_sens <- calculate_discrepancy(lit_sens, CAU_sens)
LY75_disc_sens <- calculate_discrepancy(lit_sens, LY75_sens)

# Calculate discrepancies for specificity
CAU_disc_spec <- calculate_discrepancy(lit_spec, CAU_spec)
LY75_disc_spec <- calculate_discrepancy(lit_spec, LY75_spec)

# ======================== RUN ALL SCENARIOS ========================
scenarios <- list()

# 1. ORIGINAL BASE CASE
scenarios$CAU_original_sens <- list(
  name = "Original Base Case",
  test_name = "SLNB",
  accuracy = "Sensitivity",
  alpha = CAU_sens$alpha,
  beta = CAU_sens$beta,
  mean = CAU_sens$mean,
  sd = sd(rbeta(n_samples, CAU_sens$alpha, CAU_sens$beta))
)

scenarios$LY75_original_sens <- list(
  name = "Original Base Case",
  test_name = "LY75",
  accuracy = "Sensitivity",
  alpha = LY75_sens$alpha,
  beta = LY75_sens$beta,
  mean = LY75_sens$mean,
  sd = sd(rbeta(n_samples, LY75_sens$alpha, LY75_sens$beta))
)

scenarios$CAU_original_spec <- list(
  name = "Original Base Case",
  test_name = "SLNB",
  accuracy = "Specificity",
  alpha = CAU_spec$alpha,
  beta = CAU_spec$beta,
  mean = CAU_spec$mean,
  sd = sd(rbeta(n_samples, CAU_spec$alpha, CAU_spec$beta))
)

scenarios$LY75_original_spec <- list(
  name = "Original Base Case",
  test_name = "LY75",
  accuracy = "Specificity",
  alpha = LY75_spec$alpha,
  beta = LY75_spec$beta,
  mean = LY75_spec$mean,
  sd = sd(rbeta(n_samples, LY75_spec$alpha, LY75_spec$beta))
)

# Calculate mode and CI for original distributions for reporting
# Sens CAU
orig_sens_samples <- rbeta(n_samples, CAU_sens$alpha, CAU_sens$beta)
density_est <- density(orig_sens_samples)
scenarios$CAU_original_sens$mode <- density_est$x[which.max(density_est$y)]
scenarios$CAU_original_sens$ci_lower <- quantile(orig_sens_samples, 0.025)
scenarios$CAU_original_sens$ci_upper <- quantile(orig_sens_samples, 0.975)

# Sens LY75
orig_sens_samples <- rbeta(n_samples, LY75_sens$alpha, LY75_sens$beta)
density_est <- density(orig_sens_samples)
scenarios$LY75_original_sens$mode <- density_est$x[which.max(density_est$y)]
scenarios$LY75_original_sens$ci_lower <- quantile(orig_sens_samples, 0.025)
scenarios$LY75_original_sens$ci_upper <- quantile(orig_sens_samples, 0.975)

# Spec CAU
orig_spec_samples <- rbeta(n_samples, CAU_spec$alpha, CAU_spec$beta)
density_est <- density(orig_spec_samples)
scenarios$CAU_original_spec$mode <- density_est$x[which.max(density_est$y)]
scenarios$CAU_original_spec$ci_lower <- quantile(orig_spec_samples, 0.025)
scenarios$CAU_original_spec$ci_upper <- quantile(orig_spec_samples, 0.975)

# Spec LY75
orig_spec_samples <- rbeta(n_samples, LY75_spec$alpha, LY75_spec$beta)
density_est <- density(orig_spec_samples)
scenarios$LY75_original_spec$mode <- density_est$x[which.max(density_est$y)]
scenarios$LY75_original_spec$ci_lower <- quantile(orig_spec_samples, 0.025)
scenarios$LY75_original_spec$ci_upper <- quantile(orig_spec_samples, 0.975)

# 2. UPDATED BASE CASE
# Adjusted using full SLNB-derived discrepancy (applied to both tests)
# For LY75, we apply the SLNB literature value as external reference

scenarios$CAU_adjusted_sens <- incorporate_discrepancy(CAU_sens, CAU_disc_sens, "Updated Base Case", n_samples)
scenarios$CAU_adjusted_sens$accuracy <- "Sensitivity"

scenarios$LY75_adjusted_sens <- incorporate_discrepancy(LY75_sens, LY75_disc_sens, "Updated Base Case", n_samples)
scenarios$LY75_adjusted_sens$accuracy <- "Sensitivity"

scenarios$CAU_adjusted_spec <- incorporate_discrepancy(CAU_spec, CAU_disc_spec, "Updated Base Case", n_samples)
scenarios$CAU_adjusted_spec$accuracy <- "Specificity"

scenarios$LY75_adjusted_spec <- incorporate_discrepancy(LY75_spec, LY75_disc_spec, "Updated Base Case", n_samples)
scenarios$LY75_adjusted_spec$accuracy <- "Specificity"

## ======================== Underestimation scenarios ========================

# Calculate performance ratios for relative scenarios
# Ratio of SLNB to LY75 performance (baseline test performance)
ratio_LY75_sens <- CAU_sens$mean / LY75_sens$mean
ratio_LY75_spec <- CAU_spec$mean / LY75_spec$mean

# SCENARIO 1: Underestimation - Relative
# Relative scaling based on test performance
# Adjusts magnitude in proportion to baseline test performance

# Sensitivity
LY75_relative_disc_sens <- create_relative_discrepancy(CAU_disc_sens, LY75_sens, ratio_LY75_sens)

scenarios$LY75_under_relative_sens <- incorporate_discrepancy(LY75_sens, LY75_relative_disc_sens, 
                                                       "Scenario 1 (Underestimation - Relative)", n_samples)
scenarios$LY75_under_relative_sens$accuracy <- "Sensitivity"

# Specificity
LY75_relative_disc_spec <- create_relative_discrepancy(CAU_disc_spec, LY75_spec, ratio_LY75_spec)

scenarios$LY75_under_relative_spec <- incorporate_discrepancy(LY75_spec, LY75_relative_disc_spec, 
                                                       "Scenario 1 (Underestimation - Relative)", n_samples)
scenarios$LY75_under_relative_spec$accuracy <- "Specificity"

# SCENARIO 2: Underestimation - Absolute
# Full SLNB discrepancy applied to LY75
# Assumes the entire SLNB bias magnitude applies equally

# Sensitivity
scenarios$LY75_under_absolute_sens <- incorporate_discrepancy(LY75_sens, CAU_disc_sens, 
                                                       "Scenario 2 (Underestimation - Absolute)", n_samples)
scenarios$LY75_under_absolute_sens$accuracy <- "Sensitivity"

# Specificity
scenarios$LY75_under_absolute_spec <- incorporate_discrepancy(LY75_spec, CAU_disc_spec, 
                                                       "Scenario 2 (Underestimation - Absolute)", n_samples)
scenarios$LY75_under_absolute_spec$accuracy <- "Specificity"

## ======================== Overestimation scenarios ========================

# SCENARIO 3: Overestimation - Relative
# Relative scaling

scenarios$LY75_over_relative_sens <- incorporate_discrepancy(LY75_sens, LY75_relative_disc_sens, 
                                                      "Scenario 3 (Overestimation - Relative)", n_samples, subtract = TRUE)
scenarios$LY75_over_relative_sens$accuracy <- "Sensitivity"

scenarios$LY75_over_relative_spec <- incorporate_discrepancy(LY75_spec, LY75_relative_disc_spec, 
                                                      "Scenario 3 (Overestimation - Relative)", n_samples, subtract = TRUE)
scenarios$LY75_over_relative_spec$accuracy <- "Specificity"

# SCENARIO 4: Overestimation - Absolute
# Full SLNB discrepancy

scenarios$LY75_over_absolute_sens <- incorporate_discrepancy(LY75_sens, CAU_disc_sens, 
                                                      "Scenario 4 (Overestimation - Absolute)", n_samples, subtract = TRUE)
scenarios$LY75_over_absolute_sens$accuracy <- "Sensitivity"

scenarios$LY75_over_absolute_spec <- incorporate_discrepancy(LY75_spec, CAU_disc_spec, 
                                                      "Scenario 4 (Overestimation - Absolute)", n_samples, subtract = TRUE)
scenarios$LY75_over_absolute_spec$accuracy <- "Specificity"

# ======================== GENERATE OUTPUTS ========================

# Generate summary table
summary_table <- generate_summary_table(scenarios)

# Print formatted results table
print_results_table(summary_table)
#write.csv(summary_table, "discrepancy_analysis_results.csv", row.names = FALSE)

# Generate and display the plot
distribution_plot <- plot_distributions(scenarios)
print(distribution_plot)
