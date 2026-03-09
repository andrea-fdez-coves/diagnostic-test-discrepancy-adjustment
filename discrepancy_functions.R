###############################################################################
# DISCREPANCY FUNCTIONS FOR BIAS ANALYSIS
# Helper functions for discrepancy term calculations
###############################################################################

#' Logit Transformation
#'
#' Transforms a probability to the log-odds scale.
#' Used to preserve the [0,1] constraint as described in Section 2.2.1:
#' "logit(Y) = logit(f(x)) + δ"
#'
#' @param p A probability value between 0 and 1
#'
#' @return Log-odds value (real number)
#' @export
#'


logit <- function(p) log(p / (1 - p))

#' Inverse Logit Transformation
#'
#' Transforms a log-odds value back to the probability scale.
#' Used to maintain the [0,1] constraint after applying discrepancy terms.
#'
#' @param lo A log-odds value (real number)
#'
#' @return Probability value between 0 and 1
#' @export
#'

inv_logit <- function(lo) exp(lo) / (1 + exp(lo))

#' Calculate Logit-Scale Variance
#'
#' Propagates uncertainty from probability scale to log-odds scale
#' using the delta method. Essential for combining uncertainties from
#' literature and study data as described in Section 2.2.1.
#'
#' @param p Point estimate on probability scale
#' @param se_p Standard error on probability scale
#'
#' @return Variance on log-odds scale
#' @export
#'

calculate_logit_variance <- function(p, se_p) (1 / (p * (1 - p)))^2 * (se_p^2)

#' Calculate Standard Error from Confidence Interval
#'
#' Derives standard error from a 95% confidence interval assuming
#' approximate normality. Used for literature values where only
#' confidence intervals are reported.
#'
#' @param ci_low Lower bound of 95% confidence interval
#' @param ci_high Upper bound of 95% confidence interval
#'
#' @return Standard error estimate
#' @export
#'

calculate_se_from_ci <- function(ci_low, ci_high) (ci_high - ci_low) / (2 * 1.96)

#' Calculate Discrepancy Term
#'
#' Computes the discrepancy term between literature and study estimates
#' on the log-odds scale
#' δ = logit(Y) - logit(f(x))
#'
#' @param literature_data List containing literature estimates with components:
#'   \itemize{
#'     \item logodds_mean: Mean on log-odds scale
#'     \item var_logit: Variance on log-odds scale
#'   }
#' @param test_data List containing study estimates with components:
#'   \itemize{
#'     \item logodds_mean: Mean on log-odds scale
#'     \item var_logit: Variance on log-odds scale
#'   }
#'
#' @return List containing:
#'   \itemize{
#'     \item mean: Mean discrepancy on log-odds scale
#'     \item sd: Standard deviation of discrepancy
#'     \item var: Variance of discrepancy
#'   }
#' @export
#'

calculate_discrepancy <- function(literature_data, test_data) {
  # logit(Y) = logit(f(x)) + δ, where δ is the discrepancy term
  disc_mean_logodds <- literature_data$logodds_mean - test_data$logodds_mean
  disc_sd_logodds <- sqrt(literature_data$var_logit + test_data$var_logit)
  
  list(
    mean = disc_mean_logodds,
    sd = disc_sd_logodds,
    var = literature_data$var_logit + test_data$var_logit
  )
}

#' Incorporate discrepancy term
#'
#' Applies discrepancy term via Monte Carlo simulation to adjust
#' beta-distributed parameters. Returns new beta distribution parameters
#' using the Method of Moments. Used to generate inputs for the economic model.
#'
#' @param test_data List containing original test data with components:
#'   \itemize{
#'     \item name: Test name
#'     \item alpha: Beta distribution alpha parameter
#'     \item beta: Beta distribution beta parameter
#'   }
#' @param discrepancy List containing discrepancy term from calculate_discrepancy()
#' @param scenario_name Character string naming the scenario
#' @param n_samples Integer number of Monte Carlo samples
#' @param subtract Logical; if TRUE subtracts discrepancy (overestimation),
#'   if FALSE adds discrepancy (underestimation)
#'
#' @return List containing:
#'   \itemize{
#'     \item name: Scenario name
#'     \item test_name: Test name
#'     \item original_samples: Raw Monte Carlo samples from original distribution
#'     \item adjusted_samples: Raw Monte Carlo samples from adjusted distribution
#'     \item mean: Mean of adjusted distribution
#'     \item mode: Mode of adjusted distribution
#'     \item ci_lower: 2.5th percentile (lower bound of 95% CI)
#'     \item ci_upper: 97.5th percentile (upper bound of 95% CI)
#'     \item sd: Standard deviation of adjusted distribution
#'     \item alpha: Beta distribution alpha parameter
#'     \item beta: Beta distribution beta parameter
#'     \item fit_success: Logical indicating if Method of Moments succeeded
#'   }
#' @export
#'

incorporate_discrepancy <- function(test_data, discrepancy, scenario_name, n_samples, subtract = FALSE) {
  # Generate original samples from beta distribution
  original_samples <- rbeta(n_samples, test_data$alpha, test_data$beta)
  original_logodds <- logit(original_samples)
  
  # Generate discrepancy samples (represents structural uncertainty)
  discrepancy_samples <- rnorm(n_samples, discrepancy$mean, discrepancy$sd)
  
  # Apply discrepancy: logit(Y) = logit(f(x)) ± δ
  # subtract = TRUE for overestimation scenarios (correcting downward)
  # subtract = FALSE for underestimation scenarios (correcting upward)
  if (subtract) {
    adjusted_logodds <- original_logodds - discrepancy_samples
  } else {
    adjusted_logodds <- original_logodds + discrepancy_samples
  }
  
  # Transform back to probability scale
  adjusted_samples <- inv_logit(adjusted_logodds)
  
  # Method of Moments to derive new beta distribution parameters
  mean_adj <- mean(adjusted_samples)
  var_adj <- var(adjusted_samples)
  sd_adj <- sd(adjusted_samples)
  
  # Calculate mode for beta distribution
  density_est <- density(adjusted_samples)
  mode_adj <- density_est$x[which.max(density_est$y)]
  
  # Calculate 95% credible interval
  ci_lower <- quantile(adjusted_samples, 0.025)
  ci_upper <- quantile(adjusted_samples, 0.975)
  
  temp <- (mean_adj * (1 - mean_adj)) / var_adj - 1
  
  if (temp > 0) {
    alpha_new <- mean_adj * temp
    beta_new <- (1 - mean_adj) * temp
  } else {
    alpha_new <- beta_new <- NA  # Method of Moments failed (should not happen with sufficient samples)
  }
  
  list(
    name = scenario_name,
    test_name = test_data$name,
    original_samples = original_samples,
    adjusted_samples = adjusted_samples,
    mean = mean_adj,
    mode = mode_adj,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    sd = sd_adj,
    alpha = alpha_new,
    beta = beta_new,
    fit_success = temp > 0
  )
}

#' Create Relative Discrepancy
#'
#' Scales the SLNB discrepancy by the ratio of SLNB to test-specific performance.
#' Used for Scenarios 1 and 3 (relative scaling)
#' This adjusts the magnitude in proportion to baseline test performance.
#'
#' @param base_discrepancy List containing SLNB-derived discrepancy
#' @param test_data List containing test-specific data
#' @param performance_ratio Numeric ratio of SLNB to test performance
#'
#' @return List containing scaled discrepancy with components:
#'   \itemize{
#'     \item mean: Scaled mean discrepancy
#'     \item sd: Scaled standard deviation
#'   }
#' @export
#'

create_relative_discrepancy <- function(base_discrepancy, test_data, performance_ratio) {
  disc_mean_scaled <- base_discrepancy$mean * performance_ratio
  
  # Scale variance by squared performance ratio (error propagation)
  disc_var_scaled <- base_discrepancy$var * (performance_ratio^2)
  
  # Recalculate SD including test variance
  disc_sd_scaled <- sqrt(disc_var_scaled + test_data$var_logit)
  
  list(
    mean = disc_mean_scaled,
    sd = disc_sd_scaled
  )
}