############################################
# Confidence Intervals Helper Functions    #
############################################

# Identify Standard from stdev and sample size. 
# @param sd: standard_deviation
# @param n: sample size
# @begin standard_error
standard_error <- function(sd=0.0, n=0) { sd/sqrt(n) }
# @end standard_error

# Handles two-sided confidence intervals where the only value had is a confidence level for the interval.
# This is done through default mean and stdev of 0 and 1, respectively.
# Then, it finds the inversecdf with default values and a critical value of ((1 + confidence_level) / 2)
# The latter part is determined through a Z-Table Lookup from qnorm function.
# @param confidence_level
# @begin find_critical_value_with_confidence_level
find_critical_value_with_confidence_level <- function(confidence_level=0.0) {
  mean <- 0
  standard_deviation <- 1
  x <- ((1 + confidence_level)/2)
  qnorm(x, mean, standard_deviation)
}
# @end find_critical_value_with_confidence_level

# Given a confidence level and degrees of freedom, find probability,
# And plug these into qt function to determine critical value from Z-Table
# @param confidence_level
# @param df: degrees of freedom
# @begin find_critical_value_with_df
find_critical_value_with_df <- function(confidence_level=0.0, df=0) {
  x <- ((1 + confidence_level)/2)
  qt(x, df)
}
# @end find_critical_value_with_df


# Given a sample_size, confidence level, and standard deviation,
# determine MOE - Margin of Error of a sample mean.
# And plug these into qt function to determine critical value from Z-Table
# @param sample_size
# @param confidence_level
# @return MOE of mean.
# @begin sd: standard deviation
find_margin_of_error_of_mean <- function(sample_size = 0.0, confidence_level = 0.0, sd = 0.0) {
  alpha <- 1 - confidence_level
  t_critical_value <- alpha / 2
  degrees_of_freedom <- sample_size - 1
  t_actual_value <- -qt(t_critical_value, degrees_of_freedom)
  t_actual_value * sd / sqrt(sample_size)
}
# @end find_margin_of_error_of_mean


# Given a list of values, and a confidence level,
# find a confidence interval.
# @param sample: Vector of Ints.
# @param confidence_level
# @return Upper and Lower Bounds.
# @begin simply_construct_confidence_interval
simply_construct_confidence_interval <- function(sample=c(), confidence_level=0.0) {
  result <- t.test(sample, conf.level = confidence_level)
  print(result$conf.int)
}
# @end simply_construct_confidence_interval

# @param sample: Optional, list of sample values.
# @param standard_deviation: Optional, if sample is provided.
# @param confidence_level.
# @param sample_size: Optional, if sample list is provided.
# @param sample_mean: Optional, if sample list is provided.
# @return Confidence Interval's Upper and Lower bounds.
# Handles constructing a confidence interval of a #true_mean.
# @begin construct_confidence_interval_easy
construct_confidence_interval_easy <- function(sample=list(), standard_deviation=0.0, confidence_level=0.0, sample_size = 0, sample_mean = 0) {
  # Remainder of Confidence Level.
  alpha           <- (1 - confidence_level)

  # Not required but here for clarity.
  degrees         <- {}
  lower           <- {}
  upper           <- {}
  t               <- {}  
  
  if(length(sample) > 0){
    sample.n        <- (length(sample))
    sample.mean     <- (mean(unlist(sample)))  
  } else {
    sample.n <- sample_size
    sample.mean <- sample_mean
  }
  
  # Allow for standard deviation to be provided because it could be different.
  if(standard_deviation != 0.0){ 
    sample.sd     <- (standard_deviation)
  } else {
    sample.sd     <- (sd(unlist(sample)))
  }
  sample.se       <- (sample.sd/sqrt(sample.n))
  degrees.freedom <- (sample.n - 1)
  t.score         <- qt(p = (alpha / 2), df = degrees.freedom, lower.tail = F)
  margin_of_error <- t.score * sample.se
  lower.bound     <- sample.mean - margin_of_error
  upper.bound     <- sample.mean + margin_of_error
  
  print(c(lower.bound, upper.bound))
}
# @end construct_confidence_interval_easy

# @param Float point_estimate Point Estimate Estimate of where the point is.
# @param Float margin_of_error Margin of Error 
# @return estimate_of_confidence_interval
# @begin derive_mean_and_or_expected_value_from_X
derive_mean_and_or_expected_value_from_X <- function(point_estimate, margin_of_error) {
  list((point_estimate - margin_of_error), (point_estimate + margin_of_error))
}
# @end derive_mean_and_or_expected_value_from_X

# @param Numeric sample_mean
# @param Numeric margin_of_error
# @return estimate_of_confidence_interval
# @begin derive_population_mean
derive_population_mean <- function(sample_mean=0, margin_of_error=0, confidence_level=0.0) {
  list((sample_mean - margin_of_error), (sample_mean + margin_of_error))
  print("With")
  print(confidence_level)
  print("confidence")
  print("I know population mean is between these values:")
  print(list((sample_mean - margin_of_error), (sample_mean + margin_of_error)))
}
# @end derive_population_mean

# @param confidence_level
# @param x_lte
# @return z_score where x is less than something.
# @begin find_z_score_where_x_lte
find_z_score_where_x_lte <- function(confidence_level=0, x_lte=0) {
  confidence = 1-confidence_level
  qnorm(1 - confidence / 2)
}
# @end find_z_score_where_x_lte

# @begin find_confidence_interval; √
find_confidence_interval <- function(confidence_level=0, standard_deviation=0, sample_size=0) {
  alpha = 1 - confidence_level
  alpha_left_tail  = alpha/2
  alpha_right_tail = alpha_left_tail
  alpha_mid = 1 - alpha_right_tail
  data = c(
    alpha = alpha,
    alpha_left_tail = alpha_left_tail,
    margin_of_error_left = qnorm(alpha_left_tail) * standard_deviation/sqrt(sample_size),
    alpha_mid = alpha_mid,
    alpha_right_tail = alpha_right_tail,
    margin_of_error_right = -(qnorm(alpha_right_tail) * standard_deviation/sqrt(sample_size))
  )
  print(data)
}
# @end find_confidence_interval
find_confidence_interval(
  confidence_level = 0.90,
  standard_deviation = 3,
  sample_size=10
)

# @begin find_margin_of_error_from_confidence_interval
find_margin_of_error_from_confidence_interval <- function(confidence_interval=list(), xbar=0) {
  value = 0
  if(xbar > 0) {
    value = as.numeric(confidence_interval[2]) - as.numeric(xbar)  
  } else {
    value = (confidence_interval[1] + confidence_interval[2]) / 2
  }
  value
}
# @end find_margin_of_error_from_confidence_interval
find_margin_of_error_from_confidence_interval(confidence_interval = list(67.508, 68.492), xbar=68)

# @begin find_sample_mean_from_confidence_interval
find_sample_mean_from_confidence_interval <- function(confidence_interval=list()) {
  (as.numeric(confidence_interval[2]) + as.numeric(confidence_interval[1])) / 2
}
# @end find_sample_mean_from_confidence_interval
find_sample_mean_from_confidence_interval(confidence_interval = list(42.12, 47.88))

# @begin find_sample_size_for_moe (find n given a MOE and NOT SD)
# assumes we're finding a sample size for a MOE smaller than X.
# @param confidence_level  - estimated success
# @param p_hat             - estimated success
#        q_hat             - estimated failure
# @param moe               - margin of error
find_sample_size_for_moe <- function(confidence_level=0.0, margin_of_error=0.0, p_hat = 0.0) {
  z_alpha <- -qnorm((1-confidence_level)/2)
  q_hat   <- 1 - p_hat
  n       <- (((z_alpha ^ 2) * (p_hat * q_hat))/(margin_of_error ^ 2))
  n
}
# @end find_sample_size_for_moe

# @begin find_sample_size_for_moe (find n given a MOE and with a SD)
# @param confidence_level  - estimated success
# @param sd                - estimated success
# @param moe               - margin of error
# @return sample size, given margin of error.
# @begin find_sample_size_for_moe
find_sample_size_for_moe <- function(confidence_level=0.0, margin_of_error=0.0, sd = 0.0) {
  z_alpha <- -qnorm((1-confidence_level)/2)
  n       <- (((z_alpha ^ 2) * (sd ^ 2))/(margin_of_error ^ 2))
  n
}
# @end find_sample_size_for_moe

# @param z_value
# @param standard_deviation
# @param moe
# @return an optimal sample size
# @begin optimal_sample_size
optimal_sample_size <- function(z_value, standard_deviation, moe) {
  ceiling(((qnorm(z_value) ^ 2) * (standard_deviation ^ 2)) / (moe ^ 2))
}
# @end optimal_sample_size

test_margin_of_error_of_mean <- function() {
  all.equal(find_margin_of_error_of_mean(sample_size=13, confidence_level=0.9, sd=0.03), 0.01482953)
}
test_margin_of_error_of_mean()