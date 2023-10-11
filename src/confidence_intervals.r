# @param Float point_estimate Point Estimate Estimate of where the point is.
# @param Float margin_of_error Margin of Error 
# @return estimate_of_confidence_interval
# @begin estimate_confidence_interval
derive_mean_and_or_expected_value_from_X <- function(point_estimate, margin_of_error) {
  list((point_estimate - margin_of_error), (point_estimate + margin_of_error))
}
# @end derive_mean_and_or_expected_value_from_X
derive_mean_and_or_expected_value_from_X

# @param Numeric sample_mean
# @param Numeric margin_of_error
# @return estimate_of_confidence_interval
# @begin derive_population_mean
derive_population_mean <- function(sample_mean=0, margin_of_error=0, confidence_level=0.95) {
  list((sample_mean - margin_of_error), (sample_mean + margin_of_error))
  print("With")
  print(confidence_level)
  print("confidence")
  print("I know population mean is between these values:")
  print(list((sample_mean - margin_of_error), (sample_mean + margin_of_error)))
}
# @end derive_population_mean
derive_population_mean(sample=15, margin_of_error=3.2)

# @begin find_z_score_where_x_lte
find_z_score_where_x_lte <- function(confidence_level=0, x_lte=0) {
  confidence = 1-confidence_level
  qnorm(1 - confidence / 2)
}
# @end find_z_score_where_x_lte
find_z_score_where_x_lte(0.95) 

# @begin find_confidence_interval
find_confidence_interval <- function(confidence_level=0, sample_mean=0, sample_size=0, standard_deviation=0) {
  lower = qnorm((1-confidence_level) / 2, sample_mean, standard_deviation/sqrt(sample_size))
  upper = qnorm(confidence_level / 2, sample_mean, standard_deviation/sqrt(sample_size))
  list(lower, upper)
}
# @end find_confidence_interval
find_margin_of_error(confidence_level=0.95, sample_mean=10, sample_size=100, standard_deviation=3)

# @begin find_confidence_interval_without_mean
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
# @end find_confidence_interval_without_mean
find_confidence_interval(confidence_level=0.90, standard_deviation=0.2, sample_size=20)

# @begin find_confidence_interval_without_mean
find_confidence_interval <- function(confidence_level=0, standard_deviation=0, sample_size=0, xbar = 0) {
  alpha = 1 - confidence_level
  left_tail  = alpha/2
  right_tail = left_tail
  mid = 1 - right_tail
  critical_value = round(qnorm(left_tail), 2)
  margin_of_error_left = critical_value * standard_deviation/sqrt(sample_size)
  margin_of_error_right = -critical_value * standard_deviation/sqrt(sample_size)
  data = c(
    alpha = alpha,
    critical_value = critical_value,
    left_tail = left_tail,
    margin_of_error_left = margin_of_error_left,
    lower = xbar + margin_of_error_left,
    mid = mid,
    right_tail = right_tail,
    margin_of_error_right = margin_of_error_left,
    upper = xbar + margin_of_error_right
  )

  print(data)
}
# @end find_confidence_interval_without_mean
find_confidence_interval(confidence_level=0.95, standard_deviation=3, sample_size=36, xbar=68)

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

