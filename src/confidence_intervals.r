library("ggplot2")

# Given a mean of 12 fl. oz. & SD of 0.1oz,
# Then what is the probability of 35 randomly chosen being below 12.05?

# Handles two-sided confidence intervals where the only value
# had is a confidence level for the interval.
find_critical_value_with_confidence_level <- function(confidence_level=0.0) {
  mean <- 0
  standard_deviation <- 1
  x <- ((1 + confidence_level)/2)
  qnorm(x, mean, standard_deviation)
}
find_critical_value(confidence_level = 0.99)

# Handles Two-Sided Confidence Intervals
find_critical_value_with_df <- function(confidence_level=0.0, df=0) {
  x <- ((1 + confidence_level)/2)
  qt(x, df)
}
find_critical_value_with_df(confidence_level = 0.96, df=20)

find_margin_of_error_of_mean <- function(sample_size = 0.0, confidence_level = 0.0, sd = 0.0) {
  alpha <- 1 - confidence_level
  t_critical_value <- alpha / 2
  degrees_of_freedom <- sample_size - 1
  t_actual_value <- -qt(t_critical_value, degrees_of_freedom)
  t_actual_value * sd / sqrt(sample_size)
}
find_margin_of_error_of_mean(sample_size = 80, confidence_level = 0.80, sd = 1)


# @begin simply_construct_confidence_interval
simply_construct_confidence_interval <- function(sample=c(), confidence_level=0.0) {
  result <- t.test(sample, conf.level = confidence_level)
  print(result$conf.int)
}
# @end simply_construct_confidence_interval

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
construct_confidence_interval_easy(
  standard_deviation=1, confidence_level=0.80, sample_size = 80, sample_mean = 30/80
)

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

# @begin moe
moe <- function(z_value, alpha, standard_deviation) {
  (qnorm(zvalue) * standard_deviation/sqrt(sample_size))
}
# @end moe

# @begin z_value
z_value <- function(confidence_level=0){
 (1-confidence_level)/2 
}
# @end z_value

# @begin optimal_sample_size
optimal_sample_size <- function(z_value, standard_deviation, moe) {
  ceiling(((qnorm(z_value) ^ 2) * (standard_deviation ^ 2)) / (moe ^ 2))
}
# @end optimal_sample_size
optimal_sample_size(0.025, 15, 2)
