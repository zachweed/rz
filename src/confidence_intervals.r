library("ggplot2")

# If population mean is not known and population standard deviation is and sample size is,
#   then (population standard deviation)/square root (sample size) equals #Standard Deviation of Sample Mean
#   and Sample Mean +- 2 Standard Deviations equals #Population Mean

# Confidence Interval Format:
#   Point Estimate +- Margin of Error.

# Given I know a Confidence Interval
# And a Standard Deviation
# And a Sample Size
# Then I can find the Margin of Error

# z(a/2) -> 

# Estimating Population Proportion:
# 1. Given a Population (P) and a Subset (S)
# 2. Find Subset/Population -> P-Hat (proportion of trials)
# 3. 

# Estimating True Mean from Sample Mean:
# 1. If I wasn't provided with Standard Deviation then calculate it from Variance. sqrt(V)
# 2. N=number of samples
# 3. MEAN=Average
# 4. STANDARD_DEVIATION = (SQRT(V) || SD())
# 5. T=ZTable(((1-CONFIDENCE_LEVEL)/2))
# 6. SE=STANDARD_DEVIATION/SQRT(N)
# 7. MEAN +- (T * SE)

# Estimating True Mean from Sample Mean:
# 2. N=11
# 3. MEAN=24.5
# 4. STANDARD_DEVIATION = 2.6
# 5. ZTable(((1-0.8)/2)) -> 0.5793
# 6. SE=STANDARD_DEVIATION/SQRT(N) -> 0.783
# 7. MEAN +- (T * SE) -> 24.6 + (0.5398 * 0.783)
# 8. MOE = (0.5398 * 0.783)/sqrt(11) -> 0.1274
# 9. 24.5 +- 0.1367


# 
# For Example:
# N = 9
# On March 10, 2020, nine NBA games were played. 
#
# MEAN=226
# Average number of points scored in those games was 226, 
#
# SD=11.07
# with a sample variance of 122.5.
# 
# CONFIDENCE_LEVEL=0.90
# 
# ZT = (qnorm(0.1) -> (1.281)
#
# SE=11.07/SQRT(N) -> 3.69
#
# 226 +- (1.0398 * 3.69)
# UPPER = 226 + (1.282 * 3.69) -> 230.7306
# LOWER = 226 - (1.282 * 3.69) -> 221.2694

# Confidence Intervals:
#   0. Components:
#     a. Standard Deviation -> i.e., square root of variance σ
#     b. Variance -> i.e., standard deviation squared σ
#   1. If I have a sample size "η", a sample average "μ", and a standard deviation "s"
#   i. standard error cum ("s"/sqrt(η)).
#  ii. α cum difference (1 & confidence-level).
# iii. ζ(α over 2) -> crit
#  iv. answer cum (mean +- ζ(α over 2)) * (standard error).
#
#   2. e.g.:
#        i. 
#         i. Random Sample of 60 houses, Sample Average 161.61, Standard Deviation 12.83
#         ii. Find 95% confidence interval of true mean.
#        ii. standard_error == ((12.83 / sqrt(60)) == 1.656)
#       iii. (α == ((1 - 0.95) == 0.05)) -> (crit == 1.1974)
#        iv. sample mean +- (crit) * (standard error))
#         a. 161.61 +- (1.1974 * 1.656)
#          i. (161.61 - (1.1974 * 1.656) == 159.6271)
#         ii. (161.61 + (1.1974 * 1.656) == 163.5929)
#        ii. (159.62171 - 163.5929) 

# Really all that's needed is list and confidence level. +- 0.000n.
# Seems to be more accurate than manually building confidence interval
# Works With:
#   1. Professional tennis player John Isner is known for having a fast serve; his fastest recorded serve ever is 157.2 miles per hour. A sample of 10 of Isner’s serves is collected, and you observe the following speeds in miles per hour: 142, 143, 143, 144, 146, 146, 146, 147, 147, and 150. The standard deviation of his serve speed is known to be  = 3.
#      Assuming that the distribution of his serve speed follows a normal distribution, construct a 90% confidence interval for the true mean of his serve speed.
# 
simply_construct_confidence_interval <- function(sample=c(), confidence_level=0.0) {
  result <- t.test(sample, conf.level = confidence_level)
  print(result$conf.int)
}
#simply_construct_confidence_interval(
#  sample = c(142, 143, 143, 144, 146, 146, 146, 147, 147, 150),
#  confidence_level = 0.9
#)

#####
# e.g., True average / acre private island from list and confidence level.
simply_construct_confidence_interval(
  sample = c(173503, 311219, 137943, 551198, 336016, 140520, 362863, 405515),
  confidence_level = 0.9
)
# Returns a Lower and Upper bound.
#####

#####
# e.g., avg. length of movie.
simply_construct_confidence_interval(
  sample = c(79,90,94,95,102,102,107,117),
  confidence_level = 0.85
)
# Returns a Lower and Upper bound.
#####

#####
# e.g., Bacterial average cum Confidence Interval 95% E. Coli.
simply_construct_confidence_interval(
  sample = c(118,175,190,206,207,208,249,256,260,289),
  confidence_level = 0.95
)
# Returns a Lower and Upper bound.
#####

# Minimum Requirements:
#   1. A sample list of data
#   2. Standard Deviation
#   3. Confidence Level
construct_confidence_interval_easy <- function(sample=list(), standard_deviation=0.0, confidence_level=0.0) {
  # Remainder of Confidence Level.
  alpha           <- (1 - confidence_level)

  # Not required but here for clarity.
  degrees         <- {}
  lower           <- {}
  upper           <- {}
  t               <- {}  

  sample.n        <- (length(sample))
  sample.mean     <- (mean(unlist(sample)))
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
construct_confidence_interval_easy(
  sample = list(142, 143, 143, 144, 146, 146, 146, 147, 147, 150),
  standard_deviation=3,
  confidence_level=0.90
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

# @begin find_confidence_interval
find_confidence_interval <- function(confidence_level=0, sample_mean=0, sample_size=0, standard_deviation=0) {
  lower = qnorm((1-confidence_level) / 2, sample_mean, standard_deviation/sqrt(sample_size))
  upper = qnorm(confidence_level / 2, sample_mean, standard_deviation/sqrt(sample_size))
  list(lower, upper)
}
find_confidence_interval(confidence_level=0.99, sample_mean=11.81, sample_size=52, standard_deviation=3.17)
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
find_confidence_interval(confidence_level=0.90, standard_deviation=0.2, sample_size=200)

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
