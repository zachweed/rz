# This file can:
#   1. Calculate Standard Deviation for sample mean of ____.
#   2. Describe Distribution.
#   3. Find probability of sample age being above ____.
#   4. Find nth percentile.

percentileForSampleMean <- function(k=0.0, mu=0.0, standard_deviation=0.0, sample_size=0.0) {qnorm(k, mu, standard_deviation/sqrt(sample_size))}
percentileForSampleMean(k=0.95, mu=34, standard_deviation=15, sample_size=100)
percentileForSampleMean(k=0.30, mu=3, standard_deviation=2, sample_size=40)

# validate in https://www.desmos.com/calculator/rephn2eswe
pOfXLessThan <- function(mu=0.0, standard_deviation=0.0, greaterThanWhat=0.0, sample_size=0.0){pnorm(greaterThanWhat, mu, standard_deviation/sqrt(sample_size))}
pOfXLessThan(mu=34, standard_deviation=15, greaterThanWhat=30, sample_size=100)

# validate in https://www.desmos.com/calculator/rephn2eswe
pOfXGreaterThan <- function(mu=0.0, standard_deviation=0.0, greaterThanWhat=0.0, sample_size=0.0){ 1 - pnorm(greaterThanWhat, mu, standard_deviation/sqrt(sample_size))}
pOfXGreaterThan(mu=34, standard_deviation=15, greaterThanWhat=30, sample_size=100)

# Can be used to plug into qnorm for percentiles
mean <- function(upper_bound=0, lower_bound=0, sample_size=0) {sample_size * ((upper_bound+lower_bound)/2)}

# σχ
sample_standard_deviation <- function(standard_deviation = 0.0, sample_size = 0.0) {
  standard_deviation/sqrt(sample_size)
}
sample_standard_deviation(standard_deviation=15, sample_size=100)
sample_standard_deviation(standard_deviation=15, sample_size=100)

# Can be used to plug into qnorm for percentiles
standard_deviation <- function(upper_bound=0, lower_bound=0, sample_size=0) {
  sqrt(sample_size) * sqrt(((upper_bound-lower_bound)^2)/12)
}

# Given mean and standard deviation, find qnorm.
what_is_nth_percentile <- function(nth_percentile=0.0, mean=0.0, standard_deviation=0.0, sample_size=0.0){
  qnorm(nth_percentile, mean, standard_deviation/sqrt(sample_size))
}
what_is_nth_percentile(0.33, 40, 3, 100)

# Lower Bound -> 1
# Upper Bound -> 11
# After 40 days, what's the 99th percentile?
#       ^ sample size
percentile_for(
  0.75, 
  mean=mean(upper_bound=11, lower_bound=1, sample_size=40),
  standard_deviation=standard_deviation(upper_bound=11, lower_bound=1, sample_size=40)
)

# What I know
# Lower Bound 1
# Upper Bound 11
# Sample Size 40 days

# What I don't know yet
# 85th Percentile.

# How
# Find mean and standard deviation and then find 85th percentile
percentile_for(
  0.85, 
  mean=mean(upper_bound=11, lower_bound=1, sample_size=40),
  standard_deviation=standard_deviation(upper_bound=11, lower_bound=1, sample_size=40)
)

# What I know
# Lower Bound 1
# Upper Bound 11
# Sample Size 40 days

# What I don't know yet
# 80th Percentile.

# How
# Find mean and standard deviation and then find 85th percentile
percentile_for(
  0.80,
  mean=mean(upper_bound=11, lower_bound=1, sample_size=40),
  standard_deviation=standard_deviation(upper_bound=11, lower_bound=1, sample_size=40)
)

# @begin percentile_from_z_score
# If Z_score isn't provided, find closest value
# to percentile in z-table and convert.
percentile_from_z_score <- function(mean=0, standard_deviation=0, z_score=0) {
  # i.e. mu, + z_score * sd
  (mean + ((z_score) * standard_deviation))
}
percentile_from_z_score(mean=45000, standard_deviation=10000, z_score=1.28)
# @end percentile_from_z_score

# @return What is Nth percentile of a list?
# @param Vector a list of values 
# @begin percentiles
percentiles <- function(list, whatPercentile) {
  list = sort(list)
  L = (whatPercentile / 100) * length(list)
  print(L)
  L = ceiling(L)
  list[L]
}
# @end percentiles
percentiles(c(101, 130, 156, 165, 251, 277, 311, 324, 339, 404), 30)

# @return IQR
# @begin inter_quartile_range
inter_quartile_range <- function(list) {
  IQR(list)
}
# @end inter_quartile_range

interQuartileRange(c(68, 68, 71, 72, 75, 78, 79, 80, 80, 90, 92, 92, 95, 97, 99, 100))
percentiles(c(101, 130, 156, 165, 251, 277, 311, 324, 339, 404), 30)
