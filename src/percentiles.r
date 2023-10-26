# Handle Percentile Specific Logic

# @begin percentileForSampleMean
percentileForSampleMean <- function(k=0.0, mu=0.0, standard_deviation=0.0, sample_size=0.0) {qnorm(k, mu, standard_deviation/sqrt(sample_size))}
# @end percentileForSampleMean

# @begin pOfXLessThan
# validate in https://www.desmos.com/calculator/rephn2eswe
pOfXLessThan <- function(mu=0.0, standard_deviation=0.0, greaterThanWhat=0.0, sample_size=0.0){pnorm(greaterThanWhat, mu, standard_deviation/sqrt(sample_size))}
pOfXLessThan(mu=34, standard_deviation=15, greaterThanWhat=30, sample_size=100)
# @end pOfXLessThan

# @begin pOfXGreaterThan
# validate in https://www.desmos.com/calculator/rephn2eswe
pOfXGreaterThan <- function(mu=0.0, standard_deviation=0.0, greaterThanWhat=0.0, sample_size=0.0){ 1 - pnorm(greaterThanWhat, mu, standard_deviation/sqrt(sample_size))}
pOfXGreaterThan(mu=34, standard_deviation=15, greaterThanWhat=30, sample_size=100)
# @end pOfXGreaterThan

# @begin mean
# Can be used to plug into qnorm for percentiles
mean <- function(upper_bound=0, lower_bound=0, sample_size=0) {sample_size * ((upper_bound+lower_bound)/2)}
# @end mean

# @begin sample_standard_deviation
# σχ
sample_standard_deviation <- function(standard_deviation = 0.0, sample_size = 0.0) {standard_deviation/sqrt(sample_size)}
# @end sample_standard_deviation

# @begin standard_deviation
standard_deviation <- function(upper_bound=0, lower_bound=0, sample_size=0) {
  sqrt(sample_size) * sqrt(((upper_bound-lower_bound)^2)/12)
}
# @end standard_deviation

# @begin what_is_nth_percentile
# Given mean and standard deviation, find qnorm.
what_is_nth_percentile <- function(nth_percentile=0.0, mean=0.0, standard_deviation=0.0, sample_size=0.0){
  qnorm(nth_percentile, mean, standard_deviation/sqrt(sample_size))
}
what_is_nth_percentile(0.33, 40, 3, 100)
# @end what_is_nth_percentile

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

# @return IQR
# @begin inter_quartile_range
inter_quartile_range <- function(list) {
  IQR(list)
}
# @end inter_quartile_range
