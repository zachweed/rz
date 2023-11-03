################################
# CLT for Summed Distributions #
################################

# @param from
# @param to
# @param standard_deviation
# @param mean
# @param sample_size
# @begin probability_of_ex_between_two_values
probability_of_ex_between_two_values <- function(from=0.0, to=0.0, standard_deviation=0.0, mean=0.0, sample_size=0.0) {
  pnorm(to, sample_size * mean, (sqrt(sample_size)* standard_deviation)) - 
  pnorm(from, sample_size * mean, (sqrt(sample_size)* standard_deviation))
}
# @end probability_of_ex_between_two_values

# @param from: from what
# @param sample_standard_deviation
# @param sample_mean
# @param sample_size
# @begin what_is_probability_of_ex_above_value
what_is_probability_of_ex_above_value <- function(from=0.0, sample_standard_deviation=0.0, sample_mean=0.0, sample_size=0.0) {
  mean <- deriveMeanForSums(sample_size, sample_mean)
  sde <- (sqrt(sample_size) * sample_standard_deviation)
  1 - pnorm(from, mean, sde)
}
# @end what_is_probability_of_ex_above_value

# @param k: percentile
# @param sample_mean
# @param sample_standard_deviation
# @param sample_size
# @begin what_is_percentile_given_sample_mean
what_is_percentile_given_sample_mean <- function(k=0.0, sample_mean=0.0, sample_standard_deviation=0.0, sample_size=0.0) {qnorm(k, sample_mean * sample_size, sqrt(sample_size) * sample_standard_deviation)}
# @end what_is_percentile_given_sample_mean

# @param sample_size
# @param sample_mean
# @begin deriveMeanForSums
derive_mean_for_sums <- function(sample_size=0.0, sample_mean=0.0) { sample_size * sample_mean }
# @end deriveMeanForSums

# @param sample_size
# @param sample_sd
# @begin derive_standard_deviation_for_sums
derive_standard_deviation_for_sums <- function(sample_size=0.0, sample_sd=0.0) { sqrt(sample_size) * sample_sd }
# @end derive_standard_deviation_for_sums


