# Gotchas: 
# 1. Percentiles for Sample Means in Sums are different than overarching percentiles

# Random Variable EX is a sum and can be approximated with a normal distribution.
#   Traits: 
#     Mean of EX:
#       (sample size)(mu)
#     Standard Deviation of EX:
#       sqrt(sample size) * (standard deviation)
#     Z-Score
#       (EX-( (sample size) * (mean * x) ) /( (standard deviation * x) (sqrt(n) )
#
# 1. Find Mean of Sample Distribution of Sums (mu * x)
# 2. Calculate Standard Deviation of Sample Distribution of sums (sigma_x)
# 3. Z-transform mean and standard deviation
# 4. Plug in Z-value from Z-transformation to find probability.
#
# e.g., if 40 values were in a population with mean 50 and standard deviation 2,
#       what is probability of sum being between 2000 and 2080?
# i.e., what is P(2000 < EX < 2080)?
# is: probability_of_ex_between_two_values(from=2000, to=2080, standard_deviation=2, mean=50, sample_size=40)

# Where EX is sum of a random value that we need to solve for.
probability_of_ex_between_two_values <- function(from=0.0, to=0.0, standard_deviation=0.0, mean=0.0, sample_size=0.0) {
  pnorm(to, sample_size * mean, (sqrt(sample_size)* standard_deviation)) - 
  pnorm(from, sample_size * mean, (sqrt(sample_size)* standard_deviation))
}
probability_of_ex_between_two_values(from=1500, to=1800, standard_deviation=15, mean=34, sample_size=49)

# @begin whatIsProbabilityOfExAboveValue
# @param from: from what value? if finding a sum, make sure to work all math.
whatIsProbabilityOfExAboveValue <- function(from=0.0, sample_standard_deviation=0.0, sample_mean=0.0, sample_size=0.0) {
  mean <- deriveMeanForSums(sample_size, sample_mean)
  sde <- (sqrt(sample_size) * sample_standard_deviation)
  1 - pnorm(from, mean, sde)
}
whatIsProbabilityOfExAboveValue(
  from=(11 * 60),
  sample_standard_deviation=1, 
  sample_mean=10,
  sample_size=64
)
# @end whatIsProbabilityOfExAboveValue

# @begin whatIsPercentileGivenSampleMean
whatIsPercentileGivenSampleMean <- function(k=0.0, sample_mean=0.0, sample_standard_deviation=0.0, sample_size=0.0) {qnorm(k, sample_mean * sample_size, sqrt(sample_size) * sample_standard_deviation)}
# @end whatIsPercentileGivenSampleMean
whatIsPercentileGivenSampleMean(
  k=0.95, 
  sample_mean=10, 
  sample_standard_deviation=1,
  sample_size=64
)

# @begin deriveMeanForSums
# i.e. what is summed mean? 
# μ_Σχ
deriveMeanForSums <- function(sample_size=0.0, sample_mean=0.0) { sample_size * sample_mean }
# @end deriveMeanForSums

# @begin deriveStandardDeviatinForSums
# σ_Σχ
deriveStandardDeviatinForSums <- function(sample_size=0.0, sample_sd=0.0) { sqrt(sample_size) * sample_sd }
# @end deriveStandardDeviatinForSums


