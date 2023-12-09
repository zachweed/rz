########################
# Normal distributions #
########################

# @param q Vector[Float] quantiles
# @param mean -> default value 0
# @param sd -> default value 1
normal_distribution <- function (q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) {
  pnorm(q, mean = mean, sd = sd, lower.tail = lower.tail, log.p = log.p)
}

# @param q Vector[Float] quantiles
# @param mean -> default value 0
# @param sd -> default value 1
quantile_normal_distribution <- function (p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) {
  qnorm(q, mean = mean, sd = sd, lower.tail = lower.tail, log.p = log.p)
}

# @return sample size from mean, se, and sd.
# @begin sample_size
sample_size <- function(mean=0, standard_deviation=0, standard_error=0) {
  (standard_deviation/standard_error)^2
}
# @end sample_size

# @begin standard_error_of_mean
standard_error_of_mean <- function(mean=0, standard_deviation=0, n=0) { standard_deviation/sqrt(n) }
# @end standard_error_of_mean

# How many Standard Deviations to the Left/Right.
# (Z = (x - mu)/sigma)
# @begin n_sdevs_from_mean
n_sdevs_from_mean <- function(mean=0, standard_deviation=0, n=0) { mean + (n * standard_deviation) }
# @end n_sdevs_from_mean

# How many Standard Deviations to the Left/Right.
# What percentile between values, given sd and mean.
# (Z = (x - mu)/sigma)
# @begin z_score
z_score <- function(random_var=0, mean=0, standard_deviation=0) {
  (random_var-mean)/standard_deviation
}
z_score(random_var=0, mean=120, standard_deviation=625)
# @end z_score

# @param mean
# @param standard_deviation
# @param z_score
# @begin percentile_from_z_score
percentile_from_z_score <- function(mean=0, standard_deviation=0, z_score=0) { (mean + ((z_score) * standard_deviation)) }
# @end percentile_from_z_score

test_what_is_z_score <- function() {
  all.equal(z_score(random_var=166.02, mean=172.36, standard_deviation=6.34), -1)
  all.equal(z_score(random_var=178.7, mean=172.36, standard_deviation=6.34), 1)
}
test_what_is_z_score()

test_what_is_z_score <- function() {
  all.equal(z_score(random_var=10, mean=5, standard_deviation=2), 2.5)
}
test_what_is_z_score()

test_what_is_z_score <- function() {
  all.equal(z_score(10, 5, 2), 2.5)
}
test_what_is_z_score()