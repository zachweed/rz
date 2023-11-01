# Normal distributions

# @begin standard_error_of_mean
standard_error_of_mean <- function(mean=0, standard_deviation=0, n=0) {
  standard_deviation/sqrt(n)
}
standard_error_of_mean(mean=5, standard_deviation=20, n=45)
# @end standard_error_of_mean

# How many Standard Deviations to the Left/Right.
# (Z = (x - mu)/sigma)
# @begin standard_deviation
n_sdevs_from_mean <- function(mean=0, standard_deviation=0, n=0) {
  mean + (n * standard_deviation)
}
n_sdevs_from_mean(mean=120, standard_deviation=25, n=125)
# @end standard_deviation

# How many Standard Deviations to the Left/Right.
# What percentile between values, given sd and mean.
# (Z = (x - mu)/sigma)
# @begin z_score
z_score <- function(random_var=0, mean=0, standard_deviation=0) {
  (random_var-mean)/standard_deviation
}
z_score(random_var=0, mean=120, standard_deviation=625)
# @end z_score

percentile_from_z_score <- function(mean=0, standard_deviation=0, z_score=0) {
  # i.e. mu, + z_score * sd
  (mean + ((z_score) * standard_deviation))
}
random_variable_for_z_score(mean=, standard_deviation=6.28, z_score=1.27)

# Positive:
#   If mean height is 172.36cm, and standard_deviation is 6.34cm, 
#   And about 68% of the y values are between 166.02 and 178.7 cm
#   The z-scores are (____)
test_what_is_z_score <- function() {
  all.equal(z_score(random_var=166.02, mean=172.36, standard_deviation=6.34), -1)
  all.equal(z_score(random_var=178.7, mean=172.36, standard_deviation=6.34), 1)
}
test_what_is_z_score()


# Positive:
#   If someone can lose and average of 5 pounds, 
#   With a standard deviation of 2 points,
#   If they lost 10 points in a month, z_score is 2.5
test_what_is_z_score <- function() {
  all.equal(z_score(random_var=10, mean=5, standard_deviation=2), 2.5)
}
test_what_is_z_score()

# Negative:
#   If someone can lose and average of 5 pounds, 
#   With a standard deviation of 2 points,
#   If they gained 3 points in a month, z_score is 4 deviations to left.
test_what_is_z_score <- function() {
  all.equal(z_score(10, 5, 2), 2.5)
}
test_what_is_z_score()