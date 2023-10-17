# Normal distributions

# How many Standard Deviations to the Left/Right.
z_score <- function(random_var=0, mean=0, standard_deviation=0) {
  (random_var-mean)/standard_deviation
}
z_score(random_var=191.38, mean=172.36, standard_deviation=6.34)

random_variable_for_z_score <- function(mean=0, standard_deviation=0, z_score=0) {
  (mean + (z_score) * standard_deviation)
}
random_variable_for_z_score(mean=170, standard_deviation=6.28, z_score=1.27)

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
