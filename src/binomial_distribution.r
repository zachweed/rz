#####################################
# 0. Binomial Distribution.          #
# 1. Application thereof.           #
# 3. Distribution between values.   #
#####################################

# @param Numeric number_of_trials in a binomial random variable
# @return mean of a random variable.
# @begin derive_mean_and_or_expected_value_from_X
derive_mean_and_or_expected_value_from_X <- function(number_of_trials, probability_of_success) {
  number_of_trials * probability_of_success
}
# @end derive_mean_and_or_expected_value_from_X
derive_mean_and_or_expected_value_from_X(252, (1/6))

# @param Numeric number_of_trials in a binomial random variable
# @return mean of a random variable.
# @begin derive_mean_and_or_expected_value_from_X
find_standard_deviation_of_a_binomial_random_variable <- function(number_of_trials, probability_of_success, probability_of_failure, number_of_outcomes) {
  ret = 0
  if(number_of_outcomes == 1){
    ret = number_of_trials * probability_of_success * (1-probability_of_success)  
  } else {
    ret = number_of_trials * probability_of_success * probability_of_failure
  }
  ret
}
# @end derive_mean_and_or_expected_value_from_X
derive_mean_and_or_expected_value_from_X(30, 0.1)
find_standard_deviation_of_a_binomial_random_variable(252, (1/6), (5/6), 6)

# Given a test of probability of an event occurring within an interval,
# And the probability of an outcome one time is known
# And the number of attempts
# Then return the probability of a successful outcome (outcome meets expectation).
# This happens by subtracting two binomial distributions from eachother.
# @param Numeric between_first_number left_hand of interval
# @param Numeric between_second_number right_hand of interval
# @param Numeric number_of_samples how many attempts?
# @param Numeric probability probability of outcome
# @return binomial distribution of something happening at least x times.
# @begin what_is_probability_of_success
what_is_probability_of_success <- function(between_first_number=0, between_second_number=0, number_of_samples=0, probability=0) {
  pbinom(between_second_number, number_of_samples, probability) - pbinom((between_first_number - 1), number_of_samples, probability)
}
# @end what_is_probability_of_success
what_is_probability_of_success(
  between_first_number=0,
  between_second_number=20, 
  number_of_samples=43, 
  probability=0.315
)

# Find exact probability of a random variable X in binomial distribution.
# @param n Vector of Probabilities
# @param lte # of trials
# @param p probability of success
# @return binomial distribution of something happening N times.
# @begin what_is_p_of_x
what_is_p_of_x_when_equals <- function(n = 0, eq = 0, p = 0) { dbinom(eq, size = n, prob = p) }
# @end what_is_p_of_x
what_is_p_of_x_when_equals(n = 15, eq = 7, p = 0.6)

# Find the probability of a random variable X being equal to an exact value.
# @param n Vector of Probabilities
# @param lte # of trials
# @param p probability of success
# @return binomial distribution of something happening at least x times.
# @begin what_is_p_of_x_when_lte
what_is_p_of_x_when_lte <- function(n = 0, lte = 0, p = 0) { pbinom(lte, size = n, prob = p) }
# @end what_is_p_of_x_when_lte
what_is_p_of_x_when_lte(n = 17, lte = 7, p = 0.4)

# Find the probability of a random variable X having an outcome between A and B times.
# @param from At least how many successes
# @param to At most how many successes
# @param trials How many trials total?
# @param p How probable is success?
# @return binomial distribution of something happening at least x times.
# @begin what_is_ranged_probability
what_is_ranged_probability <- function(from = 0, to = 0, trials = 0, p = 0) {
  print(pbinom(to, size = trials, prob = p) - pbinom((from - 1), size = trials, prob = p))
} 
# @end what_is_ranged_probability
what_is_ranged_probability(from = 11, to = 20, trials = 44, p = 0.315)


###################################
#           Tests                 #
###################################
test_what_is_p_of_x <- function() {
  all.equal(what_is_p_of_x(15, 3, 0.6), 0.002)
}
test_what_is_p_of_x()

test_what_is_ranged_probability <- function() {
  all.equal(what_is_ranged_probability(from = 11, to = 20, trials = 35, p = 0.295), 0.4638669)
}
test_what_is_ranged_probability()
