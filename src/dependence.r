#################################
# Working with dependent events #
#################################

# @return Probability of an unknown from knowns.
# Given knowns, find probability of an unknown
# Through finding complements.
# e.g., If we know one thing, and another thing given the initial thing,
#        what is the probability of a false positive?
#
# @begin findUnknown
findUnknown <- function(someProbability, someOtherProbability, someInverseProbability, someOtherInverseProbability) {
  (someProbability * someOtherProbability) + (someInverseProbability * someOtherInverseProbability)
}
# @end findUnknown

# Determine if they are equal.
# @param probability_of_a Float -> probability of one event.
# @param probability_of_b Float -> probability of another event.
# @param probability_of_both Float -> probability of both events happening.
# @return Boolean if events are independent.
# @begin is_independent
is_independent <- function(probability_of_a, probability_of_b, probability_of_both) {
  print(c(probability_of_a * probability_of_b))
  print(c(probability_of_both))
}
is_independent(0.2, 0.2, 0.04)
# @end is_independent

# @param a - P
# @param b - P
# @param both - P
# @return Boolean if events are dependent.
# @begin is_dependent
is_dependent <- function(a, b, both) {
  is_independent(a, b, both) != TRUE
}
# @end is_dependent

# @param probability_of_a
# @param probability_of_b
# @param probability_of_both
# @return Boolean if events are dependent.
# @begin is_mutually_exclusive_with_both
is_mutually_exclusive_with_both <- function(probability_of_a, probability_of_b, probability_of_both) {
  all.equal((probability_of_a + probability_of_b), probability_of_both) 
}
# @end is_mutually_exclusive_with_both

# @param all_options: Vector[Vector[Int]]
# @begin is_mutually_exclusive_without_both
is_mutually_exclusive_without_both <- function(all_options) {
  setdiff(all_options[1], all_options[2])
}
# @end is_mutually_exclusive_without_both
