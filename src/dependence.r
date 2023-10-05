# @return Probability of an unknown from knowns.
# Given knowns, find probability of an unknown
# Through finding complements.
# e.g., If we know one thing, and another thing given the initial thing,
#        what is the probability of a false positive?
# 
findUnknown <- function(
                 someProbability, 
                 someOtherProbability, 
                 someInverseProbability, 
                 someOtherInverseProbability
               ) {
  (someProbability * someOtherProbability) + (someInverseProbability * someOtherInverseProbability)
}
A=0.12; B=0.8; C=(1-A); D=(1-B)
findUnknown(A,B,C,D)

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

# @return Boolean if events are dependent.
# @begin is_dependent
is_dependent <- function(a, b, both) {
  is_independent(a, b, both) != TRUE
}
is_dependent((5/6), (3/6), 1)
# @end is_dependent

# @return Boolean if events are dependent.
# @begin is_mutually_exclusive_with_both
is_mutually_exclusive_with_both <- function(probability_of_a, probability_of_b, probability_of_both) {
  all.equal((probability_of_a + probability_of_b), probability_of_both) 
}
is_mutually_exclusive_with_both(0.15, 0.05, 0.18)
# @end is_mutually_exclusive_with_both

# @begin is_mutually_exclusive_without_both
is_mutually_exclusive_without_both <- function(all_options) {
  setdiff(all_options[1], all_options[2])
}
A = c(5)
B = c(1,3,5)
O = list(A, B)
is_mutually_exclusive_without_both(O) != A[1]
# @end is_mutually_exclusive_without_both
