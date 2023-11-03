###################################
# Expected Value Helper Functions #
###################################

# @param probabilities: Vector of probability values.
# @return Expected Value Given a Probability Mass Function
# aka: long-term average
# aka: sum(all averages)
# @begin expected_value
expected_value <- function(probabilities) {
  sum = 0
  iteration = 1
  for(probability in probabilities) {
    sum = sum + (iteration * probability)
    iteration = iteration + 1
  }
  sum
}
# @end expected_value
