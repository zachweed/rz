# @return Expected Value Given a Probability Mass Function
# aka: long-term average
# aka: sum(all averages)
# @begin expectedValue
expected_value <- function(probabilities) {
  sum = 0
  iteration = 1
  for(probability in probabilities) {
    sum = sum + (iteration * probability)
    iteration = iteration + 1
  }
  sum
}
expected_value(
  c(
    0.2, 
    0.4,
    0.3,
    0.1
   )
)
# @end expectedValue
