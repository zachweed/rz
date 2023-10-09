# @param Float point_estimate Point Estimate Estimate of where the point is.
# @param Float margin_of_error Margin of Error 
# @return estimate_of_confidence_interval
# @begin estimate_confidence_interval
derive_mean_and_or_expected_value_from_X <- function(point_estimate, margin_of_error) {
  list((point_estimate - margin_of_error), (point_estimate + margin_of_error))
}
# @end derive_mean_and_or_expected_value_from_X
derive_mean_and_or_expected_value_from_X

# @param Numeric sample_mean
# @param Numeric margin_of_error
# @return estimate_of_confidence_interval
# @begin derive_population_mean
derive_population_mean <- function(sample_mean=0, margin_of_error=0, confidence_level=0.95) {
  list((sample_mean - margin_of_error), (sample_mean + margin_of_error))
  print("With")
  print(confidence_level)
  print("confidence")
  print("I know population mean is between these values:")
  print(list((sample_mean - margin_of_error), (sample_mean + margin_of_error)))
}
# @end derive_population_mean
derive_population_mean(sample=15, margin_of_error=3.2)
