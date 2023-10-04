# @begin what_is_variance_of
what_is_variance_of <- function(value) {
  value * value
}
# @end what_is_variance_of
what_is_variance_of(33.4)

# @begin what_is_variance_of_pmf
what_is_variance_of_pmf <- function(list_of_probabilities) {
  what_is_expected_value_of_pmf(list_of_probabilities = list_of_probabilities, squared_y = TRUE) - what_is_expected_value_of_pmf(list_of_probabilities = list_of_probabilities, squared_result = TRUE)
}
# @end what_is_variance_of_pmf
what_is_variance_of_pmf(
  list_of_probabilities = list(
    c(0.4, 1),
    c(0.3, 10),
    c(0.3, 100)
  )
)
