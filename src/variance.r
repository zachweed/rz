# @begin to_sd
to_sd <- function(variance=0) {
  sqrt(variance)
}
# @end to_sd
to_sd(variance=81)

# @begin what_is_standard_deviation_of
#   e.g., uniform distribution,
what_is_standard_deviation_of <- function(a=0, b=0) {
  sqrt(((b-a)^2)/12)
}
# @end what_is_variance_of
what_is_standard_deviation_of(a=2, b=4)

# @begin what_is_variance_of
#   e.g., uniform distribution,
what_is_variance_of <- function(a=0, b=0) {
  ((b-a)^2)/12
}
# @end what_is_variance_of
what_is_variance_of(a=3, b=7)

# @begin what_is_variance_of_pmf
# what_is_variance_of_pmf <- function(list_of_probabilities) {
#  what_is_expected_value_of_pmf(list_of_probabilities = list_of_probabilities, squared_y = TRUE) - what_is_expected_value_of_pmf(list_of_probabilities = list_of_probabilities, squared_result = TRUE)
#}
# @end what_is_variance_of_pmf

test_what_is_standard_deviation_of <- function() {
  all.equal(what_is_standard_deviation_of(a=2, b=4), 0.5773503)
}
test_what_is_standard_deviation_of()