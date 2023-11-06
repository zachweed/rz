# Given means from two different observations,
# determine how rare event is by comparing to alpha.
# @param original_mean
# @param different_mean
# @param sd
# @param n
# @param a
# @return Whether or not to reject the null-hypothesis.
test_p_value <- function(original_mean = 0, different_mean = 0, sd = 0, n = 0, a = 0) {
  h_o <- original_mean
  h_a <- different_mean
  p_value <- pnorm( (different_mean - original_mean) / (sd / sqrt(n) ) )
  reject <- a > p_value

  if(reject == TRUE){
    print("Reject Null Hypothesis")
  } else {
    print("Fail to Reject Null Hypothesis")
  }
}
# @end test_p_value

# @begin p_value_for_z
p_value_for_z <- function(z = 0) { 1 - pnorm(z) }
# @end p_value_for_z

# @param p: Success Rate (proportion)
# @param assertion: Tested Value (what hypothesis tests)
# @param sample_size: Tested Value (what hypothesis tests)
# @begin z_stat
z_stat <- function(p = 0, assertion = 0, sample_size = 0) {
  ((p - assertion) / sqrt(assertion * (1-assertion) / 100))
}
# @end z_stat

# @param p: Success Rate (proportion)
# @param assertion: Tested Value (what hypothesis tests / default to p if not given)
# @param sample_size: Tested Value (what hypothesis tests)
# @begin p_value
p_value <- function(p = 0, assertion = 0, sample_size = 0) {
  2*(1-pnorm(abs(z_stat(p=p, assertion=assertion, sample_size=sample_size))))
}
# @end p_value
p_value(p=0.3, assertion=0.3*150, sample_size=43)

# Finding test statistic for a mean that's possible different from an asserted mean.
find_test_statistic <- function(mu = 0, x_bar=0, standard_deviation = 0, sample_size = 0) {
  (x_bar - mu)/(standard_deviation/sqrt(sample_size))
}