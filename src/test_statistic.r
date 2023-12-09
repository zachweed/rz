# @param h0 Numeric -> expected mean from the observer.
# @param ha Numeric -> both observed & calculated mean.
# Adjust sd if provided
# Compute test statistic given:
#   (1) Assumed mean
#   (2) Our Hypothesis
#   (3) Random sample size
#   (4) Standard deviation
cum_sample_hypothesized_df_sum_p <- function(h0, ha, sd, n) {
  sd <- 1
  (h0 - ha)/(sqrt(1)^2/n)
}

# @param sd_one, sd_two -> sample standard deviations (estimates of unknowns)
# @param x_one, x_two -> sample means
# @param mu_one, mu_two -> population means
from_welchs_t_test <- function(x_one, x_two, mu_one, mu_two, sd_one, sd_two, n_one, n_two) {
  (
    ( 
      (x_one - x_two) - (mu_one - mu_two)) /
      sqrt(
        ((sd_one)^2 / n_one) + ((sd_two)^2/n_two)
      )
  )
}

test_cum_sample_hypothesized_df_sum_p <- function() {
  all.equal(cum_sample_hypothesized_df_sum_p(48.6, 46, 4.2, 30), 0.004)
}
test_cum_sample_hypothesized_df_sum_p()