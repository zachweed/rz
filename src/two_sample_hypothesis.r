library(methods)

# @begin TwoSampleHypothesisTest
two_sample_hypothesis_test <- setRefClass("TwoSampleHypothesisTest",
                               fields=list(
                                 x_bar_one = "numeric", x_bar_two = "numeric", mu_one = "numeric", mu_two = "numeric", 
                                 sd_one = "numeric", sd_two = "numeric", n_one = "numeric", n_two = "numeric"
                               ),
                               methods=list(
                                 run_test = function(alpha, p_value) {
                                   if(alpha > p_value) { print("reject") }
                                   if(alpha < p_value) { print("fail to reject") }
                                 }
                                 # i.e. z-score.
                                 find_test_statistic_no_difference = function() {
                                   ((x_bar_one - x_bar_two)) / sqrt((((sd_one)^2)/n_one)+(((sd_two)^2)/n_two))
                                 },
                                 find_p_value_no_difference = function() {
                                   2 * pt(find_test_statistic_no_difference(), degrees_of_freedom())
                                 },
                                 # defaults to 5% significance level.
                                 find_p_value_greater_than = function() {
                                   1 - pnorm((x_bar_one - x_bar_two), 0, sqrt((sd_one^2/n_one + sd_two^2/n_two)))
                                 },
                                 # i.e. z-score.
                                 find_test_statistic_with_x_bar = function() {
                                   ((x_bar_one - x_bar_two) - (mu_one - mu_two)) / sqrt((((sd_one)^2)/n_one)+(((sd_two)^2)/n_two))
                                 },
                                 # i.e. z-score.
                                 find_test_statistic_without_x_bar = function() {
                                   (mu_one - mu_two) / sqrt((((sd_one)^2)/n_one)+(((sd_two)^2)/n_two))
                                 },
                                 find_test_statistic_without_mu = function() {
                                   (x_bar_one - x_bar_two) / sqrt((((sd_one)^2)/n_one)+(((sd_two)^2)/n_two))
                                 },
                                 should_reject_null_hypothesis = function(alpha=0, test_statistic = 0) {
                                   qnorm(alpha/2, lower.tail=FALSE) < test_statistic
                                 },
                                 welchs_t_test_with_lists = function(list_one, list_two) {
                                   t.test(list_one, list_two)
                                 },
                                 standard_error = function() {
                                   sqrt((sd_one^2/n_one)+(sd_two^2/n_two))
                                 },
                                 welchs_t_test_without_lists = function() {
                                   (x_bar_one - x_bar_two)/standard_error()
                                 },
                                 # Standard Deviation of two sample Standard Deviations
                                 population_standard_deviation_from_sample_standard_deviations = function() {
                                   sqrt( ((sd_one)^2/n_one) + ((sd_two)^2/n_two) )
                                 },
                                 degrees_of_freedom = function() {
                                   (( (((sd_one ^ 2) / n_one) + (( sd_two ^ 2 )/n_two ))^2 / ( ((((sd_one ^ 2) / n_one)^2) / (n_one - 1)) + (((sd_two ^ 2) / n_two)^2)/(n_two - 1) )))
                                 }
                               )
)

test_test_statistic_to_p_value <- function() {
  all.equal((2*1-pnorm(1.1148)), 0.2649361)
}
test_test_statistic_to_p_value()