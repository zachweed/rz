library(methods)

# @begin TwoSampleHypothesisTest
two_sample_hypothesis_test <- setRefClass("TwoSampleHypothesisTest",
                               fields=list(
                                 x_bar_one = "numeric", x_bar_two = "numeric", mu_one = "numeric", mu_two = "numeric", 
                                 sd_one = "numeric", sd_two = "numeric", n_one = "numeric", n_two = "numeric"
                               ),
                               methods=list(
                                 # i.e. z-score.
                                 find_test_statistic_no_difference = function() {
                                   ((x_bar_one - x_bar_two)) / sqrt((((sd_one)^2)/n_one)+(((sd_two)^2)/n_two))
                                 },
                                 find_p_value_no_difference = function() {
                                   2 * pt(find_test_statistic_no_difference(), degrees_of_freedom())
                                 },
                                 # i.e. z-score.
                                 find_test_statistic_with_x_bar = function() {
                                   ((x_bar_one - x_bar_two) - (mu_one - mu_two)) / sqrt((((sd_one)^2)/n_one)+(((sd_two)^2)/n_two))
                                 },
                                 # i.e. z-score.
                                 find_test_statistic_without_x_bar = function() {
                                   (mu_one - mu_two) / sqrt((((sd_one)^2)/n_one)+(((sd_two)^2)/n_two))
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
                                 degrees_of_freedom = function() {
                                   (( (((sd_one ^ 2) / n_one) + (( sd_two ^ 2 )/n_two ))^2 / ( ((((sd_one ^ 2) / n_one)^2) / (n_one - 1)) + (((sd_two ^ 2) / n_two)^2)/(n_two - 1) )))
                                 }
                               )
)
