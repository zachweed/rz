library(methods)

# @begin TwoSampleHypothesisTest
two_sample_hypothesis_test <- setRefClass("TwoSampleHypothesisTest",
                               fields=list(
                                 x_bar_one = "numeric", x_bar_two = "numeric", mu_one = "numeric", mu_two = "numeric", 
                                 sd_one = "numeric", sd_two = "numeric", n_one = "numeric", n_two = "numeric"
                               ),
                               methods=list(
                                 what_is_test_statistic = function(a, b) {
                                   differences <- a - b
                                   mean_differences <- sum(differences) / length(differences)
                                   sum(differences) / length(differences) / (sd(differences) / sqrt(length(differences)))
                                 },
                                 run_test = function(alpha, p_value) {
                                   if(alpha > p_value) { print("reject") }
                                   if(alpha < p_value) { print("fail to reject") }
                                 },
                                 # i.e. z-score.
                                 find_test_statistic_no_difference = function() {
                                   ((x_bar_one - x_bar_two)) / sqrt((((sd_one)^2)/n_one)+(((sd_two)^2)/n_two))
                                 },
                                 find_p_value_no_difference_ie_two_tailed = function(alpha=0) {
                                   p_value <- 2 * pnorm(find_test_statistic_no_difference(), lower.tail=FALSE)
                                   if(alpha > 0){
                                     if(alpha > p_value){
                                       print(p_value)
                                       print("reject null hypothesis")
                                     } else {
                                       print("do not reject null hypothesis")
                                     }
                                   } else {
                                     print(p_value)
                                     print('without hypothesis testing')
                                   }
                                 },
                                 # defaults to 5% significance level.
                                 find_p_value_greater_than = function() {
                                   1 - pnorm((x_bar_one - x_bar_two), 0, sqrt((sd_one^2/n_one + sd_two^2/n_two)))
                                 },
                                 find_left_tailed_p_value = function(test_statistic) {
                                   pnorm(test_statistic, lower.tail=TRUE)
                                 },
                                 find_right_tailed_p_value = function(test_statistic) {
                                   pnorm(test_statistic, lower.tail=FALSE)
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
                                 welchs_t_test_with_lists = function(list_one, list_two, alternative) {
                                   if(sum(list_one) > sum(list_two)) {
                                     dominant <- list_one
                                     weaker <- list_two
                                   } else {
                                     dominant <- list_two
                                     weaker <- list_one
                                   }
                                   t.test(dominant - list_two, alternative=alternative)
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

test_test_statistic <- function() {
  t <- two_sample_hypothesis_test()
  all.equal(t$what_is_test_statistic(c(354, 540, 548, 363, 448, 422, 498, 525), c(340, 518, 536, 327, 425, 410, 473, 498)), 7.212283)
  all.equal(t$what_is_test_statistic(c(354, 540, 548, 363, 448, 422, 498, 525), c(343, 518, 512, 354, 429, 401, 471, 507)), 6.713042)
}
test_test_statistic()

