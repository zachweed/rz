library(methods)

# @begin TwoSampleHypothesisTest
two_sample_hypothesis_test <- setRefClass("TwoSampleHypothesisTest",
                               fields=list(
                                 mu_one = "numeric", mu_two = "numeric", sd_one = "numeric", sd_two = "numeric",
                                 n_one = "numeric", n_two = "numeric"
                               ),
                               methods=list(
                                 find_test_statistic = function() {
                                   (mu_one - mu_two) / sqrt((((sd_one)^2)/n_one)+(((sd_two)^2)/n_two))
                                 },
                                 should_reject_null_hypothesis = function(alpha=0, test_statistic = 0) {
                                   qnorm(alpha/2, lower.tail=FALSE) < test_statistic
                                 }
                               )
)
