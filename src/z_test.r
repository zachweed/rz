# requires: BSDA.

# Two-Sample z-test
# @param null_hypothesis_values Vector[Numeric] the null-hypothesis.
# @param null_standard_deviation Numeric sigma of the null-hypothesis.
# @param alternative_hypothesis_values Vector[Numeric] alternative-hypothesis
# @param alternative_standard_deviation Numeric sigma of the alternative-hypothesis.
# @param mu Numeric a binary assumed mean, i.e. hypothesis is a difference of mu between h0 and ha.
#        if mu is not supplied then this is a test of differences rather than testing a specific difference.         
# @return confidence interval for true difference
z_test <- function(null_hypothesis_values, null_standard_deviation, alternative_hypothesis_values, alternative_standard_deviation, mu = NULL, confidence_level = NULL) {
  z.test(x = null_hypothesis_values, sigma.x = null_standard_deviation, y = alternative_hypothesis_values, sigma.y = alternative_standard_deviation, mu = mu, conf.level = confidence_level)
}
z_test(null_hypothesis_values=c(1), null_standard_deviation=0, alternative_hypothesis_values=c(1), alternative_standard_deviation=c(1), mu=212, confidence_level=0.95)