library(methods)

# 1. if >= 95% of spark plugs pass then send batch.
# I. Given a sample of 250, 242 passed
# II. H_o -> (p < 0.95)
# III. H_a -> (p > 0.95)

# so, 242/250 passed, so what's teh probability of 95% passing at a alpha of 0.05.

# critical_value: point on test distribution to compare test statistic; correspond to a.
# 

# 1. results of a hypothesis test will create a normal distribution.
# 2. a reject region exists within the normal distribution.
# 3. a test statistic could fall within the rejection region.
# 4. so, with a=0.05:
#   i. draw a vertical line through 5% of distribution.
#  ii. draw a vertical line through point of critical value.
# iii. if critical value is within rejection region then reject.
#
# two-tailed-test:
#   reject null-hypothesis when critical_value < -absolute- value of test statistic.
# right-tailed-test:
#   reject null-hypothesis when critical value < value of test statistic.
# left-tail-test:
#   reject null-hypothesis when critical value > value of test statistic.

# @begin Percentile
null_hypothesis_test <- setRefClass("NullHypothesis",
  fields=list(
    rate="numeric", alpha="numeric", gt="numeric", sub_sample_size="numeric", p0="numeric", q0="numeric", mu="numeric", x_bar="numeric", h_o="numeric", h_a="numeric",
    d_f="numeric", significance_level="numeric", s="numeric", sd="numeric", critical_value="numeric", test_statistic="numeric", tail_number="numeric", sample_size="numeric",
    xbar="numeric"
 ),
 methods=list(
  # one sample t-test looks for signification deviation from compared mu.
  one_sample_t_test = function(sample, alternative="greater") {
    print(t.test(sample, mu=mu, alternative=alternative))
    print("Is p-value greater than alpha?")
    print(alpha)
  },
  # two sample t-test looks for signification deviation between means
  two_sample_t_test = function(sample_one, sample_two) {
    print(t.test(sample_one, sample_two))
  },
  init_cv = function(confidence_level=0.95) {
    if(confidence_level != 0) {
      qnorm((1+confidence_level)/2, 0, 1)
    }
  },
  calculate_p_value_for_normal_distribution_for_mean = function(tested_mean, expected_mean, standard_deviation=1, alpha=0.05, marker, sample_size) {
    if(all.equal(marker, "<")){
      print("area to the left is:")
      pnorm( (tested_mean - expected_mean) / (standard_deviation / sqrt(sample_size)))
      print("so that's the probability of less than or equal to the tested mean")
    }
  },
  calculate_p_value_for_proportion_hypothesis_test = function(sample_proportion, null_hypothesis_proportion, n, marker) {
    ab <- (sample_proportion - null_hypothesis_proportion) / sqrt(null_hypothesis_proportion * ((1 - null_hypothesis_proportion)/n))
    if(marker == ">"){
      print("P-VALUE IS:")
      1 - pnorm(ab)
    }
    if(marker == "<"){
      print("P-VALUE IS:")
      pnorm(ab)
    }
    if(marker == "different"){
      print("if null hypothesis is true, this is percentage chance of sample proportion differing")
      print("P-VALUE IS:")
      2 * (1- pnorm(abs(ab)))
    }
  }
 )
)

test_cv <- function() {
  n <- null_hypothesis_test()
  all.equal(n$init_cv(), 2.053749)
}
test_cv()

test_ts <- function() {
  n <- null_hypothesis_test(s=10, mu=20, sd=3, alpha=0.01, x_bar=24.5)
  all.equal(n$find_test_statistic(), 4.743416)
}
test_ts()