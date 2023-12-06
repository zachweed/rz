library(methods)

# @begin FDistribution
# assymetrical, skewed to right.
# each df has a different curve
# as df(numerator|denominator) increase, curve approximates normal
# always >= 0
FDistribution <- setRefClass("FDistribution",
  fields = list(
  ),
  methods = list(
    probability = function(x, numerator, denominator, alternative) {
      if(alternative == ">"){
        1 - pf(x, numerator, denominator, lower.tail=TRUE)  
      }
      if(alternative == "<"){
        1 - pf(x, numerator, denominator, lower.tail=FALSE)
      }
      if(alternative == "two.sided"){
        1 - pf(x, numerator, denominator)  
      }
    },
    same_size_group = function(n, variance_of_sample_means, pooled_variance) {
      (n * variance_of_sample_means)/pooled_variance
    },
    # If simple equality determination, divide squares of variances.
    # left P(X<N)
    # right P(X>N)
    # two-tailed P(X != N)
    ratio_of_variances_not_eq = function(variance_one, sample_size_one, variance_two, sample_size_two) {
      pf(variance_one/variance_two, sample_size_one - 1, sample_size_two - 1)
    },
    ratio_of_variances_gteq = function(variance_one, sample_size_one, variance_two, sample_size_two) {
      1 - pf(variance_one/variance_two, sample_size_one - 1, sample_size_two - 1)
    }
  )
)

test_f_distribution = function() {
  fd <- FDistribution()
  fd$probability(4.7, 10, 8, "<")
}
all.equal(test_f_distribution(), 0.01918741)

test_variances_not_eq = function() {
  fd <- FDistribution()
  fd$ratio_of_variances_gteq(21.2, 21, 19.3, 24)
}
all.equal(test_variances_not_eq(), 0.411)
