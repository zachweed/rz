library(methods)

# @begin FDistribution
FDistribution <- setRefClass("FDistribution",
  fields = list(
  ),
  methods = list(
    probability = function(x, numerator, denominator, alternative) {
      if(all.equal(alternative, ">")){
        1 - pf(x, numerator, denominator, lower.tail=TRUE)  
      }
      if(all.equal(alternative, "<")){
        1 - pf(x, numerator, denominator, lower.tail=FALSE)
      }
      if(all.equal(alternative, "two.sided")){
        1 - pf(x, numerator, denominator)  
      }
    },
    same_size_group = function(n, variance_of_sample_means, pooled_variance) {
      (n * variance_of_sample_means)/pooled_variance
    }
  )
)

test_f_distribution = function() {
  fd <- FDistribution()
  fd$probability(4.7, 10, 8)
}
all.equal(test_f_distribution(), 0.01918741)
