library(methods)

# @begin Correlation
Correlation <- setRefClass("Correlation",
 fields = list(
   contingency_table = "matrix"
 ),
 methods = list(
   # two_tailed
   test = function(sample_r, sample_size) {
    2 * ( 1 - pnorm((sample_r * sqrt(sample_size - 2))/(sqrt(1-sample_r^2))) )
   }
 )
)

test_correlation = function() {
  data <- matrix(c(19,71,58,17,95,95,13,84,58, 424,647,-32,1470,409,277,914), ncol=2, nrow=8 )
  cor(data[,1], data[,2])
}
all.equal(test_correlation(), 0.7556489)

test_correlation = function() {
  data <- matrix(c(17,81,39,33,61,61,13,30,186,423,241,726,-301,942,-304,-413), ncol=2, nrow=8 )
  cor(data[,1], data[,2])
}
test_correlation()
all.equal(test_correlation(), 0.3690441)

test_correlation = function() {
  sample_r = 0.46
  sample_size = 29
  c <- Correlation()
  c$test(sample_r, sample_size)
}
all.equal(test_correlation(), 2.691946)

test_correlation = function() {
  sample_r = 0.21
  sample_size = 30
  c <- Correlation()
  c$test(sample_r, sample_size)
}
all.equal(test_correlation(), 1.136559)

test_correlation = function() {
  sample_r = 0.37
  sample_size = 32
  c <- Correlation()
  c$test(sample_r, sample_size)
}
all.equal(test_correlation(), 2.181382)