library(methods)

# @begin Percentile
null_hypothesis_test <- setRefClass("NullHypothesis",
  fields=list(
    mu="numeric",
    x_bar="numeric",
    h_o="numeric",
    h_a="numeric",
    d_f="numeric",
    sl="numeric",
    s="numeric",
    sd="numeric",
    critical_value="numeric",
    test_statistic="numeric",
    tails="numeric"
 ),
 methods=list(
  init_test_statistic = function() {
    test_statistic <<- (x_bar - mu)/(sample_size/sqrt())
    print(test_statistic)
  },
  init_critical_value = function() {
    critical_value <<- qt(sl/tails, d_f, lower.tail=FALSE)
    print(critical_value)
  },
  init_df = function() {
    d_f <<- (sample_size - 1)
    print(d_f)
  },
  # Given means from two different observations,
  # determine how rare event is by comparing to alpha.
  # @return Whether or not to reject the null-hypothesis.
  # @end test_p_value
  test_p_value = function() {
  p_value <- pnorm( (h_a - h_o) / (sd / sqrt(s) ) )
  reject <- sl > p_value

    if(reject == TRUE){
      print("Reject Null Hypothesis")
    } else {
      print("Fail to Reject Null Hypothesis")
    }
  },
  z_stat = function() { ((sl - h_a) / sqrt(h_a * (1-h_a) / 100)) },
  p_value_for = function(z = 0) { 1 - pnorm(z) },
  p_value = function() { 2*(1-pnorm(abs(z_stat()))) },
  find_test_statistic = function() { (x_bar - mu)/(sd/sqrt(s)) }
 )
)
