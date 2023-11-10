library(methods)

# @begin Percentile
null_hypothesis_test <- setRefClass("NullHypothesis",
  fields=list(
    rate="numeric", alpha="numeric", gt="numeric", sub_sample_size="numeric", p0="numeric", q0="numeric", mu="numeric", x_bar="numeric", h_o="numeric", h_a="numeric",
    d_f="numeric", significance_level="numeric", s="numeric", sd="numeric", critical_value="numeric", test_statistic="numeric", tail_number="numeric", sample_size="numeric"
 ),
 methods=list(
  # If given test statistic Z, find p-value.
  p_value_for_test_statistic_z = function(z) {
    1 - pnorm(z)
  },
  break_down_data = function() {
    diff <- -(h_a - h_o)
    test_statistic <- (h_a - h_o)/( sqrt( ((h_a * (1 - h_a))/sample_size) ) )
    left_of_test_statistic <- (test_statistic * diff)
    right_of_test_statistic <- ((1 - test_statistic) * diff)
    list(test_statistic, left_of_test_statistic, right_of_test_statistic)
  },
  t_test = function(sample, alternative="greater") {
    print(t.test(sample, mu=mu, alternative=alternative))
    print("Is p-value greater than alpha?")
    print(alpha)
  },
  init_cv = function(confidence_level=0.95) {
    if(confidence_level != 0) {
      qnorm((1+confidence_level)/2, 0, 1)
    }
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
  reject <- alpha > p_value

    if(reject == TRUE){
      print("Reject Null Hypothesis")
    } else {
      print("Fail to Reject Null Hypothesis")
    }
  },
  z_stat = function() { 
    diff_rate <- rate / 100
    actual_ratio <- sub_sample_size / sample_size
    print("p-value:\n")
    print((1 - pnorm(((actual_ratio) - diff_rate) / sqrt((diff_rate * (1 - diff_rate)) / sample_size))))
    print("\n")
    print("alpha:\n")
    print(alpha)
    print("\n")
  },
  p_value_for = function(z = 0) { 1 - pnorm(z) },
  p_value = function() { 2*(1-pnorm(abs(z_stat()))) },
  # can be negative.
  find_test_statistic = function() { (x_bar - mu)/(sd/sqrt(s)) }
  # distance ( measured mean <-> null mean); in standard errors.
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