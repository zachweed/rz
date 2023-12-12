# find p-value from test-statistic
# Hypothesis has utility methods for test statistics.
p_value_from_test_statistic <- function(t_score = 0, z_score = 0, sample_size = 0, tail = 0) {
  if(t_score <= 0 ){
    if(z_score == 0){
      print('find one.')
    }
  }
  if(tail == "lower") {
    pt(z_score, sample_size)  
  } else {
    if(z_score > 0) {
      pt(z_score, sample_size - 1, lower.tail = FALSE)
    } else {
      1 - pnorm(t_score)
    }
  }
}

test_p_value_from_test_statistic <- function() {
  all.equal(p_value_from_test_statistic(t_score = 2.6352), 0.004204386)
}
test_p_value_from_test_statistic()

test_p_value_from_test_statistic_o <- function() {
  all.equal(p_value_from_test_statistic(t_score = 0, z_score = 1.936, 15), 0.03666173)
}
test_p_value_from_test_statistic_o()