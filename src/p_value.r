# find p-value from test-statistic
p_value_from_test_statistic <- function(test_statistic=NULL) {
  if(test_statistic.is_null){
    print("find test statistic")
  } else {
    1 - pnorm(test_statistic)  
  }
}

test_p_value_from_test_statistic <- function() {
  all.equal(p_value_from_test_statistic(2.6352), 0.004)
}
test_p_value_from_test_statistic()