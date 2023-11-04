# Given means from two different observations,
#	determine how rare event is by comparing to alpha.
# @param original_mean
# @param different_mean
# @param sd
# @param n
# @param a
# @return Whether or not to reject the null-hypothesis.
p_value <- function(original_mean = 0, different_mean = 0, sd = 0, n = 0, a = 0) {
	h_o <- original_mean
	h_a <- different_mean
	p_value <- pnorm( (different_mean - original_mean) / (sd / sqrt(n) ) )
	reject <- a > p_value

	if(reject == TRUE){
		print("Reject Null Hypothesis")
	} else {
		print("Fail to Reject Null Hypothesis")
	}
}