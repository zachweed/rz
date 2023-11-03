######################################
# 0. CLT - Central Limit Theorem.    #
# 1. Finding SE from μ.              #
# 3. Probability of summed distro.   #
######################################

# Technically Standard Deviation Over Square root of N.
# @param mean
# @param sd
# @param n
# @begin standard_error_of_mean
standard_error_of_mean <- function(mean=0, sd=0, n=0) {
  sd/sqrt(n)
}
# @end standard_error_of_mean


# Examples:
#   1. Less Than: clt_for_sums(lower_bound=2, upper_bound=10, n=40, x=256, less_than=TRUE, greater_than = TRUE, gtv=230, ltv=270)
#   2. Greater Than: clt_for_sums(lower_bound=2, upper_bound=10, n=40, x=256, greater_than = TRUE, gtv=220)
#   3. Between Values: clt_for_sums(lower_bound=2, upper_bound=10, n=40, x=256, less_than=TRUE, greater_than = TRUE, gtv=220, ltv=245)
# Desmos can be helpful for this by defining:
#   a, b, a sample size, a mean, a standard deviation, and then using a density function.
# @param upper_bound 
# @param lower_bound 
# @param n
# @param x
# @param greater_than -> flag for only setting minimum.
# @param less_than -> flag for only setting maximum.
# @begin clt_for_sums
clt_for_sums <- function(upper_bound=0, lower_bound=0, n=0, x=0, greater_than=FALSE, less_than=FALSE, gtv=0, ltv=0) {
  vl = 0
  mu = (n * ((upper_bound + lower_bound)/2))
  sv = (sqrt(n) * sqrt(((upper_bound - lower_bound)^2)/12))
  if(less_than == TRUE){ vl = pnorm(x, mu, sv) }
  if(less_than == TRUE && greater_than == TRUE) {
    vl = pnorm(ltv, mu, sv) - pnorm(gtv, mu, sv)
  }
  vl
}
# @end clt_for_sums