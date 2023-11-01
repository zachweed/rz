# Technically Standard Deviation Over Square root of N.
standard_error_of_mean <- function(mean=0, sd=0, n=0) {
  sd/sqrt(n)
}
standard_error_of_mean(sd=20, n=45)

p_between = function(a=0, b=0, c=3, d=0){
  if(c > 0 && d > 0) {
    c = d-c
  }
  c/b
}

# central limit theorem sums, mean, and standard deviation are: 
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
# Less than
#clt_for_sums(lower_bound=2, upper_bound=10, n=40, x=225, less_than=TRUE)
#clt_for_sums(lower_bound=2, upper_bound=10, n=40, x=235, less_than=TRUE)
clt_for_sums(lower_bound=2, upper_bound=10, n=40, x=256, less_than=TRUE, greater_than = TRUE, gtv=230, ltv=270)

# Between
# clt_for_sums(lower_bound=2, upper_bound=10, n=40, x=256, less_than=TRUE, greater_than = TRUE, gtv=210, ltv=250)
# clt_for_sums(lower_bound=2, upper_bound=10, n=40, x=256, less_than=TRUE, greater_than = TRUE, gtv=220, ltv=245)
# clt_for_sums(lower_bound=2, upper_bound=10, n=40, x=256, less_than=TRUE, greater_than = TRUE, gtv=250, ltv=260)
clt_for_sums(lower_bound=2, upper_bound=10, n=40, x=256, less_than=TRUE, greater_than = TRUE, gtv=220, ltv=245)

clt_for_sums(lower_bound=2, upper_bound=10, n=40, x=225, less_than=TRUE)