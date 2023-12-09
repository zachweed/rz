#################################################
# General Uniform Distribution Helper Functions #
#################################################

library(methods)
 
# @begin E[X] 
EX = function(a=0, b=0){((a+b)/2)}
# @end E[X]

# @begin V[X] 
VX = function(a=0, b=0){((b-a)^2)/12}
# @end V[X]

# @begin SD[X] 
SX = function(a=0, b=0){
  (b-a)/sqrt(12)
}
# @end S[X] 

# @param a: lower
# @param b: upper
# @begin standard_deviation_from_random_variable
standard_deviation_from_random_variable = function(a=0, b=0){
  sqrt(
    ((b-a)^2)/12
  )
}
# @begin standard_deviation_from_random_variable
 
#########################
#       Tests           #
#########################
# Test E[X].
all.equal(EX(a=2, b=2), 2)

# Test Var[X]
all.equal(VX(a=4, b=2), 0.33333333)

# Test SD[X]
all.equal(SD(a=6, b=2), 1.154701)

# Test p_between
all.equal(p_between(a=0, b=15, c=3), (3/15))
all.equal(p_between(a=0, b=15, c=9, d=10), (1/15))