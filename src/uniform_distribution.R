library(methods)
 

# Equally Likely Outcomes
EX = function(a=0, b=0){((a+b)/2)}
VX = function(a=0, b=0){((b-a)^2)/12}

EX(a=3.5, b=6)

# Handles:
#   1. Between A and B
#   2. Less than C
p_between = function(a=0, b=0, c=3, d=0){
  if(c > 0 && d > 0) {
    c = d-c
  }
  c/b
}

standard_deviation_random_variable = function(a=0, b=0){
  sqrt(
    ((b-a)^2)/12
  )
}
standard_deviation_random_variable(a=1, b=9)

 
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