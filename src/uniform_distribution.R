library(methods)
 
# Equally Likely Outcomes

EX = function(a=0, b=0){((a+b)/2)}
VX = function(a=0, b=0){((b-a)^2)/12}
p_between = function(a=0, b=0, c=3){
  c/b
}
SD = function(a=0, b=0){
  sqrt(
    ((b-a)^2)/12
  )
}

 
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