library(methods)
 
# @begin UniformDistribution
uniform_distribution <- setRefClass("UniformDistribution",
                                      fields=list(
                                        a="numeric",
                                        b="numeric",
                                        sample="list"
                                      ),
                                      methods=list(
                                        # E[X], expected value
                                        EX = function(){
                                          ((a+b)/2)
                                        },
                                        # Variance
                                        VX = function(){
                                          ((b-a)^2)/12
                                        },
                                        # SD[X]
                                        SD = function(){
                                          (
                                            sqrt(
                                              ((b-a)^2)/12
                                            )
                                          )
                                        }
                                      )
 )
# @end Uniform Distribution
 
#########################
#       Tests           #
#########################
# Test E[X].
udist <- uniform_distribution(
  a=2,
  b=2
)
all.equal(udist$EX(), 2)

# Test Var[X]
udist <- uniform_distribution(
  a=4,
  b=2
)
all.equal(udist$VX(), 0.33333333)

# Test SD[X]
udist <- uniform_distribution(
  a=6,
  b=2
)
udist$SD()
all.equal(udist$SD(), 1.154701)