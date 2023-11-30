library(methods)

# @begin Anova
Anova <- setRefClass("Anova",
  fields = list(
    mt="matrix",
  ),
  methods = list(
    sum_of_squares = function(col_nums) {
      sums_a <- 0
      sums_b <- 0
      for(i in 1:col_nums){
        sums_a = sums_a + (sum(mt[,i])^2/length(mt[,i]))
      }
      for(i in 1:col_nums){
        sums_b = sums_b + mt[,i]
      }
      sums_a
    }
  )
)
