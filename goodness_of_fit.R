library(methods)

# @begin GoodnessOfFit
goodness_of_fit <- setRefClass("GoodnessOfFit",
  fields=list(
   # Dependent -> ((row * coluxn)/total)
   # (contingency_table = matrix(c(1,2,3,4,5,6)), ncol=3) == [[1,2,3], [4,5,6]]
   contingency_table = "matrix"
  ),
  methods=list(
    add_columns = function(columns) {
      names(contingency_table) <<- columns
    },
    call_test = function(p) {
      chisq.test(contingency_table, p = rep((1/length(contingency_table)), length(contingency_table)))
    },
    observed = function() { contingency_table$observed },
    expected = function() { contingency_table$expected },
   )
)

test_are_coin_flips_fair = function() {
  g <- goodness_of_fit(contingency_table=matrix(c(20, 57, 23), ncol=3))
  g$add_columns(c("0", "1", "2"))
}
