library(methods)

# @begin ChiSquared
# Handles Tests of Independence
ChiSquared <- setRefClass("ChiSquared",
  fields=list(
    # Dependent -> ((row * coluxn)/total)
    # (contingency_table = matrix(c(1,2,3,4,5,6)), ncol=3) == [[1,2,3], [4,5,6]]
    contingency_table = "matrix"
  ),
  methods=list(
    add_columns = function(left_columns, top_columns) {
      row.names(contingency_table) <<- left_columns
      colnames(contingency_table) <<- top_columns
    },
    call_test = function() {
      chisq.test(contingency_table)
      print("is level of significance greater than p-value?")
    },
    # Given a function like: P(X > 24)
    # @param val Int value of X (random variable)
    # @param df Int value of degrees of freedom
    # @param less_than Boolean whether to return lower, upper tail.
    # tested: √√
    probability_of_X = function(val, df, less_than) {
      pchisq(q=val, df=df, lower.tail=less_than)
    }
    # expected need -> E = ((row_total) * (column total))/total surveyed
  )
)

test_probability = function() {
 pchisq(21, df=20, lower.tail=FALSE) 
}
all.equal(test_probability(), 0.3971326)
