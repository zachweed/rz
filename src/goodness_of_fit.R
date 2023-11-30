library(methods)

# @begin GoodnessOfFit
GoodnessOfFit <- setRefClass("GoodnessOfFit",
  fields=list(
   # Dependent -> ((row * coluxn)/total)
   # (contingency_table = matrix(c(1,2,3,4,5,6)), ncol=3) == [[1,2,3], [4,5,6]]
   contingency_table = "matrix"
  ),
  methods=list(
    add_columns = function(columns) {
      names(contingency_table) <<- columns
    },
    # i.e. Goodness of Fit Test; where x^2 is chi-squared goodness of fit.
    # e.g. [[Brown, Yellow, Green, Red, Orange, Blue]]
    #       [5    , 12    , 9    , 10 , 5.    , 9.  ]]
    #      gof <- goodness_of_fit(contingency_table=matrix(c(5,12,9,10,5,9)))
    #      gof$add_columns(c("Brown", "Yellow", "Green", "Red", "Orange", "Blue"))
    #      gof$call_test()
    #       data:  gof$contingency_table
    #       X-squared = 4.72, df = 5, p-value = 0.451
    # e.g. [[   A,   C,    G,  T  ]]
    #   E->[    5,  12,    9,  10  ]
    #   O->[  0.2, 0.3,  0.3,  0.2 ]
    #      gof <- goodness_of_fit(contingency_table=matrix(c(5,12,9,10,5,9)))
    #      gof$call_test(c(0.2, 0.3, 0.3, 0.2))
    #       data:  gof$contingency_table
    #       X-squared = 4.72, df = 5, p-value = 0.451
    #     	Chi-squared test for given probabilities
    #         data:  contingency_table
    #         X-squared = 15.339, df = 3, p-value = 0.001549
    # tested: √√√
    # @param list (c) Matrix probabilities considered E in X^2=((O-E)^2)/E
    call_test = function(p) {
      chisq.test(contingency_table, p=p)
    },
    observed = function() { contingency_table$observed },
    expected = function() { contingency_table$expected }
   )
)

test_are_coin_flips_fair = function() {
  g <- goodness_of_fit(contingency_table=matrix(c(20, 57, 23), ncol=3))
  g$add_columns(c("0", "1", "2"))
}
