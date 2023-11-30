library(methods)

# @begin Regression
Regression <- setRefClass("Regression",
  fields = list(
  ),
  methods = list(
   # where r(sy/sx)
   # i.e., 
   #  r -> correlation
   #  sy -> standard deviation of response
   #  sx -> standard deviation of predictor
   best_fit_line_summary = function(x, y, response_index, predictor_index) {
     summary(lm(y~x))$coefficients
   }
  )
)

test_best_fit_line = function() {
  x <- c(701, 696, 754,716,640, 595,698,669,735,790)
  y <- c(75, 67, 68, 68, 64, 76,90,97,79,91)
  r <- Regression()
  r$best_fit_line_summary(x,y)
}
test_best_fit_line()
