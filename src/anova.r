library(methods)

# @begin Anova
# one-way ANOVA is always right-tailed because larger F-values are in right tail and cause rejection of H_0
Anova <- setRefClass("Anova",
  fields = list(
  ),
  methods = list(
    # p-value is P(>X)
    linear_regression = function(independent_variable, dependent_variable) {
      independent_variable <- factor(independent_variable)
      data <- data.frame(independent_variable, dependent_variable)
      linear_regression_model <- lm(dependent_variable ~ independent_variable, data = data)
      anova(linear_regression_model)
    }
  )
)

test_anova = function() {
  # independent variable
  plan <- c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3)
  independent_variable <- plan
  # dependent variable
  weight_loss <- c(7,8,4.5,8.9,7.7,5.2,6,6,7,5.5,4,5,4,6,8)
  dependent_variable <- weight_loss
  diet_data <- data.frame(independent_variable, dependent_variable)
  linear_regression_model <- lm(dependent_variable ~ independent_variable, data = diet_data)
  # p-value should be 0.1571
  anova(linear_regression_model)
}
test_anova()
