# @begin t_score
t_score <- function(x_bar=3, mu=3, standard_deviations=3, sample_size=3) {
  (x_bar - mu)/(standard_deviations/sqrt(sample_size))
}
# @end t_score

# @begin degrees_of_freedom
df <- function(standard_deviations) {
  standard_deviations - 1
}
degrees_of_freedom <- function(standard_deviations) {
  df(standard_deviations)
}
# @end degrees_of_freedom

#########################
#       Tests           #
#########################

all.equal(
  degrees_of_freedom(
    5
  ), 4
)

all.equal(
  round(t_score(
    x_bar = 4, mu=2, standard_deviations=4, sample_size=2
  ), 2), 1.41
)
