from_normal_distributions <- function(mean=0, standard_deviation=0, standard_error=0) {
  (standard_deviation/standard_error)^2
}
from_normal_distributions(mean=5,standard_deviation=20, standard_error=0.5)