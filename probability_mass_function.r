# Work related to probability mass functions

# @param list_of_probabilities List[Vector[Int, Int]]
# @return Positive or Negative Int that's E[X].
# @begin ev_pmf
ev_pmf <- function(list_of_probabilities) {
  sum = 0
  for(p in list_of_probabilities) {
    sum = sum + (p[1] * p[2] )
  } 
  sum
}
what_is_expected_value_of_pmf(
  list(
    c(
      0.5, 30
    ),
    c(
      0.5, -50
    )
  )
)
# @end ev_pmf

# @param list_of_probabilities List[Vector[Int, Int]]
# @return Positive Int; doesn't have to be between 0 and 1.
# @begin what_is_sdev_of_probability_distribution
sdev_pdist <- function(list_of_probabilities) {
  e_x_2 = 0
  e_x = 0
  for(p in list_of_probabilities) {
    e_x = e_x + (p[1] * p[2])
    e_x_2 = e_x_2 + (p[1] * (p[2] ^ 2))
  }
  e_x2 = e_x ^ 2
  sqrt(e_x_2 - e_x2)
}
sdev_pdist(
  list(
    c(
      0.25, 1
    ),
    c(
      0.25, 2
    ),
    c(
      0.2, 3
    ),
    c(
      0.3, 4
    )
  )
)
# @end sdev_pdist


###################################
#           Tests                 #
###################################

# Describe: Expected Values of an abstract PMF
test_ev_pmf <- function() {
  ev_pmf(
    list(
      c(
        -1, 0.2
      ),
      c(
        1, 0.2
      ),
      c(
        3, 0.2
      ),
      c(5, 0.4)
    )
  ) == 2.6
}
test_ev_pmf()

test_sdev_pdist <- function() {
  all.equal(sdev_pdist(
    list(
      c(
        0.3, -10000
      ),
      c(
        0.5, 10000
      ),
      c(
        0.2, 100000
      )
    )
  ), 39949.97)
}
test_sdev_pdist
