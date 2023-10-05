# What is the Expected value of a series of probabilities
# in a probability mass function?
#   Application:
#     1. Betting on something and finding E[X].
#     2. Abstract data and finding E[X].
# @param list_of_probabilities List[Vector[Int, Int]]
# @return Positive or Negative Int; doesn't have to be between 0 and 1.
# @begin what_is_expected_value_of_pmf
what_is_expected_value_of_pmf <- function(list_of_probabilities = list_of_probabilities, squared_result = FALSE, squared_y = FALSE) {
  sum = 0
  for(p in list_of_probabilities) {
    if(squared_y){
      sum = sum + (p[1] * (p[2] ^ 2))
    } else {
      sum = sum + (p[1] * (p[2]))
    }
  }
  if(squared_result) {
    sum = sum ^ 2  
  } else {
    sum = sum
  }
  sum
}
# @end what_is_expected_value_of_pmf
what_is_expected_value_of_pmf(
  list_of_probabilities = list(
    c(
      (0.4), 1
    ),
    c(
      (0.3), 10
    ),
    c(
      (0.3), 100
    )
  )
)

# @param list_of_probabilities List[Vector[Int, Int]]
# @return Positive Int; doesn't have to be between 0 and 1.
# @OKs: V
#   0. Financial Deals, Gambling, Abstract problems, etc.
# @begin what_is_y_of_probability_distribution
what_is_y_of_probability_distribution <- function(list_of_probabilities) {
  e_x_2 = 0
  e_x = 0
  for(p in list_of_probabilities) {
    e_x = e_x + (p[1] * p[2])
    e_x_2 = e_x_2 + (p[1] * (p[2] ^ 2))
  }
  e_x2 = e_x ^ 2
  e_x_2 - e_x2
}
# @end what_is_y_of_probability_distribution

# @param list_of_probabilities List[Vector[Int, Int]]
# @return Positive Int; doesn't have to be between 0 and 1.
# @OKs: V
#   0. Financial Deals, Gambling, Abstract problems, etc.
#     a. Investments
# @begin what_is_sdev_of_probability_distribution
what_is_sdev_of_probability_distribution <- function(list_of_probabilities) {
  sqrt(what_is_y_of_probability_distribution(list_of_probabilities))
}
# @end what_is_sdev_of_probability_distribution
what_is_sdev_of_probability_distribution(
  list(
    c(
      0.2, -10000
    ),
    c(
      0.6, 10000
    ),
    c(
      0.2, 100000
    )
  )
)

# @begin what_is_variance_of_probability_distribution
# @param List[Vectors] these are a Probabilty Mass Function Table.
# @begin what_is_variance_of_probability_distribution
what_is_variance_of_probability_distribution <- function(list_of_probabilities) {
  what_is_y_of_probability_distribution(list_of_probabilities)
}
# @end what_is_variance_of_probability_distribution
what_is_variance_of_probability_distribution(
  list(
    c(0.8, -10),
    c(0.1, 5),
    c(0.1, 10)
  )
)



###################################
#           Tests                 #
###################################

# Describe: Expected Values of an abstract PMF
test_expected_value <- function() {
  what_is_expected_value_of_pmf(
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
test_expected_value()

# Describe: Expected Values of a Deal
# Where there's:
#   1. a 30% probability of losing 10.
#   2. a 50% probability of gaining 10.
#   3. and a 20% probability of gaining 100. 
# Some variance is
test_what_is_sdev_of_probability_distribution <- function() {
  all.equal(what_is_sdev_of_probability_distribution(
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
test_what_is_sdev_of_probability_distribution

test_what_is_variance_of_probability_distribution <- function() {
  all.equal(what_is_variance_of_probability_distribution(list(
    c(0.8, 1),
    c(0.1, 10),
    c(0.1, 100)
  )), 871.56)
}
test_what_is_variance_of_probability_distribution()
