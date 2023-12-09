################################
# Probability Helper Functions #
################################

P <- function(Z="Z", operator="<", operand) {
  if(operator == "<"){
    pnorm(operand)
  }
  if(operator == ">="){
    qnorm(operand)
  }
}

P(Z="Z", "<", 1)
P(Z="Z", "<", 0.4)
P(Z="Z", ">=", 0.9)

# @param Object object: Die, CardDeck, etc.
# @param String unit: how many turns (e.g. 1,2,3)
# @param String type: whether even, odds, etc.
# @return Probability of n turns on an object collection with a types
# @begin decompose_object_to_probability
decompose_object_to_probability <- function(object, unit="1 turn", type="evens") {
  if(unit == "1 turn") {
    if(type == "evens") { print(length(object$evens()) / object$number_of_options()) }
    if(type == "odds") { print(length(object$odds()) / object$number_of_options()) }
  }
}
# @end decompose_object_to_probability
  
# required: independent events. e.g., rolling N on a die, and choosing even card
# @param Numeric set_of_chances 
# @param Numeric another_set_of_chances 
# @return Probability of successive chance
# @begin find_successive_probability
find_successive_probability <- function(set_of_chances, another_set_of_chances) {
  set_of_chances * another_set_of_chances
}
# @end find_successive_probability

# Given a test of probability, find probability
# of an element of the combination.
find_probability_given_combination <- function(x, y, z) {
  v <- x + y
  v - z
}
find_probability_given_combination(30.2, 39.9, 15.1)

# Given a test of probability of an event occurring within an interval,
# And the probability of an outcome one time is known
# And the number of attempts
# Then return the probability of a successful outcome (outcome meets expectation).
# This happens by subtracting two binomial distributions from eachother.
# @param Numeric between_first_number left_hand of interval
# @param Numeric between_second_number right_hand of interval
# @param Numeric number_of_samples how many attempts?
# @param Numeric probability probability of outcome
# @return binomial distribution of something happening at least x times.
# @begin what_is_probability_of_success
what_is_probability_of_success <- function(between_first_number=0, between_second_number=0, number_of_samples=0, probability=0) {
  pbinom(between_second_number, number_of_samples, probability) - pbinom((between_first_number - 1), number_of_samples, probability)
}
# @end what_is_probability_of_success
what_is_probability_of_success(
  between_first_number=11,
  between_second_number=20, 
  number_of_samples=37,
  probability=0.300
)

# Probability of Random Variable between from and two
# If variation then convert to standard deviation.
# @begin probability_of_rando_between
probability_of_random_variable_between <- function(mean=0, standard_deviation=1, from=0, to=0) {
  pnorm(to, mean=mean, sd=standard_deviation) - 
  pnorm(from, mean=mean, sd=standard_deviation)
}
# @end probability_of_rando_between

# Essentially this solves a probability for a flipped known.
# @begin solve_decision_tree
solve_decision_tree <- function(probability_a, probability_b, probability_c) {
  (probability_a * probability_b) + ((probability_a + probability_b) * probability_c)
}
# @end solve_decision_tree

# @return What is probability of independent events with two outcomes?
# @param List[Vector[Numeric, Numeric]] probability pairs, where
#        first number is 0 if NOT and 1 otherwise.
# @begin what_is_probability_of_sequence
what_is_probability_of_sequence <- function(sequence) {
  counter = 1
  for(s in sequence) {
    if(s[1] == 0) {
      s = 1 - s[2]
    } else {
      s = s[2]
    }
    counter = counter * s
  }
  counter
}
# @end what_is_probability_of_sequence

# @return What is stacked probability of an outcome?
#   i.e., dependent probability.
#   e.g., given probability of A and B, what is probability of A and NOT B?
# @param List[Vector] probability pairs
# @begin what_is_stacked_probability_outcome
# @usage -> what_is_stacked_probability_outcome(list(c(0.9, 0.50), c(0.08, 0.85), c(0.02, 0.6))) => 0.47
what_is_stacked_probability_outcome <- function(probabilities) {
  counter = 0
  for(p in probabilities) {
    counter = counter + (p[1] * p[2])
  }
  1 - counter
}
# @end what_is_stacked_probability_outcome

# called "complement" of n.
# @return What is stacked probability of an outcome with not?
#         if it does not work then try manually.
# @param List[Vector] probability pairs
# @begin what_is_stacked_probability_outcome
what_is_stacked_probability_outcome_with_not <- function(probabilities) {
  counter = 0
  for(p in probabilities) {
    counter = counter + (p[1] * (1 - p[2]))
  }
  counter
}
# @end what_is_stacked_probability_outcome

# @return What the probability of two mutually exclusive events
# @param Probability of event A
# @param Probability of event B
# @begin what_is_probability_of_two_mutually_exclusive_events_combined
what_is_probability_of_two_mutually_exclusive_events <- function(A, B) { A+B }
# @end what_is_probability_of_two_mutually_exclusive_events_combined

# Given I have probability of two events
# And I have probability of an event Given another event
# Then return probability of one event, separate from other.
# @param probability_of_both Two events
# @param given_that Given that A then B == .
# @begin what_is_probability_of_two_events_given_relation
what_is_probability_of_two_events_given_relation <- function(probability_of_both = 0, given_that = 0) {
  outcome = probability_of_both / given_that
  print(outcome)
}
# @end what_is_probability_of_two_events_given_relation

# @return What the probability of two dependent events is.
#         i.e.:
#           a. ((A), (B)) -> (A & B) [Given a and b what is probability of both?]
#           b. Given probability of A and a dependent B
#              What is probability of A and B?
# @param Probability of event A
# @param Probability of event B
# @begin what_is_probability_of_two_dependent_events
what_is_probability_of_two_dependent_events_ie_assumed_dependence <- function(A, B) {
  outcome = A * B
  print(outcome)
}
what_is_probability_of_two_dependent_events(0.08, 0.37)
what_is_probability_of_two_dependent_events(0.43, 0.992)
# @end what_is_probability_of_two_events

# @param Float a_or_b
# @param Float a
# @param Float b
# @begin test_of_mutual_exclusivity
test_of_mutual_exclusivity <- function(a_or_b, a, b) {
  a_or_b == a + b
}
# @end test_of_mutual_exclusivity

# @param Probability of an event.
# @param How many times to multiply the event.
# @return Likelihood of n events in a row.
# @begin what_is_probability_of_three_events
what_is_probability_of_n_events <- function(probability, number_of_events) {
  probability * number_of_events
}
what_is_probability_of_n_events((1-0.65), 2)
# @end what_is_probability_of_three_events

# @return What the probability of two independent events is.
# @param Probability of event A
# @param Probability of event B
# @param Probability of event C
# usage: whatIsProbabilityOfTwoEventsAndCombinedEvent(0.273, 0.403, 0.176)
what_is_probability_of_two_independent_events <- function(A, B, C) {
  (A+B-C)
}
what_is_probability_of_two_independent_events(0.273, 0.403, 0.176)

# @return Probability of Not
# @begin what_is_probability_that_this_and_not_that
what_is_probability_that_this_and_not_that <- function(a_what=0, both=0) {
  a_what - both
}
what_is_probability_that_this_and_not_that(a_what=0.7, both=0.2)
# @end what_is_probability_that_this_and_not_that

# @return Probability of Neither by finding difference of summing probability of either 
#         and probability of both.
# @begin what_is_probability_that_neither
# @param Float x
# @param Float y
# @param Float both
what_is_probability_that_neither <- function(x=0, y=0, both=0) {1 - ((x + y) - both)}
# @end what_is_probability_that_neither

# @return Probability of Either.
# @begin what_is_probability_that_either
what_is_probability_that_either <- function(a=0, b=0, both=0) {
  (a + (b) - both) 
}
what_is_probability_that_either(a=0.6, b=0.5, both=0.4)
# @end what_is_probability_that_either

# Conditional Probability Formula with assumption.
# @param x first dependent event
# @param y second dependent event
# @return probability of something given another thing and mutual non-exclusivity.
# @begin what_is_probability_given_that 
what_is_probability_given_that <- function(x, y) {
  (x * y) / y
}
# @end what_is_probability_given_that
# (0.43) (0.992) AND P(0.992|0.43) => 0.43
# what_is_probability_given_that(0.43, 0.992) => 0.43

# Essentially, if given a Maximum Bound earlier in problem,
#   then subtract sub-maximum for different maximum.
#   and subtract minimum from different maximum
#   and divide 1 by the number as A.
#   then subtract sub_problem_maximum from true_maximum as B
#   and multiply A and B.
# (e.g., (P(x > 12| x > 8)), (0, 23))
what_is_probability_between_two_integers <- function(true_maximum, sub_problem_maximum, true_minimum) {
  a <- 1/(true_maximum-true_minimum)
  b <- true_maximum - sub_problem_maximum
  a * b
}
what_is_probability_between_two_integers(12.5, 12.5, 1)


###################################
#            Tests                #
###################################
test_probability_that_neither <- function() {
  all.equal(what_is_probability_that_neither(0.6, 0.3, 0.2), 0.3)
}
test_probability_that_neither()

# So, what is probability of N events happening in a row?
test_what_is_probability_of_n_events <- function() {
  all.equal(what_is_probability_of_n_events(0.92, 3), 0.779)
}
test_what_is_probability_of_n_events()

test_what_is_probability_of_n_given_z <- function() {
  all.equal(what_is_probability_given_that(0.17, 0.3), 0.567)
}
test_what_is_probability_of_n_given_z()

test_what_is_probability_between_two_integers <- function() {
  all.equal(what_is_probability_between_two_integers(23, 12, 8), 0.73333333)
}
test_what_is_probability_between_two_integers()

test_what_is_probability_of_rando <- function() {
  all.equal(
    probability_of_rando_between(mean=102,standard_deviation=9,from=102, to=111),
    0.3413447
  )
}
test_what_is_probability_of_rando()

test_successive_chances <- function() {
  find_successive_probability(1/6, 12/52)
}