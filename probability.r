# Independence Rule
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
what_is_probability_of_sequence(
  list(
    c(1, 0.8),
    c(1, 0.8),
    c(1, 0.8)
  )
)

# @return What is stacked probability of an outcome?
#         i.e., dependent probability.
# @param List[Vector] probability pairs
# @begin what_is_stacked_probability
what_is_stacked_probability_outcome <- function(probabilities) {
  counter = 0
  for(p in probabilities) {
    counter = counter + (p[1] * p[2])
  }
  counter
}
# @end what_is_stacked_probability

what_is_stacked_probability_outcome(list(
  # supra-attribute, sub-attribute
  c(0.9, 0.6),
  # supra-attribute, sub-attribute
  c(0.08, 0.75),
  # supra-attribute, sub-attribute
  c(0.02, 0.60)
))


# @return What the probability of two mutually exclusive events
# @param Probability of event A
# @param Probability of event B
# @begin what_is_probability_of_two_mutually_exclusive_events_combined
what_is_probability_of_two_mutually_exclusive_events_combined <- function(A, B) {
  A+B
}
what_is_probability_of_two_mutually_exclusive_events_combined(0.15, 0.1)
# @end what_is_probability_of_two_mutually_exclusive_events_combined

# @return What the probability of two dependent events is.
# @param Probability of event A
# @param Probability of event B
# @begin what_is_probability_of_two_dependent_events
what_is_probability_of_two_dependent_events <- function(A, B) {
  fractions(A * B)
}
what_is_probability_of_two_dependent_events(0.05, 0.42)
# @end what_is_probability_of_two_events

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

# @return Given:
#   1. probability of one event
#   2. probability of another event
#   3. and probability of both events
#   4. provide probability of neither event.
# @param A One event's probability.
# @param B another event's probability.
# @param C probability of both.
# @begin what_is_probability_that_neither
what_is_probability_that_neither <- function(A, B, C) {
  1 - ((A + B) - C)
}
what_is_probability_that_neither(0.7, 0.4, 0.3)
# @end what_is_probability_that_neither

# Conditional Probability Formula.
# @param probability_of_both First dependent event
# @param given_what second dependent event
# @return probability of something given another thing
# @begin what_is_probability_given_that 
what_is_probability_given_that <- function(probability_of_both, probability_of_whats_given) {
  probability_of_both / probability_of_whats_given
}
# @end what_is_probability_given_that
what_is_probability_given_that(0.02, 0.05)


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
