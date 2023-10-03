# @return What is stacked probability of an outcome?
#         i.e., dependent probability.
# @param List[Vector] probability pairs
# @begin what_is_stacked_probability
stacked_probability_outcome <- function(probabilities) {
  counter = 0
  for(p in probabilities) {
    counter = counter + (p[1] * p[2])
  }
  counter
}
# @end stacked_probability_outcome

# @return What the probability of two mutually exclusive events
# @param Probability of event A
# @param Probability of event B
# @begin what_is_probability_of_two_mutually_exclusive_events_combined
what_is_probability_of_two_mutually_exclusive_events_combined <- function(A, B) {
  A+B
}
# @end what_is_probability_of_two_mutually_exclusive_events_combined

# @return What the probability of two independent events is.
# @param Probability of event A
# @param Probability of event B
# usage: whatIsProbabilityOfTwoIndependentEvents((1/6), (12/52))
# @begin what_is_probability_of_two_events
probability_of_two_independent_events <- function(A, B) {
  fractions(A * B)
}
probability_of_two_independent_events((1/6), (12/52))
# @end

# @param Probability of an event.
# @param How many times to multiply the event.
# @return Likelihood of n events in a row.
# @begin what_is_probability_of_three_events
what_is_probability_of_three_events <- function(probability, number_of_events) {
  probability * number_of_events
}
what_is_probability_of_three_events(0.92, 3)
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
what_is_probability_that_neither <- function(A, B, C) {
  1 - ((A + B) - C)
}
what_is_probability_that_neither(0.6, 0.3, 0.2)

# @begin what_is_probability_given_that
# make sure to look for both.
# @param probability_of_both First dependent event
# @param given_what second dependent event
# @return probability of something given another thing
# @begin what_is_probability_given_that 
what_is_probability_given_that <- function(probability_of_both, probability_of_whats_given) {
  probability_of_both / probability_of_whats_given
}
# @end what_is_probability_given_that
what_is_probability_given_that(0.48, 0.08)


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
