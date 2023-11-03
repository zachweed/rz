############################
# Logical Helper Functions #
############################

# Given a two outcomes of of an event,
# And an outcome of a combination of those events
# Then return complement without double-counted combination.
# @param List[Numeric] supposition
# @return Complement of both events without double counting.
# @begin complement_of
complement_of <- function(supposition, combination) {
  (supposition(1) + supposition(2)) - combination
}
# @end complement_of

# @param Vector values a vector of values to find a range from.
# @return Numeric
# @begin range
range <- function(values) {
  vs = lapply(values, sort)
  as.numeric(vs[length(vs)]) - as.numeric(vs[1])
}
# @end range

# @param List values to find combined likelihood of.
# @return Numeric
# @begin and_probability
and_probability <- function(l) { Reduce(l, f="*") }
# @end and_probability

# @param Numeric probability of one.
# @param Numeric probability of another.
# @param Numeric probability of both occurring.
# @return Boolean whether mutually exclusive.
# @begin mutually_exclusive
mutually_exclusive <- function(a, b, both) { 
  if(((a+b) == both)){
    print("are mutually exclusive")
  } else {
    print("are not mutually exclusive")
  }
}
# @end mutually_exclusive
