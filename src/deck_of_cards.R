library(methods)

# Exists in order to convert a matrix to a series of columns and rows for Anova, etc.
# @begin CustomMatrix
CardDeck <- setRefClass("CardDeck",
  fields = list(number_of_decks="numeric"),
  methods = list(
    evens = function() { rep(c(2,4,6,8,10), 4) },
    odds = function() { rep(c(1,3,5,7,9), 4) },
    number_of_options = function() { 52 }
  )
)
