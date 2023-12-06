library(methods)

# Exists in order to convert a matrix to a series of columns and rows for Anova, etc.
# @begin CustomMatrix
Die <- setRefClass("Die",
  fields = list(number_of_die="numeric"),
  methods = list(
    # @begin how_many_combinations
    # @param Numeric number of dice
    # @return six for each singular dice.
    how_many_combinations = function() { 6 ^ number_of_die },
    # @end how_many_combinations
    
    # @param target_sum: Expected value
    # @param dice_number: number of dice
    # @param number_of_rolls: how many rolls of dice
    # @param Numeric number of dice
    # @return Likelihood of a sum being returned from so many rolls of dice.
    # @begin likelihood_of_sum
    likelihood_of_sum = function(target_sum=0, dice_number=0, number_of_rolls=0) {
      modifier = 0
      if(target_sum == 4) {
        modifier = 3
      }
      if(target_sum >= 7){
        modifier = 6 - (target_sum - 7)
      }
      pbinom(1, number_of_rolls, modifier / how_many_combinations(dice_number))
    },
    # @end likelihood_of_sum
    
    # @begin check_for_independence
    # @param Float x
    # @param Float y
    # @param Float x_and_y
    check_for_independence = function(x, y, x_and_y) { x_and_y == x * y },
    # @end check_for_independence
    
    # @param win_probability: probability of winning
    # @param win_amount: how much?
    # @param lose_probability: probability of losing?
    # @param lose_amount: how much?
    # @return expected value of a roll return
    # @begin roll
    roll = function(win_probability = 0, win_amount = 0, lose_probability = 0, lose_amount = 0) {
      (  (win_probability * win_amount) 
         + (lose_probability * lose_amount)
      )
    }
    # @end roll
  )
)