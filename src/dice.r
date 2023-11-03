#########################
# Dice Helper Functions #
#########################

# @begin how_many_combinations
# @param Numeric number of dice
# @return six for each singular dice.
how_many_combinations <- function(dice_number) {
  6 ^ dice_number
}
# @end how_many_combinations

# @param target_sum: Expected value
# @param dice_number: number of dice
# @param number_of_rolls: how many rolls of dice
# @param Numeric number of dice
# @return Likelihood of a sum being returned from so many rolls of dice.
# @begin likelihood_of_sum
likelihood_of_sum <- function(target_sum=0, dice_number=0, number_of_rolls=0) {
  modifier = 0
  if(target_sum == 4) {
    modifier = 3
  }
  if(target_sum >= 7){
    modifier = 6 - (target_sum - 7)
  }
  pbinom(1, number_of_rolls, modifier / how_many_combinations(dice_number))
}
# @end likelihood_of_sum

# @param win_probability: probability of winning
# @param win_amount: how much?
# @param lose_probability: probability of losing?
# @param lose_amount: how much?
# @return expected value of a roll return
# @begin roll
roll <- function(win_probability = 0, win_amount = 0, lose_probability = 0, lose_amount = 0) {
  (  (win_probability * win_amount) 
   + (lose_probability * lose_amount)
  )
}
# @end roll