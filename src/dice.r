# @begin how_many_combinations
# @param Numeric number of dice
how_many_combinations <- function(dice_number) {
  6 ^ dice_number
}
# @end how_many_combinations

# @begin likelihood_of_sum
# @param Numeric number of dice
likelihood_of_sum <- function(target_sum, dice_number, number_of_rolls) {
  modifier = 0
  if(target_sum == 4) {
    modifier = 3
  }
  if(target_sum >= 7){
    modifier = 6 - (target_sum - 7)
  }
  pbinom(1, number_of_rolls, modifier / how_many_combinations(dice_number))
}
# @end how_many_combinations
likelihood_of_sum(11, 2, 5)
