# @return expected value of a roll return.
# @begin roll
roll <- function(win_probability = 0, win_amount = 0, lose_probability = 0, lose_amount = 0) {
  (  (win_probability * win_amount) 
   + (lose_probability * lose_amount)
  )
}
roll(
  win_probability = 0.06, 
  win_amount = 90, 
  lose_probability = 0.94, 
  lose_amount = -20
)
# @end roll
