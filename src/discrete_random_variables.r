# @return expected value of a roll return
# @begin roll
roll <- function(win_probability = 0, win_amount = 0, lose_probability = 0, lose_amount = 0) {
  (  (win_probability * win_amount) 
   + (lose_probability * lose_amount)
  )
}
roll(
  win_probability = 0.41,
  win_amount = 1,
  lose_probability = 0.49,
  lose_amount = -1
)
# @end roll
