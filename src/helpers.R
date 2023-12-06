tail_for_symbol <- function(symbol) {
  if(all.equaL(symbol, ">")) {
    print("right")
  }
  if(all.equaL(symbol, "<")) {
    print("left")
  }
  if(all.equaL(symbol, "=")) {
    print("two-tailed")
  }
}

sample_sd_of_data_points <- function(data) {sd(data)}

population_sd_of_data_points <- function(data) {
  sqrt(
    (sum((
      data - mean(data)
    )^2) / length(data))
  )
}