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