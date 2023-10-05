# @return What is Nth percentile of a list?
# @param Vector a list of values 
# @begin percentiles
percentiles <- function(list, whatPercentile) {
  list = sort(list)
  L = (whatPercentile / 100) * length(list)
  print(L)
  L = ceiling(L)
  list[L]
}
# @end percentiles
percentiles(c(101, 130, 156, 165, 251, 277, 311, 324, 339, 404), 30)

# @return IQR
# @begin inter_quartile_range
inter_quartile_range <- function(list) {
  IQR(list)
}
# @end inter_quartile_range

interQuartileRange(c(68, 68, 71, 72, 75, 78, 79, 80, 80, 90, 92, 92, 95, 97, 99, 100))
percentiles(c(101, 130, 156, 165, 251, 277, 311, 324, 339, 404), 30)
