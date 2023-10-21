# @begin percentile_from_z_score
# If Z_score isn't provided, find closest value
# to percentile in z-table and convert.
percentile_from_z_score <- function(mean=0, standard_deviation=0, z_score=0) {
  # i.e. mu, + z_score * sd
  (mean + ((z_score) * standard_deviation))
}
percentile_from_z_score(mean=45000, standard_deviation=10000, z_score=1.28)
# @end percentile_from_z_score

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
