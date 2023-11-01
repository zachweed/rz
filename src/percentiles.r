library(methods)

# @begin Percentile
percentile <- setRefClass("Percentile",
                       fields=list(
                         x="numeric"
                       ),
                       methods=list(
                         for_sample_mean <- function(k=0.0, mu=0.0, standard_deviation=0.0, sample_size=0.0) {
                           # percentileForSampleMean(k = 0.99, mu = 18, standard_deviation = sqrt(324), sample_size = 49)
                           qnorm(k, mu, standard_deviation/sqrt(sample_size))
                         },
                         # validation: https://www.desmos.com/calculator/rephn2eswe
                         of_int_less_than <- function(mu=0.0, standard_deviation=0.0, lessThanWhat=0.0, sample_size=0.0){
                           pOfXLessThan(mu=40, standard_deviation=sqrt(100), lessThanWhat=42, sample_size=50)
                           pnorm(lessThanWhat, mu, standard_deviation/sqrt(sample_size))
                         },
                         # https://www.desmos.com/calculator/rephn2eswe
                         # ofIntGreaterThan(mu=34, standard_deviation=15, greaterThanWhat=30, sample_size=100)
                         of_int_greater_than <- function(mu=0.0, standard_deviation=0.0, greaterThanWhat=0.0, sample_size=0.0){
                           1 - pnorm(greaterThanWhat, mu, standard_deviation/sqrt(sample_size))
                         },
                         find_mean <- function(upper_bound=0, lower_bound=0, sample_size=0) {
                           sample_size * ((upper_bound+lower_bound)/2)  
                         },
                         # σχ
                         sample_standard_deviation <- function(standard_deviation = 0.0, sample_size = 0.0) {
                           standard_deviation/sqrt(sample_size)
                         },
                         standard_deviation <- function(upper_bound=0, lower_bound=0, sample_size=0) {
                           sqrt(sample_size) * sqrt(((upper_bound-lower_bound)^2)/12)
                         },
                         nth_percentile <- function(k=0.0, mean=0.0, standard_deviation=0.0, sample_size=0.0){
                           qnorm(k, mean, standard_deviation/sqrt(sample_size))
                         },
                         percentile_from_z_score <- function(mean=0, standard_deviation=0, z_score=0) {
                           # i.e. mu, + z_score * sd
                           (mean + ((z_score) * standard_deviation))
                         },
                         nth_percentile <- function(list, whatPercentile) {
                           list = sort(list)
                           L = (whatPercentile / 100) * length(list)
                           print(L)
                           L = ceiling(L)
                           list[L]
                         },
                         inter_quartile_range <- function(list) {
                           IQR(list)
                         }
                       )
)