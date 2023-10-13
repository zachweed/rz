library(methods)

# @begin SamplingDistribution
sampling_distribution <- setRefClass("SamplingDistribution",
                       fields=list(
                         z_score="numeric",
                         sample="list",
                         population="list",
                         sample_mean="numeric",
                         sample_size="numeric",
                         population_mean="numeric",
                         # Variability in population
                         σ="numeric",
                         # Standard Deviation of Sample Statistic (Variability of Sample)
                         s="numeric",
                         # Standard Error of the Mean
                         σxbar="numeric",
                         standard_error="numeric",
                         standard_deviation="numeric"
                       ),
                       methods=list(
                         find_probability_for_mean_lteq = function(lteq=1){
                           pnorm(lteq, sample_mean, standard_deviation/sqrt(sample_size))
                         }
                       )
)
# @end TScore

#########################
#       Tests           #
#########################
sdist <- sampling_distribution(
  sample_mean=90,
  standard_deviation=15, 
  sample_size=25
)
all.equal(sdist$find_probability_for_mean_lteq(lteq=2.95), 0.09121122)