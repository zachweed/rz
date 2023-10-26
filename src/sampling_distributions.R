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
                         find_probability_for_mean = function(lte=0, gte=0){
                           val = 0
                           if(lte > 0) {
                             val = pnorm(lte, sample_mean, standard_deviation/sqrt(sample_size))  
                           }
                           if(gte > 0) {
                             val = (1 - pnorm(gte, sample_mean, standard_deviation/sqrt(sample_size)))
                           }
                           if(gte > 0 && lte > 0) {
                             val = pnorm(lte, sample_mean, standard_deviation/sqrt(sample_size)) - pnorm(gte, sample_mean, standard_deviation/sqrt(sample_size))
                           }
                           val
                         }
                       )
)
# @end TScore

# Given a company with a product normally in N Fl. Oz.,
# And a standard deviation of N
# And an actual amount contained as A,
# What is the probability that a sample mean of B deviates C?
sdist <- sampling_distribution(
  sample_mean=12,
  standard_deviation=0.1, 
  sample_size=35
)
sdist$find_probability_for_mean(gte=12.04)


# Given a company with a product normally in N Fl. Oz.,
# And a standard deviation of N
# And an actual amount contained as A,
# What is the probability that a sample mean of is between Y and Z?
sdist <- sampling_distribution(
  sample_mean=12,
  standard_deviation=0.1, 
  sample_size=35
)
sdist$find_probability_for_mean(lte = 12.06, gte=11.97)


# Given a company with a product normally in N Fl. Oz.,
# And a standard deviation of N
# And an actual amount contained as A,
# What is the probability that a sample mean of is between Y and Z?
sdist <- sampling_distribution(
  sample_mean=4,
  standard_deviation=0.1, 
  sample_size=40
)
sdist$find_probability_for_mean(lte = 225)

#########################
#       Tests           #
#########################
