library(methods)

# @begin PopulationMean
population_mean <- setRefClass("PopulationMean",
                       fields=list(
                         t_score="numeric",
                         alpha="numeric",
                         standard_deviation="numeric",
                         sample_size="numeric",
                         moe="numeric"
                       ),
                       methods=list(
                         init_moe = function(){
                           moe <<- ((t_score * alpha / 2) * (sample_size / sqrt(standard_deviation)))
                         }
                       )
)
# @end PopulationMean

#########################
#       Tests           #
#########################

pm <- population_mean(t_score = 4, alpha = 2, standard_deviation = 4, sample_size = 2)

# Given Params, Define MOE.
pm$init_moe()
all.equal(pm$moe, 4)
