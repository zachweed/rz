library(methods)

# @begin TScore
t_score <- setRefClass("TScore",
                       fields=list(
                         x_bar="numeric",
                         standard_deviations="numeric",
                         sample_size="numeric",
                         mu="numeric",
                         score="numeric",
                         degrees_of_freedom="numeric"
                       ),
                       methods=list(
                         init_df = function(){
                           degrees_of_freedom <<- standard_deviations - 1
                         },
                         init_score = function(){
                           score <<- (x_bar - mu)/(standard_deviations/sqrt(sample_size))
                         }
                       )
)
# @end TScore

#########################
#       Tests           #
#########################

t <- t_score(x_bar=4, standard_deviations=2, sample_size=4, mu=2)

# Given A Standard Deviation of 3, DF should be 2.
t$init_df()
all.equal(t$degrees_of_freedom, 2)


# Given params, determine score.
t$init_score()
all.equal(round(t$score, 2), 1.41)
