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
                         },
                         x_lt_t = function() {
                           print(pt(score, degrees_of_freedom()))
                         },
                         x_gt_t = function() {
                           print(1 - pt(score, degrees_of_freedom()))
                         },
                         x_lteq_t = function() {
                           print(qt(score, degrees_of_freedom()))
                         }
                       )
)
# @end TScore

#########################
#       Tests           #
#########################

t <- t_score(x_bar=4, standard_deviations=2, sample_size=4, mu=2)

# Given 40 Degrees of Freedom, Calculate P(x<T)
t <- t_score(x_bar=4, standard_deviations=2, sample_size=4, mu=2)
t$init_df()
all.equal(t$degrees_of_freedom, 2)

# Given A Standard Deviation of 3, DF should be 2.
t$init_df()
all.equal(t$degrees_of_freedom, 2)

# Given params, determine score.
t$init_score()
all.equal(round(t$score, 2), 1.41)

# Given T 1.05, find P(X < 1.05 with 25 df)
t <- t_score(score=1.05, degrees_of_freedom=25)
all.equal(t$degrees_of_freedom, 0.8481186)

# Given T 0.9, find P(X > 1.05 with 25 df)
t <- t_score(score=0.9, degrees_of_freedom=25)
all.equal(t$degrees_of_freedom, 0.1883541)

# Given T 0.9, find P(X <= 0.9 with 25 df)
t <- t_score(score=0.9, degrees_of_freedom=25)
all.equal(t$degrees_of_freedom, 1.316345)
