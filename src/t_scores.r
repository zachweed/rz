library(methods)

# @begin TScore
t_score <- setRefClass("TScore",
                       fields=list(
                         sample_mean="numeric",
                         standard_deviation="numeric",
                         sample_size="numeric",
                         mu="numeric",
                         score="numeric",
                         degrees_of_freedom="numeric",
                         confidence_level="numeric",
                         alpha="numeric",
                         t_critical_value="numeric",
                         t_actual_value="numeric",
                         margin_of_error="numeric",
                         sample="list",
                         interval_lower="numeric",
                         interval_upper="numeric"
                       ),
                       methods=list(
                         init_df = function(){
                           sample_size <- length(sample)
                           degrees_of_freedom <<- sample_size - 1
                         },
                         init_sample_size = function(){
                           sample_size <<- length(sample)
                         },
                         init_sample_mean = function(){
                           sample_mean <<- mean(unlist(sample))
                         },
                         init_score = function(){
                           score <<- (sample_mean - mu)/(standard_deviation/sqrt(sample_size))
                         },
                         init_alpha = function() {
                           alpha <<- 1 - confidence_level
                         },
                         init_t_critical_value = function() {
                           if(sample_size == 0){
                             init_sample_size()
                           }
                           init_alpha()
                           t_critical_value <<- alpha/2
                         },
                         init_t_actual_value = function() {
                           if(degrees_of_freedom == 0){
                             init_df()  
                           }
                           init_t_critical_value()
                           t_actual_value <<- -qt(t_critical_value, degrees_of_freedom)
                         },
                         init_standard_deviation = function() {
                           standard_deviation <<- sd(unlist(sample))
                         },
                         init_margin_of_error = function() {
                           if(standard_deviation == 0){
                            init_standard_deviation()  
                           }
                           if(sample_mean == 0){
                             init_sample_mean()  
                           }
                           init_t_actual_value()
                           margin_of_error <<- (t_actual_value * (standard_deviation/sqrt(sample_size)))
                         },
                         x_lt_t = function() {
                           print(pt(score, degrees_of_freedom))
                         },
                         x_gt_t = function() {
                           print(1 - pt(score, degrees_of_freedom))
                         },
                         x_lteq_t = function() {
                           print(qt(score, degrees_of_freedom))
                         },
                         find_confidence_interval = function() {
                           init_margin_of_error()
                           interval_lower <<- sample_mean - margin_of_error
                           interval_upper <<- sample_mean + margin_of_error
                           print("true population mean is:")
                           list(interval_lower, interval_upper)
                         },
                         find_t_value_for_confidence_interval_with_sample_size_and_mean = function(confidence_interval=0, sample_size=0, sample_mean=0, standard_deviation) {
                           t_critical_value <- qt(confidence_interval, sample_size - 1)
                           margin_of_error_lower <- sample_mean - t_critical_value * (standard_deviation/sqrt(sample_size))
                           margin_of_error_upper <- sample_mean + t_critical_value * (standard_deviation/sqrt(sample_size))
                           list(margin_of_error_lower, margin_of_error_upper)
                         }
                       )
)
# @end TScore

#########################
#       Tests           #
#########################

t <- t_score(sample_mean=4, standard_deviation=2, sample_size=4, mu=2)

# Given 40 Degrees of Freedom, Calculate P(x<T)
t <- t_score(sample_mean=4, standard_deviation=2, sample_size=4, mu=2)
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

# Given A list of babies with chemicals, find mean range for given confidence interval
t <- t_score(
  sample=list(90, 145, 147, 160, 116, 100, 159, 151, 156, 126, 137, 83, 156, 94, 121, 144, 123, 114, 139, 99),
  confidence_level=0.90
)
t$find_confidence_interval()
all.equal(t$interval_lower, 118.3408)
all.equal(t$interval_upper, 137.6592)
