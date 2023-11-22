devtools::install_github("cardiomoon/webr")
library(methods)

# @begin PairedSamples
paired_samples <- setRefClass("PairedSamples",
                                          fields=list(
                                            sample_a = "vector",
                                            sample_b = "vector",
                                            # population difference, i.e. mu_d
                                            overall_population_difference = "numeric",
                                            # x_bar_d, i.e. mean difference 
                                            overall_mean_difference = "numeric",
                                            # s_d
                                            overall_standard_deviation = "numeric",
                                            # n
                                            overall_number_of_differences = "numeric"
                                          ),
                                          methods=list(
                                            z_score = function() {
                                              ( ( overall_mean_difference - overall_population_difference )/(overall_standard_deviation/sqrt(overall_number_of_differences)) )
                                            },
                                            standard_deviation_of_differences = function() {
                                              sd(sample_a - sample_b)
                                            },
                                            mean_of_differences = function() {
                                              differences <- (sample_a - sample_b)
                                              (sum(differences))/length(differences)
                                            }
                                          )
)
