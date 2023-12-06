library(methods)

# @begin TwoSampleProportions
two_sample_proportions <- setRefClass("TwoSampleProportions",
                                          fields=list(
                                            # Provide values in Descending order.
                                            # Where sub-a and sub-b are medications a and b.
                                            # numerators of proportions
                                            x_sub_a = "numeric",x_sub_b = "numeric",
                                            # denominators of proportions
                                            n_sub_a = "numeric", n_sub_b = "numeric"
                                          ),
                                          methods=list(
                                            perform_test = function(e) {
                                              # @param alternative e.g. "less", etc.
                                              prop.test(x = c(x_sub_a, x_sub_b), n = c(n_sub_a, n_sub_b), correct = FALSE, alternative = e)
                                            },
                                            p_sub_c = function() {
                                              (x_sub_a + x_sub_b)/(n_sub_a+n_sub_b)
                                            },
                                            z_stat = function() {
                                              psample_A <- x_sub_a / n_sub_a
                                              psample_B <- x_sub_b / n_sub_b
                                              psample_pooled <- (x_sub_a + x_sub_b) / (n_sub_a + n_sub_b)
                                              (psample_A - psample_B) / sqrt(psample_pooled * (1-psample_pooled) * (1/n_sub_a + 1/n_sub_b))
                                            },
                                            two_tailed_p_value = function() {
                                              2 * pnorm(z_stat())
                                            },
                                            left_tailed_p_value = function() {
                                              1 - pnorm(z_stat())
                                            }
                                          )
TwoSampleProportions <- setRefClass("TwoSampleProportions",
  fields=list(
    # Provide values in Descending order.
    # Where sub-a and sub-b are medications a and b.
    # numerators of proportions
    sample_one = "Sample",
    sample_two = "Sample"
  ),
  methods=list(
    perform_test = function(e) {
      # @param alternative e.g. "less", etc.
      prop.test(x = c(sample_one$sample_average, sample_two$sample_average), n = c(sample_one$size, sample_two$size), correct = FALSE, alternative = e)
    },
    p_sub_c = function() {
      (x_sub_a + x_sub_b)/(n_sub_a+n_sub_b)
    },
    z_stat = function() {
      psample_A <- x_sub_a / n_sub_a
      psample_B <- x_sub_b / n_sub_b
      psample_pooled <- (x_sub_a + x_sub_b) / (n_sub_a + n_sub_b)
      (psample_A - psample_B) / sqrt(psample_pooled * (1-psample_pooled) * (1/n_sub_a + 1/n_sub_b))
    },
    two_tailed_p_value = function() {
      2 * (1 - pnorm(z_stat()))
    },
    left_tailed_p_value = function() {
      1 - pnorm(z_stat())
    }
  )
)
