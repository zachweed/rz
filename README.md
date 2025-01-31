# rz

### What rz Is

`rz` is a series of classes and utility methods for applying statistics concepts through computation. In other words, while existing code is there for handling statistics-related work within R lang, this work composes:

- Composes said helper methods within modules.
- Makes these methods available while incorporating Object Oriented principles.
- And, in some cases, performs tests of said code.

### How rz Works

Groups of functions have been grouped into centralized concepts with differentiation on `two_sample_hypothesis_tests` and `one_sample_hypothesis_tests`, and both `normal_distributions` and `uniform_distributions`. Find what you are looking for, plug in appropriate instance variables, and call an appropriate function.

### Fuzzy Search

If for whatever reason a function is not listed and one feels like it should be, they can search for it like so:

```
FuzzySearch("p-value", "/Users/user/Development/rz/src").search_for_string()

File /Users/user/Development/rz/src/null_hypothesis.r at line 0 could contain this function
File /Users/user/Development/rz/src/null_hypothesis.r at line 1 could contain this function
File /Users/user/Development/rz/src/two_sample_hypothesis.r at line 0 could contain this function
File /Users/user/Development/rz/src/two_sample_proportions.R at line 0 could contain this function
File /Users/user/Development/rz/src/anova.r at line 0 could contain this function
File /Users/user/Development/rz/src/anova.r at line 1 could contain this function
File /Users/user/Development/rz/src/p_value.r at line 0 could contain this function
File /Users/user/Development/rz/src/goodness_of_fit.R at line 0 could contain this function
File /Users/user/Development/rz/src/goodness_of_fit.R at line 1 could contain this function
File /Users/user/Development/rz/src/goodness_of_fit.R at line 2 could contain this function
File /Users/user/Development/rz/src/chi_squared.R at line 0 could contain this function
File /Users/user/Development/rz/src/null_hypothesis.r at line 0 could contain this function
File /Users/user/Development/rz/src/null_hypothesis.r at line 1 could contain this function
File /Users/user/Development/rz/src/two_sample_hypothesis.r at line 0 could contain this function
File /Users/user/Development/rz/src/two_sample_proportions.R at line 0 could contain this function
File /Users/user/Development/rz/src/anova.r at line 0 could contain this function
File /Users/user/Development/rz/src/anova.r at line 1 could contain this function
File /Users/user/Development/rz/src/p_value.r at line 0 could contain this function
File /Users/user/Development/rz/src/goodness_of_fit.R at line 0 could contain this function
File /Users/user/Development/rz/src/goodness_of_fit.R at line 1 could contain this function
File /Users/user/Development/rz/src/goodness_of_fit.R at line 2 could contain this function
File /Users/user/Development/rz/src/chi_squared.R at line 0 could contain this function
```

### Binomial Distributions

  * `#derive_mean_and_or_expected_value_from_X`
  * `#find_standard_deviation_of_a_binomial_random_variable`
  * `#what_is_probability_of_success`
  * `#what_is_p_of_x_when_equals`
  * `#what_is_p_of_x_when_lte`
  * `#what_is_ranged_probability`
  * `#test_what_is_p_of_x`
  * `#test_what_is_ranged_probability`

### CentralLimitTheorem

  * `#standard_error_of_mean`
  * `#clt_for_sums`

### ConfidenceIntervals

  * `#standard_error`
  * `#find_critical_value_with_confidence_level`
  * `#find_critical_value_with_df`
  * `#find_margin_of_error_of_mean`
  * `#simply_construct_confidence_interval`
  * `#construct_confidence_interval_easy`
  * `#derive_mean_and_or_expected_value_from_X`
  * `#derive_population_mean`
  * `#find_z_score_where_x_lte`
  * `#find_confidence_interval`
  * `#find_margin_of_error_from_confidence_interval`
  * `#find_sample_mean_from_confidence_interval`
  * `#find_sample_size_for_moe`
  * `#optimal_sample_size`
  * `#test_margin_of_error_of_mean`

### Dependence

  * `#is_independent`
  * `#is_dependent`
  * `#is_mutually_exclusive_with_both`
  * `#is_mutually_exclusive_without_both`

### Dice

  * `#how_many_combinations`
  * `#likelihood_of_sum`
  * `#roll`

### ExpectedValues

  * `#expected_value`

### Logic

  * `#complement_of`
  * `#range`
  * `#and_probability`
  * `#mutually_exclusive`

### NormalDistributions

  * `#sample_size`
  * `#standard_error_of_mean`
  * `#n_sdevs_from_mean`
  * `#z_score`
  * `#percentile_from_z_score`
  * `#test_what_is_z_score`

### OneSampleHypothesis
  
  * `#calculate_rejection_region`
  * `#calculate_critical_value`
  * `#test_p_value`
  * `#calculate_rejection_region`
  * `#calculate_p_value_for_normal_distribution_for_mean`
  * `#calculate_p_value_for_proportion_hypothesis_test`
  * `#calculate_test_type`
  * `#find_one_sample_test_statistic`
  * `#find_two_sample_test_statistic`

### PairedSamples

  * `#z_score`
  * `#standard_deviation_of_differences`
  * `#mean_of_differences`

### Percentiles

* `#for_sample_mean`
* `#of_int_less_than`
* `#of_int_greater_than`
* `#find_mean`
* `#sample_standard_deviation`
* `#standard_deviation`
* `#nth_percentile`
* `#percentile_from_z_score`
* `#interquartile_range`

### Probability

#### TBD

### Proportions

#### TBD

### SamplingDistributions

#### TBD

### Sums

#### TBD

### TScores

#### TBD

### UniformDistribution

#### TBD

### Helpers

#### TBD

### TwoSampleProportions

#### TBD
