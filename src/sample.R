library(methods)

# @begin Sample
Sample <- setRefClass("Sample",
  fields=list(
    size = "numeric",
    sample_average = "numeric",
    standard_deviation = "numeric",
    sample = "numeric"
  ),
  methods=list(
    initialize_sample_and_size = function(sample) {
      sample <<- sample
      size <<- length(sample)
    },
    initialize_sample = function(sample) {
      sample <<- sample
    }
  )
)