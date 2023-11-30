library(methods)

# @begin Sample
Sample <- setRefClass("Sample",
  fields=list(
    size = "numeric",
    sample_average = "numeric",
    standard_deviation = "numeric"
  )
)