library(methods)

# @begin MeanSquare
MeanSquare <- setRefClass("MeanSquare",
  fields=list(
    # this and ms_within are same when h0 is true. 
    ms_between="numeric",
    # comparison of each group to to its own mean.
    ms_within="numeric",
    ss_within="numeric",
    ss_total="numeric"
  ),
  methods=list(
    f_ratio = function() {
      ms_between / ms_within
    }
    calculate_ss_between = function() {
      ss_total - ss_within
    }
  )
)