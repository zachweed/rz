library(methods)

# Exists in order to convert a matrix to a series of columns and rows for Anova, etc.
# @begin CustomMatrix
CustomMatrix <- setRefClass("CustomMatrix",
  fields = list(
    data="matrix",
    ncol="numeric",
    nrow="numeric",
    list_of_columns="list",
    sums_of_columns="list"
  ),

  methods = list(
    build_list_of_columns = function() {
      mt <- list()
      for(column_index_left_right in 1:ncol){
        row <- list()
        for(row_index_top_down in 1:nrow){
          row <- append(row, data[,column_index_left_right][row_index_top_down])
        }
        mt <- append(mt, list(row))
      }
      list_of_columns <<- mt
      list_of_columns
    },
    build_sums_of_columns = function() {
      sms_of_columns <- list()
      build_list_of_columns()
      sm <- 0
      for(column_index in 1:ncol) {
        for(column_sub_index in 1:ncol) {
          sm <- sm + list_of_columns[column_index][column_sub_index]
        }
        sms_of_columns <- append(sms_of_columns, list(sm))
        sm <- 0
      }
      sums_of_columns <<- sms_of_columns
    }
  )
)
