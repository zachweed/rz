library(methods)

# @begin TestOfIndependence
test_of_independence <- setRefClass("TestOfIndependence",
                              fields=list(
                                observed_value = "numeric",
                                expected_value = "numeric",
                              ),
                              methods=list(
                                what_is_expected_value_from_table = function(row_total, column_total, total_respondent) {
                                  (row_total*column_total)/total_respondent
                                },
                                # supply a zaetrix
                                build_contingency_table = function(data) {
                                  
                                },
                              )
)
