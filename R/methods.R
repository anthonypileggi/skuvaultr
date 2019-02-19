#' Create a generic function for displaying data
#' @note Use to make a pretty DT::datatable
#' @export
display <- function(x, ...) UseMethod("display")
