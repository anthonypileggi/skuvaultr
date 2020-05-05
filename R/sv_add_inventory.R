#' Add inventory
#' @param ... args passed on to \code{\link{sv_update_inventory}}
#' @export
sv_add_inventory <- function(...) {
  sv_update_inventory(..., type = "add")
}