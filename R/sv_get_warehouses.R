#' Get warehouses
#' @importFrom magrittr "%>%"
#' @export
sv_get_warehouses <- function() {
  x <- sv_api_call(path = "inventory/getWarehouses", PageNumber = 0)
  x <- httr::content(x)$Warehouses
  sv_parse_response(x)
}
