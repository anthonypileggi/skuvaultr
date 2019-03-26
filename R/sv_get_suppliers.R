#' Get suppliers
#' @importFrom magrittr "%>%"
#' @export
sv_get_suppliers <- function() {
  x <- sv_api(path = "products/getSuppliers", PageSize = 10000)
  sv_parse_response(x$Suppliers)
}