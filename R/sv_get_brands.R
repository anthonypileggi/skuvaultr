#' Get brands
#' @importFrom magrittr "%>%"
#' @export
sv_get_brands <- function() {
  x <- sv_api(path = "products/getBrands", PageSize = 10000)
  sv_parse_response(x)
}
