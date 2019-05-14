#' Get inventory locations
#' @export
sv_get_inventory_locations <- function(skus = NULL) {

  x <- sv_api(path = "inventory/getInventoryByLocation", ProductSKUs = skus)

  # clean data
  x <- x[purrr::map_int(x, length) > 0]   # remove products w/out warehouse location data
  purrr::map_df(
    names(x),
    function(a) {
      purrr::map_df(x[[a]], function(b) dplyr::bind_cols(dplyr::tibble(Sku = toupper(a)), dplyr::as_tibble(b)))
    }
  )
}