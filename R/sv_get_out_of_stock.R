
#' Find all out-of-stock (drop-shipped) products
#' @param drop_ship_only restrict to drop-shipped products (logical/scalar)
#' @export
sv_get_out_of_stock <- function(drop_ship_only = TRUE) {

  # load product data
  x <- sv_get_products()

  # find out-of-stock
  x <- dplyr::filter(x, QuantityAvailable == 0)

  # restrict to drop-ships
  if (drop_ship_only)
    x <- dplyr::filter(x, Classification == "Drop Ship")

  return(x)
}