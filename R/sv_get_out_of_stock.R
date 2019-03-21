
#' Find all out-of-stock (drop-shipped) products
#' @param drop_ship_only restrict to drop-shipped products (logical/scalar)
#' @export
sv_get_out_of_stock <- function(drop_ship_only = TRUE) {

  # load product data
  x <- sv_get_products()

  # ignore alternate skus/codes
  x <- dplyr::filter(x, !IsAlternateSKU, !IsAlternateCode)

  # ignore discontinued products
  x <- dplyr::filter(x, !stringr::str_detect(Statuses, "NLA|Inactive|Superseded"))

  # find out-of-stock (non-NLA)
  x <- dplyr::filter(x, QuantityAvailable == 0)

  # restrict to drop-ships
  if (drop_ship_only)
    x <- dplyr::filter(x, Classification == "Drop Ship")

  return(x)
}