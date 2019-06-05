#' Get individual items contained within a sale
#' @param x data returned from 'sv_get_sales'
#' @importFrom magrittr "%>%"
#' @export
sv_parse_order_contents <- function(x) {

  # extract order components
  v <- c("FulfilledKits", "MerchantKits", "FulfilledItems", "MerchantItems", "SaleKits", "SaleItems")
  v <- intersect(v, names(x))
  out <- purrr::map_df(v, ~tidyr::unnest(dplyr::select(x, Id, .x)))

  # unnest kits
  if ("Items" %in% names(out)) {
    out <- dplyr::mutate(out, is_kit = !purrr::map_lgl(Items, is.null))
    kits <- out %>%
      dplyr::filter(is_kit) %>%
      tidyr::unnest() %>%
      dplyr::select(Id, Sku = Sku1, Quantity = Quantity1)
    parts <- out %>%
      dplyr::filter(!is_kit) %>%
      dplyr::select(Id, Sku, Quantity)
    out <- dplyr::bind_rows(kits, parts) %>%
      dplyr::arrange(Id)
  } else {
    out <- dplyr::select(out, Id, Sku, Quantity)
  }

  # uppercase
  out <- dplyr::mutate(out, Sku = toupper(Sku))

  # nest (i.e., 1 row per SaleId)
  out <- tidyr::nest(out, -Id, .key = "Items")

  return(out)
}