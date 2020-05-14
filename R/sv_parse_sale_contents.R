#' Get sale items
#' @param x data returned from 'sv_get_sales'
#' @importFrom magrittr "%>%"
#' @export
sv_parse_sale_contents <- function(x) {

  v <- c("FulfilledKits", "MerchantKits", "FulfilledItems", "MerchantItems", "SaleItems", "SaleKits")
  v <- intersect(v, names(x))

  # extract sale items
  out <-
    purrr::map_df(
      v,
      function(vv) {
        out <- x %>%
          dplyr::select(Id, vv) %>%
          tidyr::unnest(cols = c(vv))
        if ("Price" %in% names(out))
          out <- dplyr::mutate(out, Price = as.numeric(stringr::str_sub(Price, 2, -1)))
        if ("Items" %in% names(out))
          out <- dplyr::select(out, -Items)
        out
      }
    )

  if (!("PartNumber" %in% names(out)))
    out$PartNumber <- NA_character_

  out <- out %>%
    dplyr::mutate_at("Sku", toupper) %>%                                # uppercase SKU
    tidyr::nest(Items = c(Sku, PartNumber, Quantity, Price)) %>%         # 1 row per SaleId
    dplyr::mutate(
      Total = purrr::map_dbl(Items, ~sum(.x$Quantity * .x$Price))       # order total
    ) %>%
    dplyr::left_join(
      dplyr::transmute(x, Id, ShippingCost = as.numeric(stringr::str_sub(ShippingCost, 2, -1))),
      by = "Id"
    )

  return(out)
}
