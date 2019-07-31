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
      function(v) {
        out <- sales %>%
          dplyr::select(Id, v) %>%
          tidyr::unnest()
        if ("Price" %in% names(out))
          out <- dplyr::mutate(out, Price = as.numeric(stringr::str_sub(Price, 2, -1)))
        if ("Items" %in% names(out))
          out <- dplyr::select(out, -Items)
        out
      }
    )
  
  # uppercase
  out <- dplyr::mutate_at(out, "Sku", toupper)
  
  # nest (i.e., 1 row per SaleId)
  out <- tidyr::nest(out, -Id, .key = "Items")
  
  return(out)
}
