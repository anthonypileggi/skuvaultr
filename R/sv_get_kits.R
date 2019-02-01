#' Get kits
#' @importFrom magrittr "%>%"
#' @export
sv_get_kits <- function(skus = NULL) {

  products <-
    sv_api(
      path = "products/getKits",
      PageSize = 10000,
      KitSKUs = skus,
      GetAvailableQuantity = TRUE,
      IncludeKitCost = TRUE
      )

  # exit early if there is no data
  if (length(products) == 0) {
    message("No kits matching the requested SKUs were found.")
    return(NULL)
  }

  # parse response as tibble
  purrr::map_df(
    products,
    function(r) {
      vars <- names(r)[purrr::map_lgl(r, ~length(.x) == 1)]        # TODO: this ignores entries with > 1 length (e.g., multiple suppliers)
      tmp <- tibble::as_tibble(r[vars])
      r$KitLines <- purrr::map_df(r$KitLines, rlang::squash)       # TODO: get multi-item kits
      tmp$KitLines <- list(dplyr::as_tibble(r$KitLines))
      tmp <- dplyr::rename(tmp, Sku = SKU)
      tmp
    }
  ) %>%
    dplyr::mutate_at(c("LastModifiedDateTimeUtc", "AvailableQuantityLastModifiedDateTimeUtc"), sv_parse_datetime)
}
