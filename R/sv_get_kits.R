#' Get kits
#' @importFrom magrittr "%>%"
#' @export
sv_get_kits <- function(skus = NULL, include_weights = TRUE) {

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
  kits <- purrr::map_df(
    products,
    function(r) {
      vars <- names(r)[purrr::map_lgl(r, ~length(.x) == 1)]        # TODO: this ignores entries with > 1 length (e.g., multiple suppliers)
      tmp <- tibble::as_tibble(r[vars])
      r$KitLines <- purrr::map_df(r$KitLines, rlang::squash)       # TODO: get multi-item kits
      tmp$KitLines <- list(dplyr::as_tibble(r$KitLines))
      tmp$Statuses <- paste(r$Statuses, collapse = "; ")
      tmp <- dplyr::rename(tmp, Sku = SKU)
      tmp
    }
  ) %>%
    dplyr::mutate_at(c("LastModifiedDateTimeUtc", "AvailableQuantityLastModifiedDateTimeUtc"), sv_parse_datetime)

  # include weights (upon request)
  if (include_weights)
    kits <- sv_get_kit_weights(kits)

  return(kits)
}


#' Get kit weights
#' @param kits kit data from a call to \code{\link{sv_get_kits}}
#' @importFrom magrittr "%>%"
sv_get_kit_weights <- function(kits) {

  # get SKUs contained within each kit
  kit_items <- kits %>%
    dplyr::select(Sku, KitLines) %>%
    tidyr::unnest()

  # get weights for base SKUs; sum weights over kit items
  kit_weights <- kit_items %>%
    dplyr::left_join(
      sv_get_products(skus = unique(kit_items$SKU)),
      by = c("SKU" = "Sku")
    ) %>%
    dplyr::group_by(Sku) %>%
    dplyr::mutate(
      weight = dplyr::case_when(
        WeightUnit == "oz" ~ WeightValue / 16,
        TRUE ~ WeightValue
      )
    ) %>%
    dplyr::summarize(
      Weight = sum(Quantity * weight)
    )

  # join with original 'kits' data
  dplyr::left_join(kits, kit_weights, by = "Sku")
}