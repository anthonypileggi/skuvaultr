#' Get kits
#' @param skus skus (character/vector)
#' @param include_details include kit details? (weight/brand)
#' @importFrom magrittr "%>%"
#' @export
sv_get_kits <- function(skus = NULL, include_details = TRUE) {

  # submit unique skus only
  if (!is.null(skus))
    skus <- unique(toupper(skus))

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
      r$KitLines <- suppressWarnings(purrr::map_df(r$KitLines, rlang::squash))       # TODO: get multi-item kits
      tmp$KitLines <- list(dplyr::as_tibble(r$KitLines))
      tmp$Statuses <- paste(r$Statuses, collapse = "; ")
      tmp <- dplyr::rename(tmp, Sku = SKU)
      tmp
    }
  ) %>%
    dplyr::mutate_at(
      c("LastModifiedDateTimeUtc", "AvailableQuantityLastModifiedDateTimeUtc"),
      skuvaultr:::sv_parse_datetime
      )

  # include kit details (upon request)
  #  - {weight, brand}
  #   - to uppercase
  if (include_details) {
    kits <- sv_get_kit_details(kits)
    kits <- dplyr::mutate_at(kits, dplyr::vars(Sku, Code, PartNumber), toupper)
  } else {
    kits <- dplyr::mutate_at(kits, dplyr::vars(Sku, Code), toupper)
  }

  return(kits)
}


#' Get kit details
#' @param kits kit data from a call to \code{\link{sv_get_kits}}
#' @importFrom magrittr "%>%"
sv_get_kit_details <- function(kits) {

  # get SKUs contained within each kit
  kit_items <- kits %>%
    dplyr::select(Sku, KitLines) %>%
    tidyr::unnest(KitLines) %>%
    dplyr::mutate_at(c("Code", "SKU"), toupper)

  # get weights for base SKUs; sum weights over kit items
  kit_details <- kit_items %>%
    dplyr::left_join(
      sv_get_products(skus = unique(.$SKU)),
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
      PartNumber = paste(unique(PartNumber), collapse = "+"),
      Weight = sum(Quantity * weight),
      Brand = paste(unique(Brand), collapse = "; "),
      Supplier = paste(unique(Supplier), collapse = "; "),
      Classification = paste(unique(Classification), collapse = "; "),
      PkgQty = sum(Quantity),
      IncomingQuantity = min(floor(QuantityIncoming / Quantity))
    )

  # join with original 'kits' data
  dplyr::left_join(kits, kit_details, by = "Sku")
}