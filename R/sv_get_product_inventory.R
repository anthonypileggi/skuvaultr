
#' Get product/kit cost and inventory levels
#' @importFrom magrittr "%>%"
#' @export
sv_get_product_inventory <- function() {

  products <- dplyr::filter(sv_get_products(), !IsAlternateSKU, !IsAlternateCode)
  kits <- sv_get_kits()
  kit_details <- kits %>%
    dplyr::select(Sku, KitLines) %>%
    tidyr::unnest() %>%
    dplyr::group_by(SKU) %>%
    dplyr::summarize(
      n_kits = dplyr::n(),
      kits = paste(Sku, collapse = ", ")
    ) %>%
    dplyr::rename(Sku = SKU)

  dplyr::bind_rows(
    products %>%
      dplyr::left_join(kit_details, by = "Sku") %>%
      dplyr::select(Sku, Description, Classification, Cost, QuantityAvailable, n_kits, kits) %>%
      tidyr::replace_na(list(n_kits = 0, kits = "")) %>%
      dplyr::mutate(Type = "product"),
    kits %>%
      dplyr::select(Sku, Description, Cost, QuantityAvailable = AvailableQuantity) %>%
      dplyr::mutate(Type = "kit")
  )
}