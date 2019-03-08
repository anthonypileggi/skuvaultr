
#' Get product/kit cost and inventory levels
#' @importFrom magrittr "%>%"
#' @export
sv_get_product_inventory <- function() {

  products <- sv_get_products() %>%
    mutate(Weight = dplyr::case_when(WeightUnit == "lbs" ~ WeightValue, WeightUnit == "oz" ~ WeightValue / 16))
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
      dplyr::select(Sku, Description, Classification, Statuses, Cost, Weight, QuantityAvailable, n_kits, kits) %>%
      tidyr::replace_na(list(n_kits = 0, kits = "")) %>%
      dplyr::mutate(Type = "product"),
    kits %>%
      dplyr::select(Sku, Description, Statuses, Cost, Weight, QuantityAvailable = AvailableQuantity) %>%
      dplyr::mutate(Type = "kit")
  )
}