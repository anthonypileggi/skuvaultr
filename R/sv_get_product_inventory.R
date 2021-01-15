
#' Get product/kit cost and inventory levels
#' @importFrom magrittr "%>%"
#' @export
sv_get_product_inventory <- function() {

  # base products
  products <- sv_get_products() %>%
    dplyr::mutate(
      Weight = dplyr::case_when(
        WeightUnit == "lbs" ~ WeightValue,
        WeightUnit == "oz" ~ WeightValue / 16
        )
      )

  # kits
  kits <- sv_get_kits()
  kit_details <- kits %>%
    dplyr::select(Sku, KitLines) %>%
    tidyr::unnest(KitLines) %>%
    dplyr::group_by(Sku) %>%
    dplyr::mutate(n_skus = dplyr::n_distinct(SKU)) %>%
    dplyr::group_by(SKU) %>%
    dplyr::summarize(
      n_kits = dplyr::n(),
      kits = paste(Sku, collapse = ", "),
      is_simple = all(n_skus == 1) & all(Quantity == 1)
    ) %>%
    dplyr::rename(Sku = SKU)

  # combine product/kit data
  # -- for Assembled Products, only keep 'kit' rows
  dplyr::bind_rows(
    products %>%
      dplyr::left_join(kit_details, by = "Sku") %>%
      dplyr::select(
        Sku, PartNumber, Description, Classification, Brand, Supplier, Statuses,
        Cost, Price = SalePrice, MSRP = RetailPrice, Weight, QuantityAvailable, QuantityIncoming, n_kits, kits, is_simple
        ) %>%
      dplyr::mutate(Type = "product"),
    kits %>%
      dplyr::select(
        Sku, PartNumber, Description, Classification, Brand, Supplier, Statuses,
        Cost, Weight, QuantityAvailable = AvailableQuantity, QuantityIncoming = IncomingQuantity, PkgQty
        ) %>%
      dplyr::mutate(Type = "kit")
  ) %>%
    tidyr::replace_na(list(n_kits = 0, kits = "", PkgQty = 1, is_simple = FALSE, Price = 0)) %>%
    dplyr::group_by(Sku) %>%
    dplyr::arrange(Type) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
}