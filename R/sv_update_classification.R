#' Update Classification in Skuvault based on inventory contents
#' @importFrom magrittr "%>%"
#' @export
sync_sv_classifications <- function() {
  products <- skuvaultr::sv_get_product_inventory()
  out <- skuvaultr::sv_get_inventory_locations() %>%
    dplyr::filter(LocationCode == "DROP-SHIPS") %>%
    dplyr::group_by(Sku) %>%
    dplyr::summarize(
      in_house = sum(Quantity[LocationCode != "DROP-SHIPS"]),
      drop_ships = sum(Quantity[LocationCode == "DROP-SHIPS"])
    ) %>%
    dplyr::full_join(
      dplyr::select(products, Sku, Type, Classification),
      by = "Sku"
    ) %>%
    tidyr::replace_na(list(in_house = 0, drop_ships = 0)) %>%
    dplyr::mutate(
      new_class = dplyr::case_when(
        drop_ships > 0   ~ "Drop Ship",
        in_house > 0     ~ "General",
        TRUE             ~ Classification
      )
    ) %>%
    dplyr::filter(
      Type == "product",
      Classification != new_class
    ) %>%
    dplyr::select(Sku, Classification = new_class)
  if (nrow(out) > 0)
    skuvaultr::sv_update_products(out)
}