#' Get total warehouse inventory value
#' @param skus product skus (character/vector)
#' @importFrom magrittr "%>%"
#' @export
sv_get_inventory_value <- function(skus = NULL) {

  # Compute total inventory value (static, right now)

  # Load product data
  products <- sv_get_products(skus)

  # Unfurl additional skus
  alt_products <- products %>%
    dplyr::select(Sku, Cost, AlternateSku) %>%
    dplyr::filter(nchar(AlternateSku) > 0) %>%
    dplyr::mutate(
      Sku2 = purrr::map(AlternateSku, ~stringr::str_split(.x, "; ")[[1]])
    ) %>%
    tidyr::unnest()
  if (nrow(alt_products) > 0) {
    alt_products <- alt_products %>%
      dplyr::select(AltSku = Sku, Sku = Sku2, Cost)
    all_products <- products %>%
      dplyr::select(Sku, Description, Cost) %>%
      dplyr::bind_rows(alt_products)
  } else {
    all_products <- products %>%
      dplyr::select(Sku, Description, Cost) %>%
      dplyr::mutate(AltSku = NA_character_)
  }

  # Load inventory
  if (!is.null(skus)) {
    alt_skus <- all_products %>%
      dplyr::filter(AltSku %in% skus) %>%
      dplyr::pull(Sku)
    skus <- c(skus, alt_skus)
  }
  inventory <- sv_get_inventory_locations(skus)

  # Attach out-of-stocks SKUs
  if (nrow(inventory) == 0) {
     inventory <- products %>%
       dplyr::select(Sku) %>%
       dplyr::mutate(WarehouseCode = "WH1", LocationCode = "Unknown", Quantity = 0, Reserve = F)
  } else {
    inventory <- products %>%
      dplyr::select(Sku) %>%
      dplyr::full_join(
        dplyr::filter(inventory, LocationCode != "DROP-SHIPS"),
        by = "Sku") %>%
      tidyr::replace_na(list(WarehouseCode = "WH1", LocationCode = "Unknown", Quantity = 0, Reserve = F))
  }

  # Ignore dropship inventory
  #out <- dplyr::filter(inventory, LocationCode != "DROP-SHIPS")
  out <- dplyr::mutate(inventory, Quantity = ifelse(LocationCode == "DROP-SHIPS", 0, Quantity))

  # Replace AltSku with base SKU
  out <- out %>%
    dplyr::select(Sku, Quantity) %>%
    dplyr::left_join(all_products, by = "Sku") %>%
    dplyr::mutate(
      AltSku = dplyr::case_when(
        is.na(AltSku) ~ Sku,
        TRUE          ~ AltSku
      )
    ) %>%
    dplyr::select(-Sku) %>%
    dplyr::rename(sku = AltSku) %>%
    dplyr::group_by(sku) %>%
    dplyr::summarize(
      quantity = mean(Quantity),
      cost = mean(Cost)
    )

  # Summarize inventory value (excluding dropships)
  out <- out %>%
    dplyr::mutate(
      value = cost * quantity
    ) %>%
    dplyr::arrange(desc(value))

  # Attach location(s)
  out %>%
    dplyr::left_join(
      inventory %>%
        dplyr::rename(sku = Sku) %>%
        dplyr::group_by(sku) %>%
        tidyr::nest(.key = "location"),
      by = "sku"
    )
}