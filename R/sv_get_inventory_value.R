#' Get total warehouse inventory value
#' @param skus product skus (character/vector)
#' @importFrom magrittr "%>%"
#' @export
sv_get_inventory_value <- function(skus = NULL) {

  # Compute total inventory value (static, right now)

  # LOCations to ignore for inventory calcs
  ignore_locs <- c(skuvaultr::sv_dropship_locs(), "WHOLEGOODS")

  # Load product data
  products <- sv_get_products(skus)

  # Unfurl additional skus
  alt_products <- products %>%
    dplyr::select(Sku, Cost, AlternateSku) %>%
    dplyr::filter(nchar(AlternateSku) > 0) %>%
    dplyr::mutate(
      Sku2 = purrr::map(AlternateSku, ~stringr::str_split(.x, "; ")[[1]])
    ) %>%
    tidyr::unnest(Sku2)
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
        dplyr::filter(inventory, !(LocationCode %in% ignore_locs)),
        by = "Sku") %>%
      tidyr::replace_na(list(WarehouseCode = "WH1", LocationCode = "Unknown", Quantity = 0, Reserve = F))
  }

  # Ignore dropship inventory
  out <- inventory %>%
    dplyr::mutate(
      Quantity = ifelse(LocationCode %in% ignore_locs, 0, Quantity)
    )

  # Replace AltSku with base SKU
  out <- out %>%
    dplyr::left_join(all_products, by = "Sku") %>%
    dplyr::mutate(
      AltSku = dplyr::case_when(
        is.na(AltSku) ~ Sku,
        TRUE          ~ AltSku
      )
    ) %>%
    #dplyr::filter(AltSku %in% c("692187", "731-09312"))  %>%
    dplyr::select(-Sku) %>%
    dplyr::rename(sku = AltSku) %>%
    dplyr::group_by(sku, LocationCode) %>%
    dplyr::summarize_at(dplyr::vars(Quantity, Cost), mean, na.rm = T) %>%
    dplyr::summarize(
      quantity = sum(Quantity, na.rm = T),
      cost = weighted.mean(Cost, Quantity, na.rm = T)
    )

  # Summarize inventory value (excluding dropships)
  out <- out %>%
    dplyr::mutate(
      value = ifelse(quantity == 0, 0, cost * quantity)
    ) %>%
    dplyr::arrange(desc(value))

  # Attach location(s)
  out %>%
    dplyr::left_join(
      inventory %>%
        dplyr::rename(sku = Sku) %>%
        dplyr::group_by(sku) %>%
        tidyr::nest(location = c(WarehouseCode, LocationCode, Quantity, Reserve)) %>%
        dplyr::mutate(
          location = purrr::map(location, ~.x)
        ),
      by = "sku"
    )
}