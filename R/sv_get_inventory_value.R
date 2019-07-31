#' Get total warehouse inventory value
#' @param skus product skus (character/vector)
#' @importFrom magrittr "%>%"
#' @export
sv_get_inventory_value <- function(skus = NULL) {
  
  # Compute total inventory value (static, right now)
  products <- sv_get_products(skus)
  
  inventory <- sv_get_inventory_locations(skus)
  
  # unfurl additional skus
  alt_products <- products %>%
    dplyr::select(Sku, Cost, AlternateSku) %>%
    dplyr::filter(nchar(AlternateSku) > 0) %>%
    dplyr::mutate(
      Sku2 = purrr::map(AlternateSku, ~stringr::str_split(.x, "; ")[[1]])
    ) %>%
    tidyr::unnest() %>%
    dplyr::select(AltSku = Sku, Sku = Sku2, Cost)
  all_products <- products %>%
    dplyr::select(Sku, Description, Cost) %>%
    dplyr::bind_rows(alt_products)
  
  # Summarize inventory value (excluding dropships)
  inventory %>%
    dplyr::filter(LocationCode != "DROP-SHIPS") %>%
    dplyr::select(Sku, Quantity) %>%
    dplyr::left_join(all_products, by = "Sku") %>%
    tidyr::replace_na(list(Quantity = 0)) %>%
    dplyr::mutate(
      AltSku = dplyr::case_when(
        is.na(AltSku) ~ Sku,
        TRUE          ~ AltSku
      )
    ) %>%
    dplyr::select(-Sku) %>%
    dplyr::rename(Sku = AltSku) %>%
    dplyr::group_by(Sku) %>%
    dplyr::summarize(
      Quantity = sum(Quantity),
      Cost = mean(Cost)
    ) %>%
    dplyr::mutate(
      Value = Cost * Quantity
    ) %>%
    dplyr::arrange(desc(Value)) 
}