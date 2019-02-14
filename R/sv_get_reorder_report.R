
#' Create a product reorder report
#' @param n_days Number of days in-stock to order
#' @importFrom magrittr "%>%"
#' @export
sv_get_reorder_report <- function(n_days = 30) {
  
  # current stock
  products <- dplyr::filter(sv_get_products(), !IsAlternateSKU, !IsAlternateCode)
  kits <- sv_get_kits()
  
  # recent sales data
  sales <- sv_get_sales_details(start_date = Sys.Date() - 91, end_date = Sys.Date() - 1)
  
  # get quantity sold for each sku over various date-ranges
  # - base skus
  product_df <- sales %>%
    dplyr::inner_join(
      dplyr::select(products, Sku), by = "Sku"
    ) %>%
    dplyr::group_by(Sku) %>%
    dplyr::summarize(
      qty_7 = sum(Quantity[SaleDate >= Sys.Date() - 7]),
      qty_28 = sum(Quantity[SaleDate >= Sys.Date() - 28]),
      qty_63 = sum(Quantity[SaleDate >= Sys.Date() - 63]),
      qty_91 = sum(Quantity[SaleDate >= Sys.Date() - 91])
    )
  # - kit skus (string out kits to get qty counts for base skus)
  kit_df <- sales %>%
    dplyr::inner_join(
      dplyr::select(kits, Sku, KitLines),
      by = "Sku"
    ) %>%
    tidyr::unnest() %>%
    dplyr::mutate(Quantity = Quantity * Quantity1) %>%
    dplyr::group_by(SKU) %>%
    dplyr::summarize(
      qty_7 = sum(Quantity[SaleDate >= Sys.Date() - 7]),
      qty_28 = sum(Quantity[SaleDate >= Sys.Date() - 28]),
      qty_63 = sum(Quantity[SaleDate >= Sys.Date() - 63]),
      qty_91 = sum(Quantity[SaleDate >= Sys.Date() - 91])
    ) %>%
    dplyr::rename(Sku = SKU)
  # - combine base/kit skus
  df <- dplyr::bind_rows(product_df, kit_df) %>%
    dplyr::group_by(Sku) %>%
    dplyr::summarize_if(is.numeric, sum)
  
  # compute days_left and suggested_qty
  # -- based on sales in past 28 days
  products %>%
    dplyr::filter(
      !stringr::str_detect(Statuses, "NLA|Inactive")
    ) %>%
    dplyr::select(Sku, Code, Description, Brand, Supplier, Cost, QuantityAvailable, QuantityIncoming, QuantityTotalFBA) %>%
    dplyr::left_join(df, by = "Sku") %>%
    tidyr::replace_na(
      list(qty_7 = 0, qty_28 = 0, qty_63 = 0, qty_91 = 0)
    ) %>%
    dplyr::mutate(
      days_left = ifelse(qty_28 == 0, Inf, QuantityAvailable / (qty_28 / 28)),
      suggested_qty = (qty_28 / 28) * n_days - QuantityAvailable - QuantityIncoming,
      suggested_qty = ifelse(suggested_qty < 0, 0, ceiling(suggested_qty))
    ) %>%
    dplyr::arrange(desc(suggested_qty))
}