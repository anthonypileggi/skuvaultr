
#' Create a product reorder report
#' @param n_days Number of days in-stock to order
#' @importFrom magrittr "%>%"
#' @export
sv_get_reorder_report <- function(n_days = 30, include_last_season = TRUE) {

  # current stock
  products <- sv_get_products()
  kits <- sv_get_kits()

  # recent sales data
  start_date <- Sys.Date() - 119
  end_date <- Sys.Date() - 1
  last_year <- lubridate::year(end_date) - 1
  season_start_date <- lubridate::as_date(paste0(last_year, "-02-20"))
  season_end_date <- lubridate::as_date(paste0(last_year, "-07-31"))
  days_LS <- as.numeric(difftime(season_end_date, season_start_date, units = "day")) + 1
  if (include_last_season) {
    start_date <- season_start_date
  }
  sales <- sv_get_sales_details(start_date = start_date, end_date = end_date)
  #sales <- bq_get_sales_history(start_date = start_date, end_date = end_date)

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
      qty_91 = sum(Quantity[SaleDate >= Sys.Date() - 91]),
      qty_119 = sum(Quantity[SaleDate >= Sys.Date() - 119]),
      qty_LS = sum(Quantity[SaleDate >= season_start_date & SaleDate <= season_end_date])
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
      qty_91 = sum(Quantity[SaleDate >= Sys.Date() - 91]),
      qty_119 = sum(Quantity[SaleDate >= Sys.Date() - 119]),
      qty_LS = sum(Quantity[SaleDate >= season_start_date & SaleDate <= season_end_date])
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
      !stringr::str_detect(Statuses, "NLA|Inactive|Superseded")
    ) %>%
    dplyr::mutate(
      Created = lubridate::date(CreatedDateUtc)
    ) %>%
    dplyr::select(Sku, Code, Description, Brand, Supplier, Created, Cost, QuantityAvailable, QuantityIncoming, QuantityTotalFBA) %>%
    dplyr::left_join(df, by = "Sku") %>%
    tidyr::replace_na(
      list(qty_7 = 0, qty_28 = 0, qty_63 = 0, qty_91 = 0, qty_119 = 0, qty_LS = 0)
    ) %>%
    dplyr::mutate(
      days_left = ifelse(qty_28 == 0, Inf, QuantityAvailable / (qty_28 / 28)),
      days_left = round(days_left, 1),
      suggested_qty = (qty_28 / 28) * n_days - QuantityAvailable - QuantityIncoming,
      suggested_qty = ifelse(suggested_qty < 0, 0, ceiling(suggested_qty)),
      suggested_qty_LS = (qty_LS / days_LS) * n_days - QuantityAvailable - QuantityIncoming,
      suggested_qty_LS = ifelse(suggested_qty_LS < 0, 0, ceiling(suggested_qty_LS))
    ) %>%
    dplyr::arrange(desc(suggested_qty))
}