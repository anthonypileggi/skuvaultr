
#' Find all inventory bins recently emptied
#' @param start_date first day of data (Date/scalar)
#' @param end_date last day of data (Date/scalar)
#' @export
sv_get_empty_bins <- function(start_date = Sys.Date() - 7,
                              end_date = Sys.Date() - 1) {
  
  # load recent transactions
  x <- sv_get_transactions(start_date = start_date, end_date = end_date)
  
  # find all transactions that led to an empty bin
  x %>%
    dplyr::filter(
      QuantityAfter == 0,
      TransactionType %in% c("Remove", "Pick")
    ) %>%
    dplyr::group_by(Sku, Location) %>%
    dplyr::arrange(desc(TransactionDate)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(Sku, Title, Location) %>%
    dplyr::inner_join(
      dplyr::select(sv_get_products(unique(.$Sku)), Sku, Statuses),
      by = "Sku"
    ) %>%
    dplyr::filter(
      stringr::str_detect(Statuses, "NLA|Superseded|Inactive"),
      !stringr::str_detect(Location, "DROP-SHIPS|GENERAL")
    ) %>%
    dplyr::mutate(
      Location = stringr::str_replace(Location, "WH1--", "")
    ) %>%
    dplyr::arrange(Location)
}