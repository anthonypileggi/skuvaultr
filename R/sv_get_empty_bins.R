
#' Find all inventory bins recently emptied
#' @param start_date first day of data (Date/scalar)
#' @param end_date last day of data (Date/scalar)
#' @export
sv_get_empty_bins <- function(start_date = Sys.Date() - 7,
                              end_date = Sys.Date()) {

  # TODO: update to use all history from BigQuery

  # load recent transactions
  #x <- sv_get_transactions(start_date = start_date, end_date = end_date)
  x <- bq_get_transactions_history(
    QuantityAfter == 0,
    TransactionType %in% c("Pick", "Remove"),
    TransactionReason != "Move",
    start_date = start_date,
    end_date = end_date
  )

  # find all transactions that led to an empty bin
  x %>%
    dplyr::group_by(Sku) %>%
    dplyr::arrange(desc(TransactionDate)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(Sku, Location, TransactionType, TransactionReason, TransactionDate) %>%
    dplyr::left_join(
      dplyr::select(sv_get_products(unique(.$Sku)), Sku, QuantityAvailable, Statuses),
      by = "Sku"
    ) %>%
    dplyr::mutate(
      Reason = paste(TransactionType, TransactionReason, sep = " - "),
      Location = stringr::str_replace(Location, "WH1--", "")
    ) %>%
    dplyr::filter(
      (is.na(QuantityAvailable) | QuantityAvailable == 0),
      !stringr::str_detect(Location, "DROP-SHIP|GENERAL"),
      (is.na(Statuses) | stringr::str_detect(Statuses, "NWS|NLA|Superseded|Inactive"))
    ) %>%
    dplyr::select(Sku, Location, Reason, Statuses) %>%
    dplyr::arrange(Location)
}