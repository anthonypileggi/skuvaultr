#' Get transactions
#' @param start_date first day of data (Date/scalar)
#' @param end_date last day of data (Date/scalar)
#' @export
sv_get_transactions <- function(start_date = Sys.Date() - 14,
                                end_date = Sys.Date() - 1,
                                sale_id = NULL) {

  # return transactions for a specific order
  if (!is.null(order_id))
    return(sv_get_transactions_7day(dates$start, dates$end, order_id = order_id))

  dates <- split_date_range(start_date, end_date, n = 7)

  purrr::map2_df(dates$start, dates$end, function(x, y) {
    Sys.sleep(2)
    sv_get_transactions_7day(x, y)
  })
}

#' Get transactions (<= 7 days only)
#' @param start_date first day of data (Date/scalar)
#' @param end_date last day of data (Date/scalar)
#' @param sale_id sale id (character/scalar) -- NOT WORKING
#' @importFrom magrittr "%>%"
sv_get_transactions_7day <- function(start_date = Sys.Date() - 1,
                                     end_date = Sys.Date() - 1,
                                     sale_id = NULL) {
  from_date <- paste0(start_date, "T05:00:00.0000000Z")     # time in UTC
  to_date <- paste0(end_date + 1, "T04:59:59.0000000Z")     # time in UTC
  x <- sv_api(
    path = "inventory/getTransactions",
    PageSize = 10000,
    FromDate = from_date,
    ToDate = to_date,
    SaleId = sale_id
    )
  x %>%
    sv_parse_response() %>%
    dplyr::mutate_at(c("TransactionDate"), sv_parse_datetime)
}