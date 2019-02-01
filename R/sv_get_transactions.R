#' Get transactions
#' @param start_date first day of data (Date/scalar)
#' @param end_date last day of data (Date/scalar)
#' @importFrom magrittr "%>%"
#' @export
sv_get_transactions <- function(start_date = Sys.Date() - 1, end_date = Sys.Date() - 1) {
  from_date <- paste0(start_date, "T05:00:00.0000000Z")     # time in UTC
  to_date <- paste0(end_date + 1, "T04:59:59.0000000Z")     # time in UTC
  x <- sv_api(path = "inventory/getTransactions", PageSize = 10000, FromDate = from_date, ToDate = to_date)
  x %>%
    sv_parse_response() %>%
    dplyr::mutate_at(c("TransactionDate"), sv_parse_datetime)
}