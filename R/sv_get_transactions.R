#' Get transactions
#' @param start_date first day of data (Date/scalar)
#' @param end_date last day of data (Date/scalar)
#' @export
sv_get_transactions <- function(start_date = Sys.Date() - 14,
                                end_date = Sys.Date() - 1,
                                sale_id = NULL) {

  dates <- split_date_range(start_date, end_date, n = 7)

  # return transactions for a specific order
  if (!is.null(sale_id))
    return(sv_get_transactions_7day(dates$start, dates$end, sale_id = sale_id))

  purrr::map2_df(dates$start, dates$end, function(x, y) {
    Sys.sleep(12)
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

  start_datetime <- as.POSIXct(paste(start_date, "00:00:00"))
  end_datetime <- as.POSIXct(paste(end_date, "23:59:59"))
  from_date <- format(lubridate::with_tz(start_datetime, tzone = "UTC"), "%Y-%m-%dT%T.0000000Z")
  to_date <- format(lubridate::with_tz(end_datetime, tzone = "UTC"), "%Y-%m-%dT%T.0000000Z")

  x <- sv_api(
    path = "inventory/getTransactions",
    PageSize = 10000,
    FromDate = from_date,
    ToDate = to_date,
    SaleId = sale_id
    )

  if (length(x) == 0) {
    out <- dplyr::tibble(
      User = character(), Sku = character(), Code = character(),
      ScannedCode = character(), LotNumber = character(), Title = character(),
      Quantity = integer(), QuantityBefore = integer(), QuantityAfter = integer(),
      Location = character(), TransactionType = character(),
      TransactionReason = character(), TransactionNote = character(),
      TransactionDate = Sys.time()[-1],
      ContextId = character()
    )
  } else {
    out <- x %>%
      sv_parse_response() %>%
      dplyr::mutate_at(c("TransactionDate"), sv_parse_datetime) %>%
      dplyr::mutate_at(c("Sku", "Code"), toupper)
  }

  return(out)
}