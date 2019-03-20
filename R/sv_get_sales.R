#' Get sales
#' @param start_date first day of data (Date/scalar)
#' @param end_date last day of data (Date/scalar)
#' @param order_id order/sale ids (character/vector)
#' @importFrom magrittr "%>%"
#' @export
sv_get_sales <- function(start_date = Sys.Date() - 14,
                         end_date = Sys.Date() - 1,
                         order_id = NULL) {

  # return a specific order
  if (!is.null(order_id))
    return(sv_get_sales_7day(order_id = order_id))

  dates <- split_date_range(start_date, end_date, n = 7)

  purrr::map_df(
    1:nrow(dates),
    function(i) {
      Sys.sleep(13)
      sv_get_sales_7day(dates$start[i], dates$end[i], ...)
    }
  )
}

#' Get sales (<= 7 days only)
#' @param start_date first day of data (Date/scalar)
#' @param end_date last day of data (Date/scalar)
#' @param order_id order/sale ids (character/vector)
#' @importFrom magrittr "%>%"
sv_get_sales_7day <- function(start_date = Sys.Date() - 1,
                              end_date = Sys.Date() - 1,
                              order_id = NULL) {

  # impose API restrictions
  # TODO: auto-iterate to get all days of sale data
  if (difftime(end_date, start_date, units = "day") > 7)
    stop("Date range must be <= 7 days!", call. = FALSE)
  message("Collecting sales for [", start_date, "] - [", end_date, "]")

  # call API
  if (!is.null(order_id)) {
    x <- sv_api_call(path = "sales/getSales", OrderIds = order_id)
    x <- httr::content(x)$Sales
  } else {
    from_date <- paste0(start_date, "T05:00:00.0000000Z")     # time in UTC
    to_date <- paste0(end_date + 1, "T04:59:59.0000000Z")     # time in UTC
    x <- sv_api(path = "sales/getSalesByDate", PageSize = 10000, FromDate = from_date, ToDate = to_date)
  }

  # clean data
  x %>%
    sv_parse_response() %>%
    dplyr::mutate_at(c("SaleDate"), sv_parse_datetime)
}