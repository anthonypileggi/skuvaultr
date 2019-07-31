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
    return(sv_get_sales_7day(order_id = order_id, start_date = Sys.Date() - 1000, end_date = Sys.Date()))

  dates <- split_date_range(start_date, end_date, n = 7)

  purrr::map_df(
    1:nrow(dates),
    function(i) {
      Sys.sleep(13)
      sv_get_sales_7day(dates$start[i], dates$end[i])
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
  if (difftime(end_date, start_date, units = "day") > 7 & is.null(order_id))
    stop("Date range must be <= 7 days!", call. = FALSE)
  message("Collecting sales for [", start_date, "] - [", end_date, "]")

  # call API
  if (!is.null(order_id)) {
    x <- sv_api_call(path = "sales/getSales", OrderIds = order_id)
    x <- httr::content(x)$Sales
  } else {
    start_datetime <- as.POSIXct(paste(start_date, "00:00:00"))
    end_datetime <- as.POSIXct(paste(end_date, "23:59:59"))
    from_date <- format(lubridate::with_tz(start_datetime, tzone = "UTC"), "%Y-%m-%dT%T.0000000Z")
    to_date <- format(lubridate::with_tz(end_datetime, tzone = "UTC"), "%Y-%m-%dT%T.0000000Z")
    x <- sv_api(path = "sales/getSalesByDate", PageSize = 10000, FromDate = from_date, ToDate = to_date)
  }

  # clean data
  x %>%
    sv_parse_response() %>%
    dplyr::mutate_at(c("SaleDate"), sv_parse_datetime)
}