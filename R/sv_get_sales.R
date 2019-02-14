#' Get sales
#' @param start_date first day of data (Date/scalar)
#' @param end_date last day of data (Date/scalar)
#' @importFrom magrittr "%>%"
#' @export
sv_get_sales <- function(start_date = Sys.Date() - 14, end_date = Sys.Date() - 1) {
  # generate date sequences in 7-day groups
  date_seq <- seq(start_date, end_date, by = "days")
  ids <- seq(1, length(date_seq), by = 7)
  dates <- dplyr::tibble(
    start = c(start_date, date_seq[tail(ids, -1)]),
    end = c(date_seq[ ids[-1] ] - 1, end_date)
    ) %>%
    dplyr::filter(start <= end)

  # call api for each 7-day date range
  purrr::map2_df(dates$start, dates$end, function(x, y) {
    Sys.sleep(2)
    sv_get_sales_7day(x, y)
  })
}

#' Get sales (<= 7 days only)
#' @param start_date first day of data (Date/scalar)
#' @param end_date last day of data (Date/scalar)
#' @importFrom magrittr "%>%"
sv_get_sales_7day <- function(start_date = Sys.Date() - 1, end_date = Sys.Date() - 1) {

  # impose API restrictions
  # TODO: auto-iterate to get all days of sale data
  if (difftime(end_date, start_date, units = "day") > 7)
    stop("Date range must be <= 7 days!", call. = FALSE)
  message("Collecting sales for [", start_date, "] - [", end_date, "]")

  # call API
  from_date <- paste0(start_date, "T05:00:00.0000000Z")     # time in UTC
  to_date <- paste0(end_date + 1, "T04:59:59.0000000Z")     # time in UTC
  x <- sv_api(path = "sales/getSalesByDate", PageSize = 10000, FromDate = from_date, ToDate = to_date)

  # clean data
  x %>%
    sv_parse_response() %>%
    dplyr::mutate_at(c("SaleDate"), sv_parse_datetime)
}