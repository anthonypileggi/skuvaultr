#' Estimate the ship date based on the order date
#' @param order_date datetime of order (POSIXct/vector)
#' @note Factors in 2pm cutoff and no shipping on weekends
#' @export
estimate_ship_date <- function(order_date) {
  dplyr::case_when(
    lubridate::wday(order_date) == 6 & lubridate::hour(order_date) >= 14 ~ lubridate::date(order_date) + 3,    # friday afternoon
    lubridate::wday(order_date) == 7 ~ lubridate::date(order_date) + 2,                                        # saturday
    lubridate::wday(order_date) == 1 ~ lubridate::date(order_date) + 1,                                        # sunday
    lubridate::hour(order_date) < 14 ~ lubridate::date(order_date),
    lubridate::hour(order_date) >= 14 ~ lubridate::date(order_date) + 1
  )
}


#' Divide a date-range into subsequences of a given size
#' @param start_date First day of date range (Date/scalar)
#' @param end_date Last day of date range (Date/scalar)
#' @export
split_date_range <- function(start_date, end_date, n = 7) {
  date_seq <- seq(start_date, end_date, by = "days")
  ids <- seq(1, length(date_seq), by = 7)
  dates <- dplyr::tibble(
    start = c(start_date, date_seq[tail(ids, -1)]),
    end = c(date_seq[ ids[-1] ] - 1, end_date)
  )
  dplyr::filter(dates, start <= end)
}