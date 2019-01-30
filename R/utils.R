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