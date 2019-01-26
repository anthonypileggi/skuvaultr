#' Get sales
#' @param start_date first day of data (Date/scalar)
#' @param end_date last day of data (Date/scalar)
#' @importFrom magrittr "%>%"
#' @export
sv_get_sales <- function(start_date = Sys.Date() - 1, end_date = Sys.Date() - 1) {

  # impose API restrictions
  # TODO: auto-iterate to get all days of sale data
  if (difftime(end_date, start_date, units = "day") > 7)
    stop("Date range must be <= 7 days!", call. = FALSE)

  # Iterate to get all sales
  go <- TRUE
  pg <- -1
  pg_size <- 10000
  from_date <- paste0(start_date, "T05:00:00.0000000Z")     # time in UTC
  to_date <- paste0(end_date + 1, "T04:59:59.0000000Z")     # time in UTC
  x <- list()
  while (go) {
    pg <- pg + 1
    message("Getting page ", pg + 1, " of sales data.")
    r <- sv_api("sales/getSalesByDate", PageSize = pg_size, PageNumber = pg, FromDate = from_date, ToDate = to_date)
    new_x <- httr::content(r)
    x <- c(x, new_x)
    if (length(new_x) < pg_size)
      go <- FALSE
  }

  # Clean data
  x %>%
    sv_parse_response() %>%
    dplyr::mutate_at(c("SaleDate"), sv_parse_datetime)
}