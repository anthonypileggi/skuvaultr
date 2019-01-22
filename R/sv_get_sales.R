#' Get sales
#' @param start_date first day of data (Date/scalar)
#' @param end_date last day of data (Date/scalar)
#' @export
sv_get_sales <- function(start_date = Sys.Date() - 1, end_date = Sys.Date() - 1) {

  if (nchar(Sys.getenv("SKU_VAULT_TENANT_TOKEN")) == 0 | nchar(Sys.getenv("SKU_VAULT_USER_TOKEN")) == 0)
    sv_get_tokens()

  my_json <-
    list(
      TenantToken = Sys.getenv("SKU_VAULT_TENANT_TOKEN"),
      UserToken = Sys.getenv("SKU_VAULT_USER_TOKEN"),
      PageSize = 10000,
      PageNumber = -1,
      FromDate = paste0(start_date, "T05:00:00.0000000Z"),     # times in UTC
      ToDate = paste0(end_date + 1, "T04:59:59.0000000Z")
    )

  # Iterate to get all sales for the requested period
  go <- TRUE
  sales <- list()
  while (go) {
    my_json$PageNumber <- my_json$PageNumber + 1
    message("Getting page ", my_json$PageNumber + 1, " of sales data.")
    r <- httr::POST("https://app.skuvault.com/api/sales/getSalesByDate", body = my_json, encode = "json")
    new_sales <- httr::content(r)
    sales <- c(sales, new_sales)
    if (length(new_sales) < my_json$PageSize)
      go <- FALSE
  }

  # Clean data
  # clean-up data
  dplyr::mutate(
    sv_parse_response(sales),
    SaleDate = as.POSIXct(strptime(SaleDate, "%Y-%m-%dT%H:%M:%OSZ"), tz = "UTC"),
    SaleDate = lubridate::with_tz(SaleDate, tzone = Sys.timezone())
  )
}