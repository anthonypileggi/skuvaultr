#' Parse SkuVault datetime
sv_parse_datetime <- function(x) {
  if (stringr::str_detect(x, "T")[1]) {
    x <- as.POSIXct(strptime(x, "%Y-%m-%dT%H:%M:%OSZ"), tz = "UTC")
  } else {
    x <- as.POSIXct(strptime(x, "%Y-%m-%d %H:%M:%SZ"), tz = "UTC")
  }
  lubridate::with_tz(x, tzone = Sys.timezone())
}