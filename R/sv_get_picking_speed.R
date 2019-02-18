#' Get picking speed (overall; user-specific)
#' @param start_date first day of data (Date/scalar)
#' @param end_date last day of data (Date/scalar)
#' @importFrom magrittr "%>%"
#' @export
sv_get_picking_speed <- function(start_date = Sys.Date(), end_date = Sys.Date()) {
  
  x <- sv_get_transactions(start_date = start_date, end_date = end_date) %>%
    dplyr::filter(
      TransactionType == "Pick",
      Location != "WH1--DROP-SHIPS"
      ) %>%
    dplyr::arrange(TransactionDate)
  
  # overall
  out1 <- x %>%
    dplyr::mutate(
      diff = as.numeric(difftime(TransactionDate, lag(TransactionDate), units = "secs")),
      diff = ifelse(diff > 60*30, NA, diff)
    ) %>%
    dplyr::summarize(
      User = "Total",
      picked = sum(Quantity),
      avg = mean(diff, na.rm = TRUE),
      recent = mean(tail(diff, 20), na.rm = TRUE)
    ) 
  
  # users
  out2 <- x %>%
    dplyr::group_by(User) %>%
    dplyr::mutate(
      diff = as.numeric(difftime(TransactionDate, lag(TransactionDate), units = "secs")),
      diff = ifelse(diff > 60*30, NA, diff)
    ) %>%
    dplyr::summarize(
      picked = sum(Quantity),
      avg = mean(diff, na.rm = TRUE),
      recent = mean(tail(diff, 20), na.rm = TRUE)
    ) 
    
  dplyr::bind_rows(out2, out1)
}