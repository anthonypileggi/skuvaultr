
#' Construct cost history
#' @param skus skus (character/vector)
#' @importFrom magrittr "%>%"
#' @export
sv_get_cost_history <- function(skus = "hon1010") {
  
  purchases <- dplyr::bind_rows(
    skuvaultr::sv_get_purchase_orders(),
    skuvaultr::sv_get_purchase_orders(Status = "Completed")
  ) %>%
    dplyr::filter(Status != "Cancelled")
  
  purchases %>%
    tidyr::unnest() %>%
    dplyr::filter(toupper(SKU) %in% toupper(skus)) %>%
    dplyr::select(OrderDate, SKU, Quantity, ReceivedQuantity, Cost) %>%
    dplyr::filter(Cost > 0) %>%
    dplyr::mutate(
      Date = lubridate::as_date(OrderDate)
    ) %>%
    dplyr::group_by(SKU) %>%
    tidyr::nest(.key = "history") %>%
    dplyr::mutate(
      cost_history = purrr::map(history,
        function(x) {
          x %>%
            dplyr::full_join(
              dplyr::tibble(Date = seq(min(x$Date), max(x$Date), by = "day")),
              by = "Date"
            ) %>%
            dplyr::arrange(Date) %>%
            tidyr::fill(Cost, .direction = "down") %>%
            dplyr::select(Date, Cost)
        })
    ) %>%
    dplyr::select(sku = SKU, cost_history)
}

