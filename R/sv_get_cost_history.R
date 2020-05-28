
#' Construct cost history
#' @importFrom magrittr "%>%"
#' @export
sv_get_cost_history <- function() {

  purchases <- dplyr::bind_rows(
    skuvaultr::sv_get_purchase_orders(),
    skuvaultr::sv_get_purchase_orders(Status = "Completed")
  ) %>%
    dplyr::filter(Status != "Cancelled")

  df <- purchases %>%
    dplyr::select(PoNumber, CreatedDate, LineItems) %>%
    tidyr::unnest(cols = c(LineItems)) %>%
    #dplyr::filter(toupper(SKU) %in% toupper(skus)) %>%
    dplyr::select(Date = CreatedDate, Sku = SKU, Quantity, ReceivedQuantity, Cost) %>%
    dplyr::filter(Cost > 0) %>%
    dplyr::mutate(
      Date = lubridate::as_date(Date)
    ) %>%
    dplyr::arrange(Date) %>%
    tidyr::nest(history = c(Date, Quantity, ReceivedQuantity, Cost))

  p <- dplyr::progress_estimated(length(unique(df$Sku)))

  df %>%
    dplyr::mutate(
      cost_history = purrr::map(history,
        function(x) {
          p$tick()$print()
          #cat(s, "\n")
          x %>%
            dplyr::full_join(
              dplyr::tibble(Date = seq(min(x$Date), Sys.Date(), by = "day")),
              by = "Date"
            ) %>%
            dplyr::arrange(Date) %>%
            tidyr::fill(Cost, .direction = "down") %>%
            dplyr::select(Date, Cost)
        })
    )
}

