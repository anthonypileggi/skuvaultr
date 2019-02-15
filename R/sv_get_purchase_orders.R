#' Get purchase orders
#' @importFrom magrittr "%>%"
#' @export
sv_get_purchase_orders <- function(...) {
  x <- sv_api(path = "purchaseorders/getPOs", ...)
  x$PurchaseOrders %>%
    sv_parse_response() %>%
    dplyr::mutate_at(
      c("CreatedDate", "OrderDate", "OrderCancelDate", "ArrivalDueDate", "RequestedShipDate", "ActualShippedDate"),
      sv_parse_datetime
      )
}