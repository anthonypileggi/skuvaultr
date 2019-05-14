
#' Update the status of a sale
#' @param saleid saleID of the order
#' @param status order status to set
#' @export
sv_update_sale_status <- function(saleid, status = c("Completed", "Cancelled", "Pending", "ReadyToShip", "Invalid", "ShippedUnpaid")) {
  
  status <- match.arg(status)
  
  skuvaultr::sv_api_call(
    path = "sales/updateOnlineSaleStatus",
    SaleId = saleid,
    Status = status
  )
}