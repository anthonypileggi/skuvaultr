
#' Update the status of a sale
#' @param saleid saleID of the order (character/vector)
#' @param status order status to set (character/scalar)
#' @export
sv_update_sale_status <- function(saleid,
                                  status = c("Completed", "Cancelled", "Pending", "ReadyToShip", "Invalid", "ShippedUnpaid")) {

  status <- match.arg(status)

  purrr::map(
    seq_along(saleid),
    function(i) {
      if (i > 1)
        Sys.sleep(4)
      skuvaultr::sv_api_call(
        path = "sales/updateOnlineSaleStatus",
        SaleId = saleid[i],
        Status = status
      )
      message(paste0("Updated status for SaleId ", saleid[i], " to '", status, "'."))
    }
  )

}