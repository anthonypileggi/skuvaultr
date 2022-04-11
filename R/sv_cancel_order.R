#' Cancel an order (and auto-unpick items)
#' @param saleid SaleId of order to pick
#' @importFrom magrittr "%>%"
#' @export
sv_cancel_order <- function(saleid) {

  # load order picklist
  order <- sv_order_as_picklist(saleid)
  picklist <- order$picklist

  # checks
  if (order$status == "Pending")
    stop("Order has not been picked!")
  if (order$status == "Cancelled")
    stop("Order is already cancelled!")
  if (any(is.na(picklist$LocationCode)))
    stop("Cannot unpick order due to out-of-stock")

  # load warehouse ids
  warehouses <- sv_get_warehouses() %>%
    dplyr::select(
      WarehouseCode = Code,
      WarehouseId = Id
    )

  # add inventory back into warehouse (if already picked)
  if (order$status %in% c("ReadyToShip", "Completed")) {
    out <- picklist %>%
      dplyr::left_join(warehouses, by = "WarehouseCode") %>%
      dplyr::mutate(
        WarehouseId = WarehouseId,
        Reason = "Cancelled"
      ) %>%
      dplyr::select(Sku, Quantity, WarehouseId, LocationCode, Reason)
    sv_api_call(
      path = "inventory/addItemBulk",
      Items = out
    )
  }

  # cancel order
  sv_update_sale_status(saleid, "Cancelled")
}