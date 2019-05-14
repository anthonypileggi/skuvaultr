
#' Auto-Pick an order
#' @param saleid SaleId of order to pick
#' @importFrom magrittr "%>%"
#' @export
sv_pick_order <- function(saleid) {

  # load order picklist
  order <- sv_order_as_picklist(saleid)
  picklist <- order$picklist

  # checks
  if (order$status != c("Pending", "ReadyToShip"))
    stop("Order does not need to be picked!")
  if (any(picklist$Quantity > picklist$Available))
    stop("Not enough inventory to pick this order!")

  # prepare picklist
  picklist <- picklist %>%
    dplyr::transmute(
      SaleId = saleid,
      Sku = Sku,
      WarehouseId = 2640,    # WH1
      LocationCode = LocationCode,
      Quantity = Quantity,
      IsExpressPick = FALSE,
      Note = "Picked using API"
    )

  # pick order
  sv_api_call(
    path = "inventory/pickItemBulk",
    Items = picklist
  )

  # complete order after picking
  sv_update_sale_status(saleid, "Complete")
}