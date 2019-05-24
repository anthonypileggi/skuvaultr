
#' Auto-Pick an order
#' @param saleid SaleId of order to pick (character/scalar)
#' @param picklist prepared picklist (data.frame)
#' @importFrom magrittr "%>%"
#' @export
sv_pick_order <- function(saleid = NULL, picklist = NULL) {

  if (is.null(saleid) & is.null(picklist))
    stop("Must provide a `saleid` or a prepared `picklist`!")

  # load order picklist (if not provided)
  if (is.null(picklist)) {
    order <- sv_order_as_picklist(saleid)
    picklist <- order$picklist
    picklist$Id <- saleid

    # check order status
    if (order$status %in% c("Cancelled", "Completed"))
      stop("Order does not need to be picked!")
  }

  # check picklist format
  if (any(picklist$Quantity > picklist$Available))
    stop("Not enough inventory to pick this order!")
  if (!all(c("Id", "Sku", "Quantity", "LocationCode", "Available") %in% names(picklist)))
    stop("Picklist does not have required fields!")

  # prepare picklist
  picklist <- picklist %>%
    dplyr::transmute(
      SaleId = Id,
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

  # complete order(s) after picking
  Sys.sleep(5)
  sv_update_sale_status(unique(picklist$SaleId), "Complete")
}