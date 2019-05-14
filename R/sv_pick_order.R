
#' Auto-Pick an order
#' @param saleid SaleId of order to pick
#' @importFrom magrittr "%>%"
#' @export
sv_pick_order <- function(saleid) {
  
  # pull up order information
  order <- sv_get_sales(order_id = saleid)
  if (nrow(order) != 1)
    stop(paste("Order", saleid, "not found!"))
  
  # get order contents
  picklist <- dplyr::bind_rows(
    purrr::map_df(order$SaleKits, ~.x$Items),
    purrr::map_df(order$SaleItems, ~dplyr::select(.x, Sku, Quantity))
  )
  
  # get item locations
  locs <- skuvaultr::sv_get_inventory_locations(skus = picklist$Sku) %>%
    dplyr::mutate_at("Sku", toupper) %>%
    dplyr::rename(Available = Quantity)
  picklist <- dplyr::left_join(picklist, locs, by = "Sku")
  
  # check inventory levels
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