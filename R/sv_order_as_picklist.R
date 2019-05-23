
#' Convert an order to a picklist
#' @param saleid SaleId of order to pick
#' @importFrom magrittr "%>%"
#' @export
sv_order_as_picklist <- function(saleid) {

  # pull up order information
  order <- skuvaultr::sv_get_sales(order_id = saleid)
  if (nrow(order) != 1)
    stop(paste("Order", saleid, "not found!"))

  # get order contents
  picklist <- NULL
  if (nrow(order$SaleKits[[1]]) > 0)
    picklist <- dplyr::bind_rows(picklist, purrr::map_df(order$SaleKits, ~.x$Items[[1]]))
  if (nrow(order$SaleItems[[1]]) > 0)
    picklist <- dplyr::bind_rows(picklist, purrr::map_df(order$SaleItems, ~dplyr::select(.x, Sku, Quantity)))

  # get item locations
  locs <- skuvaultr::sv_get_inventory_locations(skus = picklist$Sku) %>%
    dplyr::mutate_at("Sku", toupper) %>%
    dplyr::rename(Available = Quantity)
  picklist <- dplyr::left_join(picklist, locs, by = "Sku")

  return(list(status = order$Status, picklist = picklist))
}