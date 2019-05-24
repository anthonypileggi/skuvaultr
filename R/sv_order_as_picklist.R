
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

  # check if order can be fulfilled
  tmp <- picklist %>%
    group_by(Sku) %>%
    summarize(
      Quantity = head(Quantity, 1),
      Available = sum(Available)
      )
  if (any(tmp$Quantity > tmp$Available))
    stop("Not enough inventory to pick this order!")

  # account for items w/ multiple locations!
  # --> empty drop-ship bins as last resort
  # --> prioritized by bin location (i.e., alphabet)
  # --> ignore extra locations we will not be picking from
  picklist <- picklist %>%
    dplyr::group_by(Sku) %>%
    dplyr::mutate(
      LocationCode = ifelse(LocationCode == "DROP-SHIPS", "ZZZ-DROP-SHIPS", LocationCode)
    ) %>%
    dplyr::arrange(LocationCode) %>%
    dplyr::mutate(
      Pick = purrr::map2_dbl(Quantity, Available, ~min(c(.x, .y))),
      Total = cumsum(Pick),
      Extra = ifelse(Total > Quantity, Total - Quantity, 0),
      Quantity = Pick - Extra,
      LocationCode = ifelse(LocationCode == "ZZZ-DROP-SHIPS", "DROP-SHIPS", LocationCode)
    ) %>%
    dplyr::select(-Pick, -Total, -Extra) %>%
    dplyr::filter(Quantity > 0)

  return(list(status = order$Status, picklist = picklist))
}