
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
  picklist <- sv_parse_order_contents(order)$Items[[1]]

  # get item locations
  locs <- skuvaultr::sv_get_inventory_locations(skus = picklist$Sku) %>%
    dplyr::rename(Available = Quantity)
  picklist <- dplyr::left_join(picklist, locs, by = "Sku")

  # check if order can be fulfilled
  tmp <- picklist %>%
    dplyr::group_by(Sku) %>%
    dplyr::summarize(
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
      LocationCode = ifelse(
        LocationCode %in% sv_dropship_locs(),
        paste0("ZZZ-", LocationCode),
        LocationCode
      )
    ) %>%
    dplyr::arrange(LocationCode) %>%    # dropship suppliers prioritized A-Z
    dplyr::mutate(
      Pick = purrr::map2_dbl(Quantity, Available, ~min(c(.x, .y))),
      Total = cumsum(Pick),
      Extra = ifelse(Total > Quantity, Total - Quantity, 0),
      Quantity = Pick - Extra,
      LocationCode = ifelse(
        stringr::str_detect(LocationCode, "ZZZ-"),
        stringr::str_replace(LocationCode, "ZZZ-", ""),
        LocationCode
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-Pick, -Total, -Extra) %>%
    dplyr::filter(Quantity > 0)

  return(list(status = order$Status, picklist = picklist))
}