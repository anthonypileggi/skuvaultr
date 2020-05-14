#' Get order details (with kits unfolded)
#' @param start_date first day of data (Date/scalar)
#' @param end_date last day of data (Date/scalar)
#' @param order_id saleids (character/vector)
#' @importFrom magrittr "%>%"
#' @export
sv_get_orders <- function(start_date = Sys.Date() - 1, end_date = Sys.Date(), order_id = NULL) {

  # load orders w/in date range
  sales <- sv_get_sales(start_date = start_date, end_date = end_date, order_id = order_id)

  # attach order details (date/marketplace/recipient/shipping-details)
  out <- sales %>%
    dplyr::select(-Channel) %>%
    dplyr::left_join(
      sv_parse_channel(sales),
      by = "Id"
    ) %>%
    dplyr::select(Id, SaleDate, Channel, ShippingClass, Printed = PrintedStatus, ShippingInfo, ContactInfo)

  # extract order components (unnest kits)
  out <- out %>%
    dplyr::left_join(
      sv_parse_order_contents(sales),
      by = "Id"
    )

  # get picklist and fulfillment type
  out <- out %>%
    dplyr::left_join(
      sv_get_picklists(data = sales),
      by = "Id"
    ) %>%
    dplyr::rename(Fulfill = fulfill, Picklist = picklist)

  # sale summaries
  out <- out %>%
    dplyr::left_join(
      sv_parse_sale_contents(sales) %>%
        dplyr::select(-Items),
      by = "Id"
    )

  # attach product supplier/cost/classification
  skus <- unique(unlist(purrr::map(out$Items, ~.x$Sku)))
  products <- skuvaultr::sv_get_products(skus = skus) %>%
    dplyr::select(Sku, Classification, Supplier, Cost, QuantityAvailable)
  out <- out %>%
    dplyr::mutate(
      Items = purrr::map(Items, ~dplyr::left_join(.x, products, by = "Sku")),
      Classification = purrr::map_chr(Items, ~paste(unique(.x$Classification), collapse = "; ")),
      Supplier = purrr::map_chr(Items, ~paste(unique(.x$Supplier), collapse = "; ")),
      Cost = purrr::map_dbl(Items, ~sum(.x$Cost * .x$Quantity))
    )

  return(out)
}