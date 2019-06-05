#' Get order details (with kits unfolded)
#' @param start_date first day of data (Date/scalar)
#' @param end_date last day of data (Date/scalar)
#' @importFrom magrittr "%>%"
#' @export
sv_get_orders <- function(start_date = Sys.Date() - 1, end_date = Sys.Date()) {

  # get all stens dropship-eligble orders
  sales <- sv_get_sales(start_date = start_date, end_date = end_date)

  # extract order components
  out <- sv_parse_order_contents(sales)

  # attach item classifications
  out <- out %>%
    tidyr::unnest() %>%
    dplyr::mutate_at("Sku", toupper) %>%
    dplyr::left_join(
      skuvaultr::sv_get_products(skus = unique(.$Sku)) %>%
        dplyr::select(Sku, Classification, Supplier, QuantityAvailable),
      by = "Sku"
    )

  # classify orders
  order_class <- out %>%
    dplyr::group_by(Id) %>%
    dplyr::summarize_at(
      c("Classification", "Supplier"),
      function(y) paste(unique(y), collapse = "; ")
    )

  # get contact/shipping details
  order_ship <- sales %>%
    dplyr::select(Id, Status, SaleDate, Marketplace, ShippingClass, Client, ShippingInfo, ContactInfo) %>%
    tidyr::unnest()

  # attach order class and additional details
  out <- out %>%
    tidyr::nest(-Id, .key = "Items") %>%
    dplyr::left_join(order_class, by = "Id") %>%
    dplyr::left_join(order_ship, by = "Id")

  return(out)
}