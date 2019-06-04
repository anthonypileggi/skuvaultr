#' Get order details (with kits unfolded)
#' @param start_date first day of data (Date/scalar)
#' @param end_date last day of data (Date/scalar)
#' @importFrom magrittr "%>%"
#' @export
sv_get_orders <- function(start_date = Sys.Date() - 1, end_date = Sys.Date()) {

  # get all stens dropship-eligble orders
  sales <- skuvaultr::sv_get_sales(start_date = start_date, end_date = end_date)

  # extract order components
  v <- c("FulfilledKits", "MerchantKits", "FulfilledItems", "MerchantItems", "SaleKits", "SaleItems")
  v <- intersect(v, names(sales))
  out <- purrr::map_df(v, ~tidyr::unnest(dplyr::select(sales, Id, .x)))

  # TODO: attach actual 'Order' contents (i.e., w/ kits)
  # TODO: return all fields, not just a select few...

  # unnest kits
  if ("Items" %in% names(out)) {
    out <- dplyr::mutate(out, is_kit = !purrr::map_lgl(Items, is.null))
    kits <- out %>%
      dplyr::filter(is_kit) %>%
      tidyr::unnest() %>%
      dplyr::select(Id, Sku = Sku1, Quantity = Quantity1, PartNumber = PartNumber1)
    parts <- out %>%
      dplyr::filter(!is_kit) %>%
      dplyr::select(Id, Sku, Quantity, PartNumber)
    out <- dplyr::bind_rows(kits, parts) %>%
      dplyr::arrange(Id)
  } else {
    out <- dplyr::select(out, Id, Sku, Quantity, PartNumber)
  }

  # attach item classifications
  out <- out %>%
    dplyr::mutate_at(c("Sku", "PartNumber"), toupper) %>%
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