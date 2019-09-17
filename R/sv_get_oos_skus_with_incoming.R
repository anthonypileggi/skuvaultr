
#' Find out-of-stock products with quantity on-order
#' @importFrom magrittr "%>%"
#' @export
sv_get_oos_skus_with_incoming <- function() {

  # identify skus
  p <- skuvaultr::sv_get_products()

  skus <- p %>%
    dplyr::filter(QuantityAvailable == 0, QuantityIncoming > 0) %>%
    dplyr::pull(Sku)

  # load PO history
  purchases <- skuvaultr::sv_get_purchase_orders()

  # find most recent PO for each sku and summarize
  purchases %>%
    tidyr::unnest(LineItems, names_repair = tidyr::tidyr_legacy) %>%
    dplyr::filter(toupper(SKU) %in% toupper(skus)) %>%
    dplyr::group_by(SKU) %>%
    dplyr::arrange(desc(CreatedDate)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(CreatedDate) %>%
    dplyr::select(
      PoNumber, Supplier = SupplierName, CreatedDate, Sku = SKU,
      Ordered = Quantity, Received = ReceivedQuantity, Notes = PublicNotes1
      )
}
