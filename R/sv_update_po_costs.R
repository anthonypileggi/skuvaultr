#' Update costs in a skuvault purchase order
#' @param data input data.frame that includes columns {PoNumber, Sku, Cost}
#' @importFrom magrittr "%>%"
#' @export
sv_update_po_costs <- function(data) {

  # check input data
  if (!all(names(data) %in% c("PoNumber", "Sku", "Cost")))
    stop("Input `data` should include columns {PoNumber, Sku, Cost}.")
  if (any(is.na(data)))
    stop("All values in `data` must be non-missing!")

  # extract PO number -- only can do 1 at a time (for now)
  po_number <- unique(data$PoNumber)
  if (length(po_number) > 1)
    stop("Can only update 1 PO at a time!")
  message(paste0("Updating PO # ", po_number, "..."))

  # load skuvault po
  purchase_order <- skuvaultr::sv_get_purchase_orders(PoNumbers = po_number) %>%
    dplyr::select(PoId, PoNumber, CreatedDate, LineItems) %>%
    tidyr::unnest(LineItems)

  # prepare input data
  data <- data %>%
    dplyr::select(
      SKU = Sku,
      new_cost = Cost
      )

  # merge skuvault/gardner invoices
  out <- purchase_order %>%
    dplyr::left_join(data, by = "SKU")


  # user messages/info
  message(paste("Updates not on PO ", po_number))
  print(dplyr::filter(data, !(SKU %in% out$SKU)))

  n_qty_zero <- sum(out$Quantity <= 0)
  if (n_qty_zero > 0)
    stop(paste("Updating PO costs will remove", n_qty_zero, "SKUs with Quantity=0"))

  n_updates <- sum(!is.na(out$new_cost) & abs(out$Cost - out$new_cost) >= .01)
  message(paste0("Updating PO costs for ", n_updates, "/", nrow(out), " SKUs"))


  # # update cost field  <--- will be deprecated!
  # # --> pull in COST field in skuvault product data (not PO)
  # products <- skuvaultr::sv_get_products(out$SKU) %>%
  #   dplyr::select(Sku, Cost)
  # updates <- out %>%
  #   dplyr::select(Sku = SKU, new_cost) %>%
  #   dplyr::left_join(products, by = "Sku") %>%
  #   dplyr::filter(
  #     !is.na(new_cost) & abs(new_cost - Cost) >= .01
  #     ) %>%
  #   dplyr::select(Sku, Cost = new_cost)
  # if (nrow(updates) > 0) {
  #   message((paste0("Updating product costs for ", nrow(updates), " SKUs...")))
  #   skuvaultr::sv_update_products(updates)
  # }


  # update PO in Skuvault
  # -- if no updates, still set PaymentStatus field
  out <- out %>%
    dplyr::transmute(
      PurchaseOrderId = PoId,
      PoNumber,
      PaymentStatus = "FullyPaid",
      SentStatus = "Sent",
      SKU,
      Quantity,          # keep qty in skuvalt
      QuantityTo3PL,
      Cost = dplyr::coalesce(new_cost, Cost),
      PrivateNotes,
      PublicNotes,       # should we log cost-updates?
      Variant,
      Identifier
    ) %>%
    dplyr::filter(
      PoNumber %in% po_number,
      Quantity > 0     # remove items received but not on PO (this will remove those line items!)
    ) %>%
    tidyr::nest(
      LineItems = c(SKU, Quantity, QuantityTo3PL, Cost, PrivateNotes, PublicNotes, Variant, Identifier)
    )

  # submit PO updates to skuvault
  message(paste0("Submitting updates for ", po_number, " to Skuvault..."))
  r <- skuvaultr::sv_api_call(
    path = "purchaseorders/updatePOs",
    POs = out
  )
  message(paste0("...", httr::content(r)$Status))


  return(invisible(out))
}