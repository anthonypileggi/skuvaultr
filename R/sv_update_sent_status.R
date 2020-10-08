#' Update costs in a skuvault purchase order
#' @param data input data.frame that includes columns {PoNumber, Sku, Cost}
#' @importFrom magrittr "%>%"
#' @export
sv_update_sent_status <- function(po_number, sent_status = c("NotSent", "Sent", "NeedToResend")) {
  
  sent_status <- match.arg(sent_status)
  
  # load skuvault po
  purchase_order <- skuvaultr::sv_get_purchase_orders(PoNumbers = po_number)
  
  # update PO in Skuvault
  # -- if no updates, still set PaymentStatus field
  out <- purchase_order %>%
    dplyr::transmute(
      PurchaseOrderId = PoId,
      PoNumber = PoNumber,
      SentStatus = sent_status
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