#' Add drop-ship inventory to skuvault
#' @param skus skus to add quantity
#' @param qty quantity to add (must be same length as `skus`, or length 1)
#' @export
sv_add_dropship_inventory <- function(skus, qty) {
  
  # checks
  if (length(qty) != length(skus) & length(qty) != 1)
    stop("Input `qty` must be length 1 or the same length as `skus`!", call. = FALSE)
  
  # prep for API
  out <- dplyr::tibble(
    Sku = skus,
    Quantity = qty,
    WarehouseId = 2640,
    LocationCode = "DROP-SHIPS",
    Reason = "Add for Drop-Ships"
  )
  
  # TODO: if >100 calls, split them up
  if (nrow(out) > 100)
    stop("You can only submit 100 Skus at a time!", call. = FALSE)
  
  # submit to API
  sv_api_call(
    path = "inventory/addItemBulk",
    Items = out
  )
}