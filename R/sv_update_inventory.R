#' Update inventory (add/remove)
#' @param skus skus to add quantity
#' @param qty quantity to add (must be same length as `skus`, or length 1)
#' @param locs locations to add them too (must be same length as `skus`, or length 1)
#' @param type add/remove (scalar/character, must choose one)
#' @export
sv_update_inventory <- function(skus, qty, locs, type = c("add", "remove")) {

  # checks
  type <- match.arg(type)
  if (length(qty) != length(skus) & length(qty) != 1)
    stop("Input `qty` must be length 1 or the same length as `skus`!", call. = FALSE)
  products <- sv_get_products(skus = unique(skus))
  if (!all(skus %in% products$Sku))
    stop("Not all SKUs are valid!", call. = FALSE)

  # prep for API
  out <- dplyr::tibble(
    Sku = skus,
    Quantity = qty,
    WarehouseId = 2640,
    LocationCode = locs,
    Reason = stringr::str_to_title(type)
  )

  # submit to API (split if > 100 line items)
  ids <- 1:nrow(out)
  ids <- split(ids, ceiling(seq_along(ids)/100))
  ct <- 0
  for (i in ids) {
    ct <- ct + 1
    if (ct > 1)
      Sys.sleep(6)
    message(stringr::str_to_title(type), " inventory for ", length(i), " SKUs (", max(i), "/", max(unlist(ids)), ").")
    response <- sv_api_call(
      path = paste0("inventory/", type, "ItemBulk"),
      Items = out[i, ]
    )
  }

  return(invisible(out))
}