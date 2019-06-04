#' Create a new kit in Skuvault
#' @param x data.frame containing new kits (columns: Sku, Title, KitLines)
#' @export
sv_create_kits <- function(x) {

  # check contents of x
  if (!all(names(x) %in% c("Sku", "Title", "KitLines")))
    stop("Must provide a data.frame with columns: {Sku, Title, KitLines}")
  if(!all(names(x$KitLines[[1]]) %in% c("LineName", "Combine", "Quantity", "Items")))
    stop("Must provide a data.frame with KitLines containing: {LineName, Combine, Quantity, Items}")

  # submit to API
  n <- nrow(x)
  for (i in 1:n) {
    message("Creating ", x$Sku[i], " as a kit in SkuVault. (", i, "/", n, ")")
    if (i > 1)
      Sys.sleep(6)
    response <- sv_api_call(
      path = "products/createKit",
      Sku = x$Sku[i],
      Code = x$Sku[i],
      Title = x$Title[i],
      KitLines = x$KitLines[[i]]
    )
    # -- check for errors
    content <- httr::content(response)
    if (content$Status != "Success")
      stop(paste("Errors: ", paste(content$Errors, collapse = ";")), call. = FALSE)
  }
}