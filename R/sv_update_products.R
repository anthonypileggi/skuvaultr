#' Update SkuVault product data based on an input data.frame
#' @param df data.frame containing columns SKU + columns to update
#' @export
sv_update_products <- function(df) {
  ids <- 1:nrow(df)
  ids <- split(ids, ceiling(seq_along(ids)/100))
  for (i in ids) {
    message("Updating data for ", length(i), " products (", max(i), "/", max(unlist(ids)), ").")
    sv_api_call(
      path = "products/updateProducts",
      Items = df[i, ]
    )
    Sys.sleep(12)
  }
}