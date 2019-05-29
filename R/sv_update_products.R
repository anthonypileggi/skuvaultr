#' Update SkuVault product data based on an input data.frame
#' @param df data.frame containing columns SKU + columns to update
#' @export
sv_update_products <- function(df) {

  if ("Attributes" %in% names(df))
    df <- dplyr::mutate(df, Attributes = purrr::map(Attributes, jsonlite::unbox))

  ids <- 1:nrow(df)
  ids <- split(ids, ceiling(seq_along(ids)/100))
  ct <- 0
  for (i in ids) {
    ct <- ct + 1
    if (ct > 1)
      Sys.sleep(12)
    message("Updating data for ", length(i), " products (", max(i), "/", max(unlist(ids)), ").")
    sv_api_call(
      path = "products/updateProducts",
      Items = df[i, ]
    )
  }
}