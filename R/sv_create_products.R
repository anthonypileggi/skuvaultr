#' Create new products in Skuvault
#' @param df data.frame containing new products (columns: Sku, Description)
#' @export
sv_create_products <- function(df) {

  # check contents of df
  req_cols <- c("Sku", "Classification", "Supplier", "Brand")
  req_cols2 <- c("SupplierName", "IsPrimary")
  if (!all(req_cols %in% names(df)))
    stop(paste0("Must provide a data.frame with columns: {", paste(req_cols, collapse = ", "), "}"))
  if ("SupplierInfo" %in% names(df)) {
    if (!all(req_cols2 %in% names(df$SupplierInfo[[1]])))
      stop(paste0("SupplierInfo must include columns: {", paste(req_cols2, collapse = ", "), "}"))
  }
  if ("Attributes" %in% names(df))
    df <- dplyr::mutate(df, Attributes = purrr::map(Attributes, jsonlite::unbox))

  # submit to API
  ids <- 1:nrow(df)
  ids <- split(ids, ceiling(seq_along(ids)/100))
  ct <- 0
  for (i in ids) {
    ct <- ct + 1
    if (ct > 1)
      Sys.sleep(12)
    message("Creating ", length(i), " new products (", max(i), "/", max(unlist(ids)), ").")
    response <- sv_api_call(
      path = "products/createProducts",
      Items = df[i, ]
    )
    # -- check for errors
    content <- httr::content(response)
    if (!(content$Status %in% c("OK", "Success")))
      stop(paste("Errors: ", paste(content$Errors, collapse = ";")), call. = FALSE)
  }

}