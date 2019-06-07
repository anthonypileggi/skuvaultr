#' Get products
#' @export
sv_get_products <- function(skus = NULL) {

  # submit unique skus only
  if (!is.null(skus))
    skus <- unique(toupper(skus))

  # Iterate to get a full list of products
  products <- sv_api(path = "products/getProducts", PageSize = 10000, ProductSKUs = skus)

  # exit early if there is no data
  if (length(products) == 0) {
    message("No products matching the requested SKUs were found.")
    return(NULL)
  }

  # parse response
  #  - create columns for alternate code/sku (and remove corresponding rows)
  products %>%
    sv_parse_response() %>%
    dplyr::mutate_at(c("CreatedDateUtc", "ModifiedDateUtc"), sv_parse_datetime) %>%
    dplyr::mutate_at("WeightValue", as.numeric) %>%
    dplyr::mutate_at(c("Sku", "Code", "PartNumber"), toupper) %>%
    dplyr::group_by(Id) %>%
    dplyr::mutate(
      AlternateCode = paste(Code[IsAlternateCode], collapse = "; "),
      AlternateSku = paste(Sku[IsAlternateSKU], collapse = "; ")
      ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!IsAlternateCode, !IsAlternateSKU)
}
