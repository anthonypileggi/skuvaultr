#' Get products
#' @export
sv_get_products <- function(skus = NULL) {

  # Iterate to get a full list of products
  products <- sv_api(path = "products/getProducts", PageSize = 10000, ProductSKUs = skus)

  # exit early if there is no data
  if (length(products) == 0) {
    message("No products matching the requested SKUs were found.")
    return(NULL)
  }

  # parse respose
  products %>%
    sv_parse_response() %>%
    dplyr::mutate_at(c("CreatedDateUtc", "ModifiedDateUtc"), sv_parse_datetime) %>%
    dplyr::mutate_at("WeightValue", as.numeric)

}
