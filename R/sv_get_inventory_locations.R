#' Get locations
#' @export
sv_get_inventory_locations <- function(skus = NULL) {

  if (nchar(Sys.getenv("SKU_VAULT_TENANT_TOKEN")) == 0 | nchar(Sys.getenv("SKU_VAULT_USER_TOKEN")) == 0)
    sv_get_tokens()

  my_json <-
    list(
      TenantToken = Sys.getenv("SKU_VAULT_TENANT_TOKEN"),
      UserToken = Sys.getenv("SKU_VAULT_USER_TOKEN"),
      PageSize = 10000,
      PageNumber = -1,
      ProductSKUs = skus
    )

  # Iterate to get a full list of products
  go <- TRUE
  products <- list()
  while (go) {
    my_json$PageNumber <- my_json$PageNumber + 1
    message("Getting page ", my_json$PageNumber + 1, " of inventory location data.")
    r <- httr::POST("https://app.skuvault.com/api/inventory/getInventoryByLocation", body = my_json, encode = "json")
    new_products <- httr::content(r)$Items
    products <- c(products, new_products)
    if (!is.null(skus) || length(new_products) < my_json$PageSize)  # TODO: make sure all skus were checked for
      go <- FALSE
  }
  #sv_parse_response(products)

  # clean data
  x <- products[purrr::map_int(products, length) > 0]   # remove products w/out warehouse location data
  purrr::map_df(
    names(x),
    function(a) {
      purrr::map_df(x[[a]], function(b) dplyr::bind_cols(tibble::tibble(Sku = a), as_tibble(b)))
    }
  )
}