#' Get products
#' @export
sv_get_products <- function() {

  if (nchar(Sys.getenv("SKU_VAULT_TENANT_TOKEN")) == 0 | nchar(Sys.getenv("SKU_VAULT_USER_TOKEN")) == 0)
    sv_get_tokens()

  my_json <-
    list(
      TenantToken = Sys.getenv("SKU_VAULT_TENANT_TOKEN"),
      UserToken = Sys.getenv("SKU_VAULT_USER_TOKEN"),
      PageSize = 10000,
      PageNumber = -1
      )

  # Iterate to get a full list of products
  go <- TRUE
  products <- list()
  while (go) {
    my_json$PageNumber <- my_json$PageNumber + 1
    message("Getting page ", my_json$PageNumber + 1, " of product data.")
    r <- httr::POST("https://app.skuvault.com/api/products/getProducts", body = my_json, encode = "json")
    new_products <- httr::content(r)$Products
    products <- c(products, new_products)
    if (length(new_products) < my_json$PageSize)
      go <- FALSE
  }
  sv_parse_response(products)
}
