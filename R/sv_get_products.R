#' Get products
#' @export
sv_get_products <- function() {

  # Iterate to get a full list of products
  go <- TRUE
  pg <- -1
  pg_size <- 10000
  products <- list()
  while (go) {
    pg <- pg + 1
    message("Getting page ", pg + 1, " of product data.")
    r <- sv_api("products/getProducts", PageSize = pg_size, PageNumber = pg)
    new_products <- httr::content(r)$Products
    products <- c(products, new_products)
    if (length(new_products) < pg_size)
      go <- FALSE
  }
  #sv_parse_response(products)

  # parse respose
  products %>%
    sv_parse_response() %>%
    dplyr::mutate_at(c("CreatedDateUtc", "ModifiedDateUtc"), sv_parse_datetime)

}
