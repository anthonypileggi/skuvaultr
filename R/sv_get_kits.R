#' Get kits
#' @importFrom magrittr "%>%"
#' @export
sv_get_kits <- function(skus = NULL) {

  # Iterate to get a full list of kits
  go <- TRUE
  pg <- -1
  pg_size <- 10000
  products <- list()
  while (go) {
    pg <- pg + 1
    message("Getting page ", pg + 1, " of kit data.")
    r <- sv_api("products/getKits", PageSize = pg_size, PageNumber = pg,
                KitSKUs = skus, GetAvailableQuantity = TRUE, IncludeKitCost = TRUE)
    new_products <- httr::content(r)$Kits
    products <- c(products, new_products)
    if (length(new_products) < pg_size)
      go <- FALSE
  }
  #sv_parse_response(products)

  # exit early if there is no data
  if (length(products) == 0) {
    message("No kits matching the requested SKUs were found.")
    return(NULL)
  }

  # parse response as tibble
  purrr::map_df(
    products,
    function(r) {
      vars <- names(r)[purrr::map_lgl(r, ~length(.x) == 1)]        # TODO: this ignores entries with > 1 length (e.g., multiple suppliers)
      tmp <- tibble::as_tibble(r[vars])
      r$KitLines <- purrr::map_df(r$KitLines, rlang::squash)       # TODO: get multi-item kits
      tmp$KitLines <- list(dplyr::as_tibble(r$KitLines))
      tmp <- dplyr::rename(tmp, Sku = SKU)
      tmp
    }
  ) %>%
    dplyr::mutate_at(c("LastModifiedDateTimeUtc", "AvailableQuantityLastModifiedDateTimeUtc"), sv_parse_datetime)
}
