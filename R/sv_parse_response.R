#' Parse a nested list returned by the API as a tibble
#' @param response a nested list
#' @export
sv_parse_response <- function(response) {

  purrr::map_df(
    response,
    function(r) {
      #vars <- names(r)[purrr::map_lgl(r, ~!is.list(.x))]
      vars <- names(r)[purrr::map_lgl(r, ~length(.x) == 1)]        # TODO: this ignores entries with > 1 length (e.g., multiple suppliers)
      tmp <- tibble::as_tibble(r[vars])
      for (v in c("SupplierInfo", "KitLines")) {                                  # format list objects as tibbles
        if (v %in% names(r)) {
          r[[v]] <- suppressWarnings(purrr::map_df(r[[v]], rlang::squash))
          tmp[[v]] <- list(dplyr::as_tibble(r[[v]]))
        }
      }
      for (v in c("FulfilledKits", "MerchantKits", "SaleKits")) {
        if (v %in% names(r))
          tmp[[v]] <- list(sv_parse_kit(r[[v]]))
      }
      for (v in c("FulfilledItems", "MerchantItems", "SaleItems")) {
        if (v %in% names(r))
          tmp[[v]] <- list(sv_parse_item(r[[v]]))
      }
      if ("ProcessedItems" %in% names(r))
        tmp[["ProcessedItems"]] <- list(purrr::map_df(r[["ProcessedItems"]], dplyr::as_tibble))
      if ("LineItems" %in% names(r))
        tmp[["LineItems"]] <- list(purrr::map_df(r[["LineItems"]], dplyr::as_tibble))
      if ("Statuses" %in% names(r))
        tmp[["Statuses"]] <- paste(sort(unlist(r[["Statuses"]])), collapse = "; ")
      if ("ShippingCost" %in% names(r))
        tmp[["ShippingCost"]] <- paste(r[["ShippingCost"]][c("s", "a")], collapse = "")
      tmp
    }
  )

}