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
          tmp[[v]] <- list(skuvaultr:::sv_parse_kit(r[[v]]))
      }
      for (v in c("FulfilledItems", "MerchantItems", "SaleItems")) {
        if (v %in% names(r))
          tmp[[v]] <- list(skuvaultr:::sv_parse_item(r[[v]]))
      }
      for (v in c("ProcessedItems", "LineItems", "Attributes")) {
        if (v %in% names(r))
          tmp[[v]] <- list(purrr::map_df(r[[v]], dplyr::as_tibble))
      }
      for (v in c("ShippingInfo", "ContactInfo")) {
        if (v %in% names(r))
          tmp[[v]] <- list(dplyr::as_tibble(r[[v]]))
      }
      if ("Statuses" %in% names(r))
        tmp[["Statuses"]] <- paste(sort(unlist(r[["Statuses"]])), collapse = "; ")
      if ("ShippingCost" %in% names(r))
        tmp[["ShippingCost"]] <- paste(r[["ShippingCost"]][c("s", "a")], collapse = "")

      if ("Context" %in% names(r)) {
        if (is.null(r[["Context"]])) {
          tmp[["ContextId"]] <- NA_character_
        } else {
          tmp[["ContextId"]] <- r[["Context"]]$ID
        }
      }

      tmp
    }
  )

}