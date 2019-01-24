#' Parse a nested list returned by the API as a tibble
#' @param response a nested list
#' @export
sv_parse_response <- function(response) {
  # purrr::map_df(
  #   response,
  #   function(r) {
  #     vars <- names(r)[purrr::map_lgl(r, ~length(.x) == 1)]   # TODO: this ignores entries with > 1 length (e.g., multiple suppliers)
  #     tibble::as_tibble(r[vars])
  #   }
  # )

  purrr::map_df(
    response,
    function(r) {
      vars <- names(r)[purrr::map_lgl(r, ~length(.x) == 1)]        # TODO: this ignores entries with > 1 length (e.g., multiple suppliers)
      tmp <- tibble::as_tibble(r[vars])
      for (v in c("SupplierInfo", "KitLines")) {                                  # format list objects as tibbles
        if (v %in% names(r)) {
          r[[v]] <- purrr::map_df(r[[v]], rlang::squash)
          tmp[[v]] <- list(dplyr::as_tibble(r[[v]]))
        }
      }
      tmp
    }
  )

}