#' Parse a nested list returned by the API as a tibble
#' @param response a nested list
#' @export
sv_parse_response <- function(response) {
  purrr::map_df(
    response,
    function(r) {
      vars <- names(r)[purrr::map_lgl(r, ~length(.x) == 1)]   # TODO: this ignores entries with > 1 length (e.g., multiple suppliers)
      tibble::as_tibble(r[vars])
    }
  )
}