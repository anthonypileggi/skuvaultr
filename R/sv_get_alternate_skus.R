
#' Get a list of alternate SKUs
#' @param data data from call to \code{\link{skuvaultr::sv_get_products}}
#' @importFrom magrittr "%>%"
#' @export
sv_get_alternate_skus <- function(data = NULL) {
  
  # TODO: speed-up by bypassing sv_get_products()
  
  # load data (if not provided)
  if (is.null(data))
    data <- skuvaultr::sv_get_products()

  # Unfurl additional skus
  data %>%
    dplyr::select(Sku, Cost, AlternateSku) %>%
    dplyr::filter(nchar(AlternateSku) > 0) %>%
    dplyr::mutate(
      AltSku = purrr::map(AlternateSku, ~stringr::str_split(.x, "; ")[[1]])
    ) %>%
    tidyr::unnest(AltSku) %>% 
    dplyr::select(Sku, AltSku)
}