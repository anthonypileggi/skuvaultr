#' Parse a list of items
sv_parse_item <- function(x) {
  purrr::map_df(x,
    function(aa) {
      out <- dplyr::tibble(
        Sku = aa$Sku,
        Quantity = aa$Quantity,
        Price = paste(aa$UnitPrice[c("s", "a")], collapse = "")
      )
      if (!is.null(aa$PartNumber))
        out <- dplyr::mutate(out, PartNumber = aa$PartNumber)
      out
    }
  )
}

#' Parse a list of kits
sv_parse_kit <- function(x) {
  purrr::map_df(x,
    function(a) {
      dplyr::tibble(
        Sku = a$Sku,
        Quantity = a$Quantity,
        Price = paste(a$UnitPrice[c("s", "a")], collapse = ""),
        Items = list(sv_parse_item(a$Items))
      )
    })
}
