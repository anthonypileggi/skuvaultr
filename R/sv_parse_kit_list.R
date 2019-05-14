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
      out <- dplyr::tibble(
        Sku = a$Sku,
        Quantity = a$Quantity,
        Price = paste(a$UnitPrice[c("s", "a")], collapse = ""),
        #Items = list(sv_parse_item(a$Items))
      )
      if ("KitItems" %in% names(a)) {
        out <- dplyr::mutate(out, Items = list(sv_parse_sale_kit(a$KitItems)))
      } else {
        out <- dplyr::mutate(out, Items = list(sv_parse_item(a$Items)))
      }
      out
    })
}

#' Parse a SaleKit item
sv_parse_sale_kit <- function(xx) {
  purrr::map2_df(names(xx), xx, ~dplyr::tibble(Sku = .x, Quantity = .y))
}