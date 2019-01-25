#' Generate multipacks based on SkuVault product data
#' @param x data for a single product (non-kit) from a call to \code{\link{sv_get_products}}
#' @param n pack sizes (integer/vector)
#' @export
sv_create_multipacks <- function(x, n = c(2, 4, 8)) {
  purrr::map_df(
    n, 
    function(i) {
      dplyr::mutate(
        x,
        Description = paste(i, "Pack", Description),
        Sku = paste0(Sku, "-", i),
        PartNumber = paste(PartNumber, i, "Pack"),
        WeightValue = WeightValue * i,
        Cost = Cost * i
      )
    }
  )
}