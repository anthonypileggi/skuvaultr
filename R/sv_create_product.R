#' Create a new product in SkuVault
#' @param Sku Sku
#' @param Brand Brand
#' @param Classification Classification
#' @param Supplier Supplier
#' @param Cost Cost
#' @export
sv_create_product <- function(Sku, Brand, Classification, Supplier, Cost, ...) {

  sv_api_call(
    path = "products/createProduct",
    Sku = Sku,
    Brand = Brand,
    Classification = Classification,
    Supplier = Supplier,
    Cost = Cost,
    SupplierInfo = list(
      SupplierName = Supplier,
      Cost = Cost,
      IsPrimary = TRUE,
      IsActive = TRUE
    ),
    ...
  )

}