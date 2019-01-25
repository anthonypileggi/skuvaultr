#' Generate a ChannelAdvisor Listing from SkuVault product data
#' @param x data from a call to \code{\link{sv_get_products}}
#' @export
sv_create_listing <- function(x) {
  dplyr::transmute(
    x,
    `Auction Title` = Description,
    `Inventory Number` = Sku,
    `Brand` = Brand,
    `Manufacturer` = Supplier,
    MPN = PartNumber,
    #ASIN = NA_character_,                                                  # TODO: how do we get this??
    `Seller Cost` = Cost,
    `Buy It Now Price` = ifelse(SalePrice > 0, SalePrice, 2 * Cost),        # default to 50% profit margins if 'SalePrice' empty in SkuVault
    Description = Description,                                              # TODO: this should feed off a template
    #`Picture URLs` = NA_character_,                                        # TODO: setup FTP; automatically associate picture urls
    Attribute81Name = "under1pound",
    Attribute81Value = ifelse(WeightValue > 0, WeightValue < 1, NA)         # TODO: we need to start recording the 'weight' field in SkuVault to automate this!
  ) 
}