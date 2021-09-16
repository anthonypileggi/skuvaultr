
#' Parse Sales Channel from sales data
#' @param x sales data from a call to \code{\link{sv_get_sales}}
#' @importFrom magrittr "%>%"
#' @export
sv_parse_channel <- function(x) {

  x %>%
    dplyr::mutate(
      is_fulfilled = purrr::map2_lgl(FulfilledItems, FulfilledKits, ~nrow(.x) > 0 | nrow(.y) > 0),
      Channel = dplyr::case_when(
        Marketplace == "Amazon Seller Central - US" & is_fulfilled  ~ "Amazon FBA",
        Marketplace == "Amazon Seller Central - US" & !is_fulfilled ~ "Amazon",
        Marketplace == "Amazon Seller Central - CA"                 ~ "Amazon CA",
        Marketplace == "eBay Fixed Price"                           ~ "eBay",
        Marketplace == "Unknown"                                    ~ "Pricefalls",
        Marketplace == "Manual"                                     ~ "Service/Returns",
        Marketplace == "Walmart Marketplace"                        ~ "Walmart",
        TRUE                                                        ~ Marketplace
      )
    ) %>%
    dplyr::select(Id, Channel)

}