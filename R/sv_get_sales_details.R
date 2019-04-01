#' Get cleaned multi-channel sales data (1 row / sku)
#' @param ... arguments passed on to \code{\link{sv_get_sales}}
#' @importFrom magrittr "%>%"
#' @export
sv_get_sales_details <- function(...) {

  # load sales data from the API
  sales <- sv_get_sales(...)

  # ignore {cancelled orders, FBA transfers}
  sales <- sales %>%
    dplyr::filter(
      Status %in% c("Completed", "ReadyToShip"),
      Marketplace != "TransferSaleHoldsPendingQuantity"
    )

  # convert to 1 row per sku
  out <-
    purrr::map_df(
      c("FulfilledKits", "MerchantKits", "FulfilledItems", "MerchantItems"),
      function(v) {
        out <- sales %>%
          dplyr::select(Id:Marketplace, v) %>%
          tidyr::unnest()
        if ("Price" %in% names(out))
          out <- dplyr::mutate(out, Price = as.numeric(stringr::str_sub(Price, 2, -1)))
        if ("Items" %in% names(out))
          out <- dplyr::select(out, -Items)
        out
      }
    ) %>%
    dplyr::arrange(SaleDate)

  # attach product costs
  #   - if a sku is an AP, just use first occurrence
  products <- sv_get_product_inventory() %>%
    dplyr::select(Sku, Brand, Supplier, Classification, Type, Cost)
  out <- out %>%
    dplyr::left_join(products, by = "Sku") %>%
    dplyr::mutate(
      Date = lubridate::date(SaleDate)
    )

  # attach Channel/FBA status
  channels <- sales %>%
    dplyr::mutate(
      is_fulfilled = purrr::map2_lgl(FulfilledItems, FulfilledKits, ~nrow(.x) > 0 | nrow(.y) > 0),
      Channel = dplyr::case_when(
        Marketplace == "Amazon Seller Central - US" & is_fulfilled  ~ "Amazon FBA",
        Marketplace == "Amazon Seller Central - US" & !is_fulfilled ~ "Amazon",
        Marketplace == "eBay Fixed Price"                           ~ "eBay",
        Marketplace == "Unknown"                                    ~ "Pricefalls",
        Marketplace == "Manual"                                     ~ "Service/Returns",
        Marketplace == "Walmart Marketplace"                        ~ "Walmart",
        TRUE                                                        ~ Marketplace
      )
    ) %>%
    dplyr::select(Id, Channel)
  out <- dplyr::left_join(out, channels, by = "Id")

  as_sales_details(out)
}


## CONSTRUCTORS ======================================

#' Assign an object to the `sales_details` class
#' @param x an object containing summary sales data
#' @export
as_sales_details <- function(x) {
  if (!is_sales_details(x))
    class(x) <- append("sales_details", class(x))
  x
}

#' Check if an object is of the `sales_details` class
#' @param x an R object to check
#' @export
is_sales_details <- function(x) {
  inherits(x, "sales_details")
}



# METHODS =====================================

#' Summarize a sales_details object
#' @param x object of 'sales_details' class
#' @param ... grouping dimensions
#' @importFrom magrittr "%>%"
#' @export
summary.sales_details <- function(x, ...) {
  dims <- rlang::quos(...)
  out <- x %>%
    dplyr::group_by(!!!dims) %>%
    dplyr::summarize(
      price = mean(Price),
      cost = mean(Cost),
      orders = dplyr::n_distinct(ChannelId),
      quantity = sum(Quantity),
      revenue = sum(Quantity * Price),
      profit = sum(Quantity * (Price - Cost))
    ) %>%
    dplyr::arrange(desc(revenue))
  as_sales_details(out)
}


#' Display a sales_details object
#' @importFrom magrittr "%>%"
#' @export
display.sales_details <- function(x, ...) {
  summary(x, ...) %>%
    DT::datatable(
      rownames = FALSE,
      options = list(
        dom = ifelse(nrow(.) > 20, "lfpt", "t"),
        pageLength = min(20, nrow(x))
      )
    ) %>%
    DT::formatCurrency(c("price", "cost"), digits = 2) %>%
    DT::formatCurrency(c("revenue", "profit"), digits = 0) %>%
    DT::formatRound(c("orders", "quantity"), digits = 0)
}