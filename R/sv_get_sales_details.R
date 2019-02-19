#' Get cleaned multi-channel sales data (1 row / sku)
#' @param ... arguments passed on to \code{\link{sv_get_sales}}
#' @importFrom magrittr "%>%"
#' @export
sv_get_sales_details <- function(...) {

  # load sales data from the API
  sales <- sv_get_sales(...)

  # convert to 1 row per sku
  out <-
    purrr::map_df(
      c("FulfilledKits", "MerchantKits", "FulfilledItems", "MerchantItems"),
      function(v) {
        out <- sales %>%
          dplyr::select(Id:Marketplace, v) %>%
          tidyr::unnest() %>%
          dplyr::mutate(
            Price = as.numeric(stringr::str_sub(Price, 2, -1))
          )
        if ("Items" %in% names(out))
          out <- dplyr::select(out, -Items)
        out
      }
    ) %>%
    dplyr::arrange(SaleDate)

  # attach product costs
  out <- out %>%
    dplyr::left_join(
      dplyr::bind_rows(
        dplyr::select(sv_get_products(unique(out$Sku)), Sku, Cost),
        dplyr::select(sv_get_kits(unique(out$Sku)), Sku, Cost)
      ),
      by = "Sku"
    ) %>%
    dplyr::mutate(
      Date = lubridate::date(SaleDate)
    )

  as_sales_details(out)
}


## CONSTRUCTORS ======================================

#' Assign an object to the `sales_details` class
#' @param x an object containing summary sales data
#' @export
as_sales_details <- function(x) {
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
  x %>%
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
}
