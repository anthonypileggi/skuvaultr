#' Call a SkuVault API endpoint and iterate over all pages
#' @param ... additional arguments passed to the API
#' @export
sv_api <- function(...) {
  params <- list(...)
  pg_size <- ifelse(is.null(params$PageSize), 10000, params$PageSize)
  go <- TRUE
  pg <- -1
  x <- list()
  while (go) {
    pg <- pg + 1
    message("Getting page ", pg + 1, " of API data. (PageSize = ", pg_size, ")")
    r <- sv_api_call(..., PageNumber = pg)
    new_x <- httr::content(r)
    if (params$path == "products/getKits") {
      new_x <- new_x$Kits
    } else if (params$path == "products/getProducts") {
      new_x <- new_x$Products
    } else if (params$path == "products/getBrands") {
      new_x <- new_x$Brands
    } else if (params$path == "inventory/getInventoryByLocation") {
      new_x <- new_x$Items
    } else if (params$path == "inventory/getTransactions") {
      new_x <- new_x$Transactions
    }
    # new_x <- dplyr::case_when(
    #   params$path == "products/getKits" ~ new_x$Kits,
    #   params$path == "products/getProducts" ~ new_x$Products,
    #   params$path == "products/getBrands" ~ new_x$Brands,
    #   params$path == "inventory/getInventoryByLocation" ~ new_x$Items,
    #   params$path == "inventory/getTransactions" ~ new_x$Transactions
    # )
    message("response has ", length(new_x), " rows.")
    x <- c(x, new_x)
    if (length(new_x) < pg_size)
      go <- FALSE
  }
  return(x)
}

#' Make a SkuVault API call
#' @param path path to api endpoint
#' @param ... additional named parameters
#' @export
sv_api_call <- function(path, ...) {

  # setup creds
  if (nchar(Sys.getenv("SKU_VAULT_TENANT_TOKEN")) == 0 | nchar(Sys.getenv("SKU_VAULT_USER_TOKEN")) == 0)
    sv_get_tokens()

  # setup query parameters
  my_json <-
    list(
      TenantToken = Sys.getenv("SKU_VAULT_TENANT_TOKEN"),
      UserToken = Sys.getenv("SKU_VAULT_USER_TOKEN"),
      ...
    )

  # call api
  url <- httr::modify_url("https://app.skuvault.com", path = paste0("api/", path))
  if ("KitSKUs" %in% names(my_json) && length(my_json$KitSKUs) == 1 && path %in% c("products/getKits")) {
    response <- httr::POST(url, body = my_json)
  } else {
    #my_json <- jsonlite::toJSON(my_json, force = T, auto_unbox = T)
    response <- httr::POST(url, body = my_json, encode = "json")
  }

  # check status code
  if (httr::status_code(response) != 200) {
    stop(
      sprintf(
        "SkuVault API request failed [%s] \n Errors: [%s]",
        httr::status_code(response),
        paste(httr::content(response)$Errors, collapse = ";")
      ),
      call. = FALSE
    )
  }

  response
}