#' Make a SkuVault API call
#' @param path path to api endpoint
#' @export
sv_api <- function(path, ...) {

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
  response <- httr::POST(url, body = my_json, encode = "json")

  # check status code
  if (httr::status_code(response) != 200) {
    stop(
      sprintf(
        "SkuVault API request failed [%s]\n%s\n<%s>",
        httr::status_code(resp)
      ),
      call. = FALSE
    )
  }

  response
}