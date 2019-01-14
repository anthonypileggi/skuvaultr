#' Get a SkuVault API token set (TenantToken/UserToken)
#' @param email SkuVault account email address
#' @param password SkuVault account password
#' @export
sv_get_tokens <- function(email = Sys.getenv("SKU_VAULT_EMAIL"),
                         password = Sys.getenv("SKU_VAULT_PASSWORD")) {

  my_json <- list(Email = email, Password = password)
  r <- httr::POST("https://app.skuvault.com/api/gettokens", body = my_json, encode = "json")
  tokens <- httr::content(r)
  Sys.setenv("SKU_VAULT_TENANT_TOKEN" = tokens$TenantToken)
  Sys.setenv("SKU_VAULT_USER_TOKEN" = tokens$UserToken)
  tokens
}
