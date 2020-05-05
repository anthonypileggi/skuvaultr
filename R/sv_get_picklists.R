
#' Get picklist given a set of SaleIds
#' @param saleids saleid(s) (character/vector)
#' @param data data returned from 'sv_get_sales'
#' @importFrom magrittr "%>%"
#' @export
sv_get_picklists <- function(saleids, data = NULL) {

  # load sales data (if not provided)
  if (is.null(data)) {
    data <- skuvaultr::sv_get_sales(order_id = saleids)
    if (nrow(data) != length(saleids))
      stop("Not all sales were found!")
  }

  # get order contents
  order <- skuvaultr::sv_parse_order_contents(data)
  order <- dplyr::rename(order, picklist = Items)
  data <- dplyr::left_join(data, order, by = "Id")

  # get item locations
  #   -- check if it can be fulfilled
  skus <- unique(dplyr::bind_rows(data$picklist)$Sku)
  locs <- sv_get_inventory_locations(skus = skus) %>%
    dplyr::mutate_at("Sku", toupper) %>%
    dplyr::rename(Available = Quantity)
  data <- data %>%
    dplyr::mutate(
      picklist = purrr::map(picklist, ~dplyr::left_join(.x, locs, by = "Sku"))
    )

  # get fulfill/dropship status
  data <- data %>%
    dplyr::mutate(
      fulfill = purrr::map_chr(picklist,
        function(p) {
          p %>%
            dplyr::group_by(Sku) %>%
            dplyr::summarize(
              Quantity = head(Quantity, 1),
              in_house = sum(Available[!(LocationCode %in% sv_dropship_locs())]),
              dropship = sum(Available[LocationCode %in% sv_dropship_locs()])
            ) %>%
            dplyr::summarize(
              fulfill = dplyr::case_when(
                all(in_house >= Quantity)              ~ "in-house",
                all(dropship >= Quantity)              ~ "dropship",
                all(in_house + dropship >= Quantity)   ~ "hybrid",
                TRUE                                   ~ "no"
              )
            ) %>%
            dplyr::pull(fulfill)
        })
    )

  # account for items w/ multiple locations!
  # --> empty drop-ship bins as last resort
  # --> prioritized by bin location (i.e., alphabet)
  # --> ignore extra locations we will not be picking from
  data <- data %>%
    dplyr::mutate(
      picklist = purrr::map2(
        picklist, fulfill,
        function(p, f) {
          if (f == "in-house") {
            p <- dplyr::filter(p, !(LocationCode %in% sv_dropship_locs()))
          } else if (f == "dropship") {
            p <- dplyr::filter(p, LocationCode %in% sv_dropship_locs())
          }
          p %>%
            dplyr::group_by(Sku) %>%
            dplyr::mutate(
              LocationCode = ifelse(
                LocationCode %in% sv_dropship_locs(),
                paste0("ZZZ-", LocationCode),
                LocationCode
                )
            ) %>%
            dplyr::arrange(LocationCode) %>%   # dropship suppliers prioritized A-Z
            dplyr::mutate(
              Pick = purrr::map2_dbl(Quantity, Available, ~min(c(.x, .y))),
              Total = cumsum(Pick),
              Extra = ifelse(Total > Quantity, Total - Quantity, 0),
              Quantity = Pick - Extra,
              LocationCode = ifelse(
                stringr::str_detect(LocationCode, "ZZZ-"),
                stringr::str_replace(LocationCode, "ZZZ-", ""),
                LocationCode
                )
            ) %>%
            dplyr::ungroup() %>%
            dplyr::select(-Pick, -Total, -Extra) %>%
            dplyr::filter(Quantity > 0)
        }
      )
    )

  # restrict to selected columns
  data <- dplyr::select(data, Id, Status, fulfill, picklist)

  return(data)
}