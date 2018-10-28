#' @title Get API for Tushare Pro
#'
#' @description This package supports a api for Tushare Pro
#'
#' @param token
#'
#' @return function
#'
#' @example pro = pro_api(token)
#'
#' @export

library(httr)

pro_api <- function(token) {
  http_url <- "http://api.tushare.pro"

  return(function(api_name, ...){
    params <- list(
      token = token,
      api_name = api_name,
      params = list(...)
    )
    r <- httr::POST(http_url, body = params, encode = "json")
    res <- httr::content(r, "parsed", 'application/json')
    columns <- res$data$fields
    items = unlist(res$data$items)
    df <- data.frame(matrix(items,
                           nrow = length(items) / length(columns),
                           byrow = TRUE))
    names(df) <- columns
    return(df)
  })
}
