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
library(tidyverse)
library(forecast)
library(data.table)

utils::globalVariables(c(".", "adj_factor", "left_join", "mutate_all", "select", "str_squish"))

# Return the interface via passing a token.
# The interface lets you call each api in Tushare Pro.
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
    if(is.null(res$data)) {
      return(NULL)
    }
    columns <- res$data$fields
    items = unlist(res$data$items)
    df <- data.frame(matrix(items,
                           nrow = length(items) / length(columns),
                           byrow = TRUE))
    names(df) <- columns
    return(df)
  })
}


# return the interface "pro_bar"
# "pro_bar" is an interface like "bar" in tushare.
# The interface lets you get quotations of different financial products such as stock, mutual fund and index.
pro_bar <- function(token) {
  pro_api <- Tushare::pro_api(token = token)
  return(function(ts_code='',
                  start_date=NULL,
                  end_date=NULL,
                  freq='D',
                  asset='E',
                  market='',
                  adj = NULL,
                  ma = c(),
                  retry_count = 3) {
    ## Adjust parameters
    ts_code <- ts_code %>% str_squish %>% toupper
    freq <- freq %>% str_squish %>% toupper
    asset <- asset %>% str_squish %>% toupper

    for(ii in 1:retry_count) {
      try_res <- try({
        data <- NULL
        if(asset == 'E') {                ## Stock & Mutual Fund
          if(freq == 'D') {

            ## the fields will be adjusted
            PRICE_COLS = c('open', 'close', 'high', 'low', 'pre_close')
            df = pro_api(api_name = 'daily',
                         ts_code = ts_code,
                         start_date = start_date,
                         end_date = end_date)

            ## adjust the prices
            if (!is.null(adj)) {
              ## get adjusted factors
              fcts = pro_api(api_name = 'adj_factor',
                             ts_code = ts_code,
                             start_date = start_date,
                             end_date = end_date)
              data = left_join(df, fcts) %>% mutate_all(as.character)

              ## modify each prices via adjusted factors
              for(col in PRICE_COLS) {
                if(adj == 'hfq') {
                  data[,col] = as.numeric(data[,col]) * as.numeric(data[,'adj_factor'])
                } else {
                  data[,col] = as.numeric(data[,col]) * as.numeric(data[,'adj_factor']) / as.numeric(data[1,'adj_factor'])
                }
                data[,col] = sprintf("%.2f", data[,col])
              }
              data = data %>% select(-adj_factor)
            } else {
              data = df %>% mutate_all(as.character)
            }

            ## add moving average prices
            if(!is.null(ma) & length(ma) > 0) {
              for(a in ma) {
                if(round(a) == a) {
                  data[,paste0('ma', a)] = ma(as.numeric(data[,'close']), a, centre=F) %>%
                    c() %>%
                    sprintf("%.2f", .) %>%
                    shift(., n=a%/%2, fill=NA, type="lead")
                }
              }
            }

            ## modify the data type as numeric
            for(col in PRICE_COLS) {
              data[,col] = as.numeric(data[,col])
            }
          }
        }
        if (asset == 'I') {        ## Index in Shanghai & Shenzhen Stock Exchange
          if(freq == 'D') {
            data <- pro_api(api_name = 'index_daily',
                            ts_code = ts_code,
                            start_date = start_date,
                            end_date = end_date)
          }
        }
        if (asset == 'C') {        ## Digital Coins
          #//////////////////////// well soon
          next
        }
        return(data)
      })
      if('try-error' %in% class(try_res)) {
        return(NULL)
      }
    }
  })
}
