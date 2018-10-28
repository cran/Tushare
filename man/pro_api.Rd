\name{pro_api}
\alias{pro_api}
\title{
Tushare api to get data
}
\description{
After passing your token you get the api.
Let's name it 'pro'(such as pro = Tushare::pro_api(token = 'YOUR TOKEN HERE')).
Then you can call pro(api_name = 'API_NAME', other_params = 'YOUR OTHER PARAMETERS') to get data.
}
\usage{
pro_api(token)
}
\arguments{
  \item{token}{
Your token. You can obtain it from Tushare Pro website after logging.
}
}
\examples{
## step 1. Use your token and call Tushare::pro_api to get pro_api interface
## You can get your token from the Tushare Pro website<https://tushare.pro>.
## Check details via visiting <https://tushare.pro/document/1?doc_id=38>
## and <https://tushare.pro/document/1?doc_id=39>.

\dontrun{pro <- Tushare::pro_api(token = 'YOUR TOKEN HERE')}

## step 2. Use pro to call each data api via passing "api_name" and other optional parameters.

\dontrun{pro(api_name = 'daily', ts_code='000001.SZ', start_date='20180702', end_date='20180704')}
## This code will return daily k-line information of stock '000001.SZ' from 20180702 to 20180704.
##     ts_code trade_date open high  low close pre_close change pct_change        vol      amount
## 1 000001.SZ   20180704 8.63 8.75 8.61  8.61      8.67  -0.06      -0.69  711153.37  617278.559
## 2 000001.SZ   20180703 8.69  8.7 8.45  8.67      8.61   0.06        0.7 1274838.57 1096657.033
## 3 000001.SZ   20180702 9.05 9.05 8.55  8.61      9.09  -0.48      -5.28 1315520.13 1158545.868
}
