#' ---
#' Title: Time Series Coursework 1
#' Author: Shiu Hoi Lam
#' Date: 10/3/2026
#' ---

# 1. Load Data ------------------------------------------------------------
data<-read.csv("data/hk_tourism.csv")
head(data)
str(data)
options(scipen=999)

# 2a. Create Time Series ------------------------------------------------------------
ts_data<-ts(data$Total, start=c(2004,1), frequency=12)
plot(ts_data,
     main="Hong Kong Monthly Visitor Arrivals",
     ylab="Number of Visitors",
     xlab="Year")

# 2b. Variance Constant
log_visitors<-log(data$Total)
ts_log<-ts(log_visitors, start=c(2004,1), frequency=12)
plot(ts_log,
     main="Log of Hong Kong Monthly Visitor Arrivals",
     ylab="Log(Number of Visitors)",
     xlab="Year",
     col="black",
     lwd=2)

# 3. Decomposition ------------------------------------------------------------
decomp<-stl(ts_data, s.window="periodic")
plot(decomp)

# 4. Forecast ------------------------------------------------------------
library(prophet)
library(zoo)
library(dplyr)
data$Date<-as.Date(as.yearmon(paste(data$Year, data$Month), "%Y %b"))
hk_tourism_data<-data.frame(ds=data$Date, y=data$Total)
prophet_full_model<-prophet(hk_tourism_data)
future<-make_future_dataframe(prophet_full_model, periods=36, freq="month")
forecast<-predict(prophet_full_model, future)
plot(prophet_full_model, forecast)

prophet_plot_components(prophet_full_model, forecast)

# 5. Reduced Model ------------------------------------------------------------
data_pre_covid<-data[data$Date<as.Date("2020-01-01"), ]
d_pre<-data.frame(
    ds=data_pre_covid$Date,
    y=data_pre_covid$Total)
prophet_precovid_model<-prophet(d_pre)
future_pre<-make_future_dataframe(prophet_precovid_model, periods=36, freq="month")
forecast_pre<-predict(prophet_precovid_model, future_pre)
plot(prophet_precovid_model, forecast_pre)

prophet_plot_components(prophet_precovid_model, forecast_pre)

