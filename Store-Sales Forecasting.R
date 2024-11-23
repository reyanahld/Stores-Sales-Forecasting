library(readr)
library(tidyverse)
library(lubridate)
library(forecast)
library(ggplot2)
###########################
data <- read.csv(file = "D:\\portofolio\\Forecasting\\stores_sales_forecasting.csv", header = TRUE)
head(data)

########################
# Pastikan Order.Date adalah dalam format tanggal
data$Order.Date <- mdy(data$Order.Date)

# Agregasi data berdasarkan bulan
data_monthly <- data %>%
  mutate(Month = floor_date(Order.Date, "month")) %>%
  group_by(Month) %>%
  summarise(Monthly_Sales = sum(Sales))

# Cek hasil agregasi
head(data_monthly)


###########################
# Membuat objek time series untuk penjualan bulanan
ts_sales <- ts(data_monthly$Monthly_Sales, frequency = 12, start = c(2016, 11)) # asumsikan mulai pada Nov 2016

# Cek time series
plot(ts_sales, main = "Time Series Penjualan Bulanan", ylab = "Penjualan", xlab = "Waktu")


##########################

# Visualisasi penjualan bulanan
ggplot(data_monthly, aes(x = Month, y = Monthly_Sales)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red") +
  labs(title = "Tren Penjualan Bulanan", x = "Bulan", y = "Total Penjualan") +
  theme_minimal()


# Bar plot untuk distribusi segment pelanggan
ggplot(data, aes(x = Segment)) +
  geom_bar(fill = "lightcoral", color = "red") +
  labs(title = "Distribusi Segment Pelanggan", x = "Segment", y = "Jumlah") +
  theme_minimal()


###########################
# Membangun model ARIMA
model_arima <- auto.arima(ts_sales)

# Menampilkan ringkasan model ARIMA
summary(model_arima)

# Membuat forecast untuk 12 bulan ke depan
forecast_sales <- forecast(model_arima, h = 12)

# Menampilkan hasil peramalan dalam bentuk angka
forecast_results <- as.data.frame(forecast_sales)

# Menampilkan hasil peramalan (point forecast, lower, dan upper)
print(forecast_results)

# Plot hasil forecast
plot(forecast_sales, main = "Forecast Penjualan untuk 12 Bulan ke Depan", ylab = "Penjualan", xlab = "Waktu")


# Menghitung akurasi dari model
accuracy(forecast_sales)

# Menghitung akurasi dari model
accuracy_metrics <- accuracy(forecast_sales)

# Menampilkan MAPE dalam persen
mape_value <- accuracy_metrics[5]  # MAPE berada di kolom ke-5 dari output accuracy()
cat("MAPE: ", mape_value, "%\n")

