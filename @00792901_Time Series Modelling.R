install.packages("ggplot2")  
install.packages("lubridate")  # For easier handling of date data
install.packages("tseries")  # For time series analysis 
install.packages("zoo")  # For handling time series data
install.packages("forecast")

library(ggplot2)
library(lubridate)
library(tseries)
library(zoo)
library(forecast)






file_path <- "C:\\Users\\gurwi\\statistic all task\\stats task 3\\AACG.csv"

# Import the CSV file into a data frame
my_data <- read.csv(file_path)

# View the first few rows of the data
head(my_data)




my_data$Date <- as.Date(my_data$Date, format="%Y-%m-%d")



#Plot the Close Price Over Time
ggplot(my_data, aes(x = Date, y = Close)) +
  geom_line(color = "blue", size = 1.2) +  # Line plot with blue color
  geom_point(color = "red", size = 2) +  # Points at each data point
  theme_minimal() +  # Minimal theme for a clean look
  labs(title = "Closing Price Over Time", x = "Date", y = "Close Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels



#Plot Volume Over Time
ggplot(my_data, aes(x = Date, y = Volume)) +
  geom_line(color = "green", size = 1.2) +  # Line plot with green color
  geom_point(color = "orange", size = 2) +  # Points at each data point
  theme_minimal() +
  labs(title = "Trading Volume Over Time", x = "Date", y = "Volume") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels




#Plotting Multiple Variables Together
ggplot(my_data) +
  geom_line(aes(x = Date, y = Close), color = "blue", size = 1.2) +  # Close Price
  geom_line(aes(x = Date, y = Volume / max(Volume) * max(Close)), color = "red", size = 1.2) +  # Normalized Volume
  scale_y_continuous(
    name = "Close Price", 
    sec.axis = sec_axis(
      trans = ~ . / max(my_data$Volume) * max(my_data$Close),  # Use the correct scaling here
      name = "Volume"
    )
  ) + 
  labs(title = "Close Price and Volume Over Time") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels




# Convert 'Close' to a time series object with monthly frequency
ts_data <- ts(my_data$Close, frequency = 12)  # Monthly data

# Decompose the time series
decomposed_data <- decompose(ts_data)
plot(decomposed_data)




# Perform the Augmented Dickey-Fuller (ADF) test
adf_test <- adf.test(my_data$Close, alternative = "stationary")

# Display the result of the ADF test
print(adf_test)




# Plot the time series for visual inspection
ggplot(my_data, aes(x = Date, y = Close)) + 
  geom_line(color = "blue") +
  labs(title = "Time Series Plot of Close Prices", x = "Date", y = "Close Price") +
  theme_minimal()






# First differencing to make the series stationary
diff_data <- diff(my_data$Close, differences = 1)

# Perform the ADF test on the differenced data
adf_test_diff <- adf.test(diff_data, alternative = "stationary")

# Print the result of the differenced series ADF test
print(adf_test_diff)






# Plot the differenced time series for visual inspection
ggplot(data.frame(Date = my_data$Date[-1], Diff_Close = diff_data), aes(x = Date, y = Diff_Close)) +
  geom_line(color = "red") +
  labs(title = "Differenced Time Series of Close Prices", x = "Date", y = "Differenced Close Price") +
  theme_minimal()





# Second differencing 
diff_data2 <- diff(diff_data, differences = 1)

# Perform ADF test on the second differenced series
adf_test_diff2 <- adf.test(diff_data2, alternative = "stationary")

# Print the result of the second differenced series ADF test
print(adf_test_diff2)







# Convert 'Close' to a time series object
ts_data <- ts(my_data$Close, frequency = 252)  # Assuming 252 trading days per year

# Fit the Auto ARIMA model
auto_arima_model <- auto.arima(ts_data)

# Display the model summary
summary(auto_arima_model)

# Forecast the next 30 days
forecasted_values <- forecast(auto_arima_model, h = 30)

# Plot the forecasted values
plot(forecasted_values)









# Plot ACF and PACF for manual ARIMA selection
acf(ts_data)  # Autocorrelation Function plot
pacf(ts_data)  # Partial Autocorrelation Function plot

# Fit an ARIMA model based on ACF and PACF plots
manual_arima_model <- arima(ts_data, order = c(1, 1, 1))

# Display the model summary
summary(manual_arima_model)

# Forecast the next 30 days using the manual ARIMA model
forecasted_values_manual <- forecast(manual_arima_model, h = 30)

# Plot the forecasted values
plot(forecasted_values_manual)









# Fit the ETS model
ets_model <- ets(ts_data)

# Display the ETS model summary
summary(ets_model)

# Forecast the next 30 days using the ETS model
forecasted_values_ets <- forecast(ets_model, h = 30)

# Plot the forecasted values
plot(forecasted_values_ets)










