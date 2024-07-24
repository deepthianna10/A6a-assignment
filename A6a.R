# Install and load necessary packages
if (!require(quantmod)) install.packages("quantmod", dependencies = TRUE)
if (!require(tidyverse)) install.packages("tidyverse", dependencies = TRUE)
if (!require(timetk)) install.packages("timetk", dependencies = TRUE)
library(quantmod)
library(tidyverse)
library(timetk)

# Define the ticker symbol and date range
ticker <- "WIPRO.NS"
start_date <- as.Date("2021-04-01")
end_date <- as.Date("2024-06-30")

# Download the data
getSymbols(ticker, src = "yahoo", from = start_date, to = end_date)
data <- get(ticker)

# Select the Adjusted Close column
df <- data %>% tk_tbl() %>% select(date = index, adj_close = contains("Adjusted"))

# Check for missing values
cat("Missing values:\n")
print(colSums(is.na(df)))
# Plot the data
ggplot(df, aes(x = date, y = adj_close)) +
  geom_line(color = "blue") +
  labs(title = "WIPRO.NS Adj Close Price",
       x = "Date",
       y = "Adj Close Price") +
  theme_minimal()



# Decomposition of time series
# Install ggplot2 if it is not already installed
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)

# Load the data
df <- read.csv("C:/Users/HP/Downloads/WIT.csv")

# Ensure the 'Adj Close' column exists and convert it to a time series object
# Assuming the data is monthly, adjust the frequency accordingly if it is different
adj_close_ts <- ts(df$Adj.Close, frequency = 12)

# Ensure the 'Adj Close' column exists and convert it to a time series object
# Remove rows with missing values in 'Adj.Close'
df <- df[!is.na(df$Adj.Close), ]
# Check the structure of the dataframe
str(df)


# Perform seasonal decomposition
result <- decompose(adj_close_ts, type = "multiplicative")

# Check the number of observations
print(paste("Number of observations:", nrow(df)))

# Convert the decomposition result to a data frame for ggplot
decomp_df <- data.frame(
  Date = time(adj_close_ts),
  Observed = result$observed,
  Trend = result$trend,
  Seasonal = result$seasonal,
  Residual = result$random
)

# Check the length of each component to debug
cat("Length of observed: ", length(result$observed), "\n")
cat("Length of trend: ", length(result$trend), "\n")
cat("Length of seasonal: ", length(result$seasonal), "\n")
cat("Length of random: ", length(result$random), "\n")

# Ensure the 'Adj.Close' column exists and convert it to a time series object
# Convert the 'Date' column to Date type
df$Date <- as.Date(df$Date, format="%Y-%m-%d")

# Check for missing values in the time series
anyNA(adj_close_ts)

# Replace any NA values with the mean of the series (if any NA values are present)
if (anyNA(adj_close_ts)) {
  adj_close_ts[is.na(adj_close_ts)] <- mean(adj_close_ts, na.rm = TRUE)
}

# Ensure all values are finite
if (!all(is.finite(adj_close_ts))) {
  adj_close_ts[!is.finite(adj_close_ts)] <- mean(adj_close_ts, na.rm = TRUE)
}

# Now perform the seasonal decomposition again
result <- decompose(adj_close_ts, type = "multiplicative")

# Plot the decomposed components using base R plotting
par(mfrow = c(4, 1), mar = c(4, 4, 2, 1))
plot(result$observed, main = "Observed", ylab = "Observed")
plot(result$trend, main = "Trend", ylab = "Trend")
plot(result$seasonal, main = "Seasonal", ylab = "Seasonal")
plot(result$random, main = "Residual", ylab = "Residual")
par(mfrow = c(1, 1))  # Reset layout

# Determine the split index
split_index <- floor(0.8 * nrow(df))

# Create training and test sets
train_data <- df[1:split_index, ]
test_data <- df[(split_index + 1):nrow(df), ]

# Print the number of rows in each set to verify
cat("Number of rows in the training set: ", nrow(train_data), "\n")
cat("Number of rows in the test set: ", nrow(test_data), "\n")

# Check the first few rows of the training and test sets
head(train_data)
head(test_data)

ggplot(decomp_df, aes(x = Date)) +
  geom_line(aes(y = Trend), color = "blue") +
  labs(title = "Trend", y = "Trend") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ key, ncol = 1, scales = "free_y") +
  theme(strip.text = element_text(size = 15))

# Fit the Holt-Winters model
holt_winters_model <- hw(train_data, seasonal = "multiplicative")

# Forecast for the next year (12 months)
holt_winters_forecast <- forecast(holt_winters_model, h = 12)

# Convert the data to a data frame for ggplot2
train_df <- data.frame(Date = time(train_data), Close = as.numeric(train_data))
forecast_df <- data.frame(Date = time(holt_winters_forecast$mean), Close = as.numeric(holt_winters_forecast$mean))
fitted_df <- data.frame(Date = time(holt_winters_model$fitted), Close = as.numeric(holt_winters_model$fitted))

# Plot the forecast using ggplot2
ggplot() +
  geom_line(data = train_df, aes(x = Date, y = Close), color = 'blue', size = 1) +
  geom_line(data = forecast_df, aes(x = Date, y = Close), color = 'red', size = 1, linetype = "dashed") +
  geom_line(data = fitted_df, aes(x = Date, y = Close), color = 'green', size = 1, linetype = "solid") +
  ggtitle("Holt-Winters Forecast") +
  xlab("Date") +
  ylab("Close Price") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(train_df$Date), max(forecast_df$Date), by = 1))

# Holt-Winters Forecasting
hw_model <- HoltWinters(adj_close_ts, seasonal = "multiplicative")
hw_forecast <- forecast(hw_model, h = 60)

# Plot the Holt-Winters forecast
plot(hw_forecast, main = "Holt-Winters Forecast")

# Auto ARIMA model
arima_model <- auto.arima(adj_close_ts, seasonal = TRUE)
arima_forecast <- forecast(arima_model, h = 60)

# Plot the ARIMA forecast
plot(arima_forecast, main = "ARIMA Forecast")

# Evaluate the model
train_end <- floor(0.8 * length(adj_close_ts))
train_data <- adj_close_ts[1:train_end]
test_data <- adj_close_ts[(train_end + 1):length(adj_close_ts)]

# Refit the ARIMA model on the training data
arima_model <- auto.arima(train_data, seasonal = TRUE)
arima_forecast <- forecast(arima_model, h = length(test_data))

# Plot the forecast
plot(arima_forecast)
lines(test_data, col = "red")

# Calculate evaluation metrics
arima_rmse <- sqrt(mean((test_data - arima_forecast$mean)^2))
arima_mae <- mean(abs(test_data - arima_forecast$mean))
arima_mape <- mean(abs((test_data - arima_forecast$mean) / test_data)) * 100
arima_r2 <- 1 - sum((test_data - arima_forecast$mean)^2) / sum((test_data - mean(test_data))^2)

print(paste("ARIMA RMSE:", arima_rmse))
print(paste("ARIMA MAE:", arima_mae))
print(paste("ARIMA MAPE:", arima_mape))
print(paste("ARIMA R-squared:", arima_r2))

# Preparing data for LSTM, Random Forest, and Decision Tree
adj_close_df <- data.frame(Date = index(adj_close), Adj_Close = as.numeric(adj_close))
adj_close_df$Lag_1 <- lag(adj_close_df$Adj_Close, 1)
adj_close_df$Lag_2 <- lag(adj_close_df$Adj_Close, 2)
adj_close_df$Lag_3 <- lag(adj_close_df$Adj_Close, 3)
adj_close_df$Lag_4 <- lag(adj_close_df$Adj_Close, 4)
adj_close_df$Lag_5 <- lag(adj_close_df$Adj_Close, 5)
# Remove NA values
adj_close_df <- na.omit(adj_close_df)

# Split the data into training and test sets
train_index <- 1:floor(0.8 * nrow(adj_close_df))
train_data <- adj_close_df[train_index, ]
test_data <- adj_close_df[-train_index, ]



# Random Forest model
library(randomForest)
rf_model <- randomForest(Adj_Close ~ Lag_1 + Lag_2 + Lag_3 + Lag_4 + Lag_5, data = train_data)
rf_predictions <- predict(rf_model, test_data)

# Evaluate the Random Forest model
rf_rmse <- sqrt(mean((test_data$Adj_Close - rf_predictions)^2))
rf_mae <- mean(abs(test_data$Adj_Close - rf_predictions))
rf_mape <- mean(abs((test_data$Adj_Close - rf_predictions) / test_data$Adj_Close)) * 100
rf_r2 <- 1 - sum((test_data$Adj_Close - rf_predictions)^2) / sum((test_data$Adj_Close - mean(test_data$Adj_Close))^2)

print(paste("Random Forest RMSE:", rf_rmse))
print(paste("Random Forest MAE:", rf_mae))
print(paste("Random Forest MAPE:", rf_mape))
print(paste("Random Forest R-squared:", rf_r2))

# Decision Tree model
library(rpart)
dt_model <- rpart(Adj_Close ~ Lag_1 + Lag_2 + Lag_3 + Lag_4 + Lag_5, data = train_data)
dt_predictions <- predict(dt_model, test_data)

# Evaluate the Decision Tree model
dt_rmse <- sqrt(mean((test_data$Adj_Close - dt_predictions)^2))
dt_mae <- mean(abs(test_data$Adj_Close - dt_predictions))
dt_mape <- mean(abs((test_data$Adj_Close - dt_predictions) / test_data$Adj_Close)) * 100
dt_r2 <- 1 - sum((test_data$Adj_Close - dt_predictions)^2) / sum((test_data$Adj_Close - mean(test_data$Adj_Close))^2)

print(paste("Decision Tree RMSE:", dt_rmse))
print(paste("Decision Tree MAE:", dt_mae))
print(paste("Decision Tree MAPE:", dt_mape))
print(paste("Decision Tree R-squared:", dt_r2))

