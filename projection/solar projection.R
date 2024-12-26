install.packages(c("tidyverse", "lubridate", "forecast", "data.table", "fable", "caret", "ggplot2", "shiny", "randomForest", "xgboost"))

library(tidyverse)  # For data manipulation and visualization
library(lubridate)  # For dealing with dates
library(forecast)   # For time series forecasting
library(data.table) # For fast data manipulation
library(fable)      # For time series analysis and forecasting
library(caret)      # For machine learning
library(ggplot2)    # For data visualization
library(shiny)      # For interactive web applications
library(randomForest) # For random forest models
library(xgboost)    # For gradient boosting
library(sf)
library(spdep) #key function used for conducting spatial autocorrelation
library(dplyr)
library(tmap)
library(terra)
library(extrafont)
library(ggplot2)
library(ggspatial)
library(patchwork)
library(scico)
library(tidyverse)
library(sp)
library(raster)
library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(knitr)    # For knitting document and include_graphics function
library(ggplot2)  # For plotting
library(png) 


#working solar  electricity demand

energy_cons <- read.csv("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/projection/Solar PV electricity generation - Indonesia IEA.csv" )
head(energy_cons)


plot(energy_cons$year,energy_cons$generation)


# Fitting an ARIMA model to the entire filtered dataset
fit <- auto.arima(energy_cons$generation)

# Forecasting from the last year in the dataset to 2030
last_year_in_data <- max(energy_cons$year)
years_to_forecast <- 2030 - last_year_in_data
forecasted_values_2030 <- forecast(fit, h = years_to_forecast)

indo_demands_filtered <- energy_cons
tail(indo_demands_filtered)

# Creating a combined data frame for actual and forecasted values
forecast_years <- seq(last_year_in_data + 1, 2030)
forecasted_data <- data.frame(Year = forecast_years, electricity_demands_TWh = forecasted_values_2030$mean)
# Assuming indo_demands_filtered and forecasted_data are already defined

str(indo_demands_filtered)
str(forecasted_data)

combined_data <- bind_rows(indo_demands_filtered, forecasted_data)
tail(combined_data)
plot(combined_data$Year,combined_data$electricity_demands_TWh)

# Plotting the data
ggplot(combined_data, aes(x = Year, y = electricity_demands_TWh)) + 
  geom_line() +
  geom_point(data = filter(combined_data, Year > last_year_in_data), color = "red") +
  xlab("Year") + 
  ylab("Electricity Demand") +
  ggtitle("Actual and Forecasted Electricity Demand in Indonesia (1990 - 2030)")

forecasted_demand_2030 <- tail(forecasted_values_2030$mean, 1)
forecasted_demand_2030
energy_cons
print(paste("solar electricity demand for 2022 is 444GWh", "Predicted solar electricity demand for 2030:", forecasted_demand_2030,"GWh"))




#test and train

set.seed(123)  # For reproducibility
training_indices <- sample(2:nrow(energy_cons), 0.7 * nrow(energy_cons))
train_data <- energy_cons[training_indices, ]
test_data <- energy_cons[-training_indices, ]
summary(train_data)

# Fitting an ARIMA model
fit1 <- auto.arima(train_data$generation)
fit2 <- auto.arima(test_data$generation)

# Forecasting
forecasted_values_train <- forecast(fit1, h = nrow(train_data))
plot(forecasted_values_train)
forecasted_values_test <- forecast(fit2, h = nrow(test_data))
plot(forecasted_values_test)
lines(test_data$years, test_data$generation, col = 'red')

# Calculating accuracy
accuracy(forecasted_values_train, test_data$generation)

accuracy(forecasted_values_test, test_data$generation)


# Forecasting up to the year 2030
future_years <- as.Date(c(2030))
# 
# years_to_forecast <- as.numeric(difftime(future_years, max(indo_cons$Year), units = "days"))/365.25
# forecasted_values_2030 <- forecast(fit2, h = years_to_forecast)

years_to_forecast <- as.numeric(difftime(2030, max(energy_cons$years)))
forecasted_values_2030 <- forecast(fit2, h = years_to_forecast)

# Plotting the forecast
plot(forecasted_values_2030)
lines(energy_cons$years, energy_cons$generation, col = 'blue')
points(future_years, forecasted_values_2030$mean, col = 'red', pch = 19)


# forecasted_demand_2030$year 
# year=data.frame(2023,2024,2025,2026,2027,2028,2029,2030)
# forecasted_demand_2030$year <-year
# 
forecasted_demand_2030<- (forecasted_values_2030$mean)
# forecasted_demand_2030
indo_demands_filtered1<-energy_cons

combined_data <- bind_rows(indo_demands_filtered1, forecasted_data)
tail(combined_data)
plot(combined_data$Year,combined_data$electricity_demands_TWh)

last_year_in_data <- max(indo_cons$Year)
last_year_in_data
# 
predicted_combined_data<-filter(combined_data, Year > last_year_in_data)

ggplot() +
  geom_line(data = combined_data, aes(x = Year, y = electricity_demands_TWh), color = 'blue') +
  geom_point(data = predicted_combined_data ,aes(x = Year, y = electricity_demands_TWh), color = "red") +
  labs(x = "Year", y = "Electricity Demand (TWh)", title = "Electricity Demand Forecast") +
  theme_minimal()

# Printing the forecast for 2030
forecasted_demand_2030 <- tail(forecasted_values_2030$mean, 1)

print(paste("electricity demand for 2022",tail(indo_cons$electricity_demands_TWh), "Predicted electricity demand for 2030:", forecasted_demand_2030,"TWh"))







#working 

energy_cons <- read.csv("F:/Masters/Term 1/BENV0093 Spatial Analysis of Energy Data/assignments/spatial assignment 2/assignment 2/projection/Solar PV electricity generation - Indonesia IEA.csv" )
head(energy_cons)


plot(energy_cons$year,energy_cons$generation)


# Fitting an ARIMA model to the entire filtered dataset
fit <- auto.arima(energy_cons$generation)

# Forecasting from the last year in the dataset to 2030
last_year_in_data <- max(energy_cons$year)
years_to_forecast <- 2030 - last_year_in_data
forecasted_values_2030 <- forecast(fit, h = years_to_forecast)

indo_demands_filtered <- energy_cons
tail(indo_demands_filtered)

# Creating a combined data frame for actual and forecasted values
forecast_years <- seq(last_year_in_data + 1, 2030)
forecasted_data <- data.frame(Year = forecast_years, electricity_demands_TWh = forecasted_values_2030$mean)
# Assuming indo_demands_filtered and forecasted_data are already defined

str(indo_demands_filtered)
str(forecasted_data)

combined_data <- bind_rows(indo_demands_filtered, forecasted_data)
tail(combined_data)
plot(combined_data$Year,combined_data$electricity_demands_TWh)

# Plotting the data
ggplot(combined_data, aes(x = Year, y = electricity_demands_TWh)) + 
  geom_line() +
  geom_point(data = filter(combined_data, Year > last_year_in_data), color = "red") +
  xlab("Year") + 
  ylab("Electricity Demand") +
  ggtitle("Actual and Forecasted Electricity Demand in Indonesia (1990 - 2030)")

forecasted_demand_2030 <- tail(forecasted_values_2030$mean, 1)
forecasted_demand_2030
energy_cons
print(paste("solar electricity demand for 2022 is 444GWh", "Predicted solar electricity demand for 2030:", forecasted_demand_2030,"GWh"))















