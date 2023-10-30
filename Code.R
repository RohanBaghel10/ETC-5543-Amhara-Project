This File contains all the code that was used in Forecasting the various Family planning medicines that are being used in Ethiopia.
The main focus is on "Amoxicillin - 125mg - Tablet (Dispersible)".

software:
  - name: R
version: 4.3.0
packages:
  - name: bookdown
version: 0.35
- name: tidyverse
version: 2.0.0
- name: lubridate
version: 1.9.2
- name: forecast
version: 8.21  
- name: fable
version: 0.3.3
- name: fpp3
version: 0.5
- name: fabletools
version:  0.3.3
- name: kableExtra
version: 1.3.4


# Read the CSV file and create a data frame
data <- read_csv(here::here("data/famillyplanning_maternal_child_health_products.csv"))

# Convert the 'month' column to Date format
data$Date <- as.Date(paste0(data$month, "01"), format = "%Y %b %d")

# Create a tsibble data frame
tsibble_data <- data %>%
  as_tsibble(index = Date, key = c(region, item)) %>%
  mutate(Date = yearmonth(Date)) %>%
  select(Date, region, item, total_issued)

# Filter data for the 'Amoxicillin - 125mg - Tablet (Dispersible)' item
data_Amoxicillin_125 <- tsibble_data %>% 
  filter(item == "Amoxicillin - 125mg - Tablet (Dispersible)")

# Filter data for the previous year
data_forecast <- tsibble_data %>% filter(year(Date) < 2022 | (year(Date) == 2022 & month(Date) <= "May"))

# Create forecasting models
models_forecast <- data_forecast %>% 
  model(
    ARIMA = ARIMA(total_issued),
    ETS = ETS(total_issued),
    naive = NAIVE(total_issued)
  )

# Forecasting
forecast_horizon <- models_forecast %>% 
  forecast(h = "1 years")

# Ensure non-negative forecasts
forecast_pos_only <- forecast_horizon %>%
  mutate(.mean = ifelse(.mean < 0, 0, .mean))

# Cross-validation results

# Stretch the tsibble for cross-validation
stretched_tsdata_amox125 <- stretch_tsibble(data_Amoxicillin_125, .init = 1, .step = 1)

# Perform cross-validation and create forecasts
cv_results_Amox125 <- stretched_tsdata_amox125 %>%
  model(
    ARIMA = ARIMA(total_issued),
    ETS = ETS(total_issued),
    naive = NAIVE(total_issued)
  ) %>%
  forecast(h = "1 years") %>%
  fabletools::accuracy(tsibble_data)

# Calculate Winkler Scores
winkler_scores <- forecast_pos_only %>% 
  fabletools::accuracy(tsibble_data, list(winkler = winkler_score), level = 80)

# Plot the initial data for 'Amoxicillin-125mg'
data_Amoxicillin_125 %>% 
  autoplot() +
  facet_wrap(~ region, scales = "free") +
  theme(legend.position = "none") +
  labs(x = "Year", y = "Total Issued", title = "Initial Data Analysis for Amoxicillin-125mg") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
        
 # Plot the ETS forecasts for 'Amoxicillin-125mg'
forecast_pos_only %>% 
  filter(item == "Amoxicillin - 125mg - Tablet (Dispersible)") %>%
  filter(.model == c("ETS")) %>% 
  autoplot(tsibble_data %>% filter(item == "Amoxicillin - 125mg - Tablet (Dispersible)")) +
  facet_wrap(~ region, scales = "free") +
  labs(x = "Year", y = "Total Issued", title = "Forecasting for Amoxicillin-125mg using ETS") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
        

# Plot the ARIMA forecasts for 'Amoxicillin-125mg'
forecast_pos_only %>% 
  filter(item == "Amoxicillin - 125mg - Tablet (Dispersible)") %>%
  filter(.model == c("ARIMA")) %>% 
  autoplot(tsibble_data %>% filter(item == "Amoxicillin - 125mg - Tablet (Dispersible)")) +
  facet_wrap(~ region, scales = "free") +
  labs(x = "Year", y = "Total Issued", title = "Forecasting for Amoxicillin-125mg using ARIMA") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
        
# Plot the NAIVE forecasts for 'Amoxicillin-125mg'
forecast_pos_only %>% 
  filter(item == "Amoxicillin - 125mg - Tablet (Dispersible)") %>%
  filter(.model == c("naive")) %>% 
  autoplot(tsibble_data %>% filter(item == "Amoxicillin - 125mg - Tablet (Dispersible)")) +
  facet_wrap(~ region, scales = "free") +
  labs(x = "Year", y = "Total Issued", title = "Forecasting for Amoxicillin-125mg using NAIVE method") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
                                