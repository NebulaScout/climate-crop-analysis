install.packages(c("dplyr", "tidyr"))
library(dplyr)
library(tidyr)
library(readr)

# Load the data from the files
temp_data <- read_csv("era5-x0.25_timeseries_tas_timeseries_annual_1950-2023_mean_historical_era5_x0.25_mean.csv")
rain_data <- read_csv("era5-x0.25_timeseries_pr_timeseries_annual_1950-2023_mean_historical_era5_x0.25_mean.csv")

glimpse(temp_data)
glimpse(rain_data)

# Clean the data
temp_data <- temp_data %>% select(-code)
rain_data <- rain_data %>% select(-code)

names(temp_data) <- sub("-.*", "", names(temp_data))
names(rain_data) <- sub("-.*", "", names(rain_data))

glimpse(temp_data)
glimpse(rain_data)

temp_data <- temp_data %>%
  rename(
    Country = name
  )

rain_data <- rain_data %>%
  rename(
    Country = name
  )

country_map <- c(
  "Kenya" = "Kenya",
  "Tanzania" = "United Republic of Tanzania",
  "Zambia" = "Zambia",
  "Uganda" = "Uganda"
)

temp_data$Country <- country_map[temp_data$Country]
rain_data$Country <- country_map[rain_data$Country]

# convert datasets from wode to long format
temp_data <- temp_data %>%
  pivot_longer(
    cols = -Country,
    names_to = "Year",
    values_to = "Temperature"
  ) %>%
  mutate(Year = as.numeric(Year))

rain_data = rain_data %>%
  pivot_longer(
    cols = -Country,
    names_to = "Year",
    values_to = "Rainfall"
  ) %>%
  mutate(Year = as.numeric(Year))

glimpse(maize_clean)
crop_data <- maize_clean %>% select(-Yield_Smoothed)
glimpse(crop_data)




head(temp_data)
head(rain_data)
head(crop_data)

# merge the climate data with the crop data
merged_df <- crop_data %>%
  left_join(temp_data, by = c("Country", "Year"))

# merge rainfall data into the dataset
merged_df <- merged_df %>%
  left_join(rain_data, by = c("Country", "Year"))

merged_df <- merged_df %>%
  filter(Year >= 1990)

head(merged_df)
colSums(is.na(merged_df))

library(ggplot2)

# Temperature vs Year
ggplot(merged_df, aes(x = Year, y = Temperature, colour = Country)) +
  geom_line(linewidth = 1) +
  labs(title = "Temperature Trend Over Time",
       y = "Average Temperature (°C)", 
       x = "Year")

# Rainfall vs Year
ggplot(merged_df, aes(x = Year, y = Rainfall, colour = Country)) + 
  geom_line(linewidth = 1) +
  labs(title = "Rainfall Trend Over Time",
       y = "Total Rainfall (mm)",
       x = "Year")

# Yield vs Year
ggplot(merged_df, aes(x = Year, y = Yield, colour = Country)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Maize Yield Trend Over Time",
    y = "Yiled (kg/ha)",
    x = "Year"
  )

# Temperature vs Yield
ggplot(merged_df, aes(x = Temperature, y = Yield, colour = Country)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relationship Between Temperature and Yield",
    x = "Average Temperature (°C)",
    y = "Yield (kg/ha)"
  )

# Rainfall vs Yield
ggplot(merged_df, aes(x = Rainfall, y = Yield, colour = Country)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relationship Between Rainfall and Yield",
    x = "Total Rainfall (mm)",
    y = "Yield (kg/ha)"
  )

# Correlation
merged_df_cor <- merged_df %>%
  group_by(Country) %>%
  summarise(
    temp_yield_cor = cor(Temperature, Yield, use = "complete.obs"),
    rain_yield_cor = cor(Rainfall, Yield, use = "complete.obs")
  )
merged_df_cor
