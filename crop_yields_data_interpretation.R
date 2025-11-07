library(dplyr)
library(readr)
library(tidyr)

# Load the data
data <- read_csv("FAOSTAT_11-5-2025_Crop_Yield_data.csv")

glimpse(data)


# Data cleaning and transformation
# Remove unecesary columns
maize_clean <- data %>%
  select(Area, Item, Year, Unit, Value)

glimpse(maize_clean)

maize_clean <- maize_clean%>%
  rename(
    Country = Area,
    Crop = Item,
    Yield = Value,
    Yield_Unit = Unit
  )

glimpse(maize_clean)

maize_clean$Year <- as.numeric(maize_clean$Year)
maize_clean$Yield <- as.numeric(maize_clean$Yield)

glimpse(maize_clean)

maize_clean <- maize_clean %>%
  filter(Crop == "Maize (corn)")

selected_countries <- c("Kenya", "United Republic of Tanzania", "Uganda", "Zambia")
maize_clean <- maize_clean %>%
  filter(Country %in% selected_countries)

glimpse(maize_clean)

# EDA

summary(maize_clean)

colSums(is.na(maize_clean)) # Check for null values

# compare average yield across countries
library(dplyr)

avg_yield <- maize_clean %>%
  group_by(Country) %>%
  summarise(Average_Yield = mean(Yield))

avg_yield

# plot the yield trends over time
library(ggplot2)

ggplot(maize_clean, aes(x = Year,y = Yield, colour = Country, group = Country)) +
  geom_line(linewidth = 1.2) +
  geom_point() +
  labs(
    title = "Maize Yield Trends (1990 - 2023)",
    x = "Year",
    y = "Yield (kg/ha)"
  ) +
  theme_minimal()


# Best and worst years per country
best_worst <- maize_clean %>%
  group_by(Country) %>%
  summarise(
    Highest_Yield = max(Yield),
    Year_of_Highest = Year[which.max(Yield)],
    Lowest_Yield = min(Yield),
    Year_of_Lowest = Year[which.min(Yield)]
  )

best_worst

install.packages("pacman")
pacman::p_load(zoo)

library(zoo)

maize_clean <- maize_clean %>%
  group_by(Country) %>%
  arrange(Year) %>%
  mutate(Yield_Smoothed = rollmean(Yield, k = 3, fill = NA))

ggplot(maize_clean, aes(x = Year, y = Yield_Smoothed, colour = Country)) +
  geom_line(size = 1.3) +
  labs(
    title = "Smoothed Maize Yield Trends (3-Year Moving Average)",
    x = "Year",
    y = "kg/ha"
  ) +
  theme_minimal()


