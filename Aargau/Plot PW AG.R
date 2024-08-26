# Load necessary libraries
library(ggplot2)
library(dplyr)
library(scales)  # For comma formatting

# Define Canton Aargau colors
# The flag of Canton Aargau features black, white, and blue.
aargau_colors <- c("#000000", "#0055A4")  # Black and Blue

# Define the specific years of interest
years_of_interest <- c(1930, 1940, 1950, 1955, 1960, 1965, 1970, 1975, 1980,
                       1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020, 2023)

# Filter the data for the specific years
filtered_data <- PW_AG %>%
  filter(year %in% years_of_interest)

# Create the bar plot for 1930 to 2023
ggplot(filtered_data, aes(x = factor(year), y = sum, fill = factor(year))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = rep(aargau_colors, length.out = length(years_of_interest))) +
  labs(
    title = "Personenwagen 1930 bis 2023 \nim Kanton Aargau",
    x = "Jahr",
    y = "Anzahl"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_y_continuous(labels = comma)

# Filter the data for 1930 to 1960
filtered_data_1930_1960 <- filtered_data %>%
  filter(year >= 1930 & year <= 1960)

# Create the bar plot for 1930 to 1960
ggplot(filtered_data_1930_1960, aes(x = factor(year), y = sum, fill = factor(year))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = rep(aargau_colors, length.out = nrow(filtered_data_1930_1960))) +
  labs(
    title = "Personenwagen 1930 bis 1960 \nim Kanton Aargau",
    x = "Jahr",
    y = "Anzahl"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_y_continuous(labels = comma)
