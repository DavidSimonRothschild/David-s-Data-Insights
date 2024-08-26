library(ggplot2)
library(dplyr)
library(scales)

# Assuming MF_AG is your data frame and has 'year' and 'sum' columns.

# Filter the data for the specific years
filtered_data <- MF_AG %>%
  filter(year %in% c(1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020, 2023))

# Create the bar plot with rotated x-axis labels and non-scientific y-axis
ggplot(filtered_data, aes(x = factor(year), y = sum)) +
  geom_bar(stat = "identity", fill = "#004494", color = "black") + # Blue bars with black borders
  labs(title = "Töffli 1970 bis 2023 im \nKanton Aargau", x = "Jahr", y = "Anzahl") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = comma)


