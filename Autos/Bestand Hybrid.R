# Bestand Hybridantrieb [%]
KANTON_ZUERICH_606 <- read_csv("https://www.web.statistik.zh.ch/ogd/data/KANTON_ZUERICH_606.csv")






# Create a vector of entries to be removed
entries_to_remove <- c("Bezirk Affoltern", "Bezirk Andelfingen", "Bezirk Bülach", "Bezirk Dielsdorf",
                       "Bezirk Dietikon", "Bezirk Hinwil", "Bezirk Horgen", "Bezirk Meilen",
                       "Bezirk Pfäffikon", "Bezirk Uster", "Bezirk Winterthur", "Bezirk Zürich",
                       "Region Furttal", "Region Glattal", "Region Knonaueramt", "Region Limmattal",
                       "Region Oberland", "Region Pfannenstiel", "Region Unterland", "Region Weinland",
                       "Region Winterthur u. Umg.", "Region Zimmerberg", "Region Zürich",
                       "Zürich - ganzer Kanton")



filtered_df_Hybrid <- KANTON_ZUERICH_606[!(KANTON_ZUERICH_606$GEBIET_NAME %in% entries_to_remove), ]
#KANTON_ZUERICH_606 <- filtered_df_Hybrid



# Assuming your dataset is named "my_data" and you have a variable named "Year" containing the year information

# Create a list to store the subsets
subset_list <- list()

# Loop through the years from 2002 to 2022
for (year in 2002:2022) {
  # Filter the data for the current year
  subset <- filtered_df_Hybrid %>%
    filter(INDIKATOR_JAHR == year)
  
  # Store the subset in the list
  subset_list[[as.character(year)]] <- subset
}

subset_2002 <- subset_list[["2002"]]
subset_2022<- subset_list[["2022"]]




summary_data_Hybrid <- filtered_df_Hybrid %>%
  group_by(INDIKATOR_JAHR) %>%
  summarize(min_value = min(INDIKATOR_VALUE),
            max_value = max(INDIKATOR_VALUE),
            min_gebiet = GEBIET_NAME[which.min(INDIKATOR_VALUE)],
            max_gebiet = GEBIET_NAME[which.max(INDIKATOR_VALUE)])


#View(summary_data)

summary_table <- summary_data %>%
  gt()


line_plot_Hybrid <- ggplot(summary_data_Hybrid, aes(x = INDIKATOR_JAHR)) +
  geom_line(aes(y = min_value, color = "Minimum")) +
  geom_line(aes(y = max_value, color = "Maximum")) +
  labs(x = "Jahr", y = " PKW Bestand [%]", color = "Value") +  # Adjust axis labels
  ggtitle("Entwicklung Hybrid")+
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(limits = c(2002, 2022))

# Display the line plot
line_plot_Hybrid



# Loop ab hier
count_zero_entries <- c()

# Loop through the years from 2002 to 2022
for (INDIKATOR_JAHR in 2002:2022) {
  # Filter the data for the current year
  subset <- filtered_df_Hybrid[filtered_df_Hybrid$INDIKATOR_JAHR == INDIKATOR_JAHR, ]
  
  # Count the number of entries with value 0.0 in INDIKATOR_VALUE for the current year
  count <- sum(subset$INDIKATOR_VALUE == 0.0)
  
  # Append the count to the vector
  count_zero_entries <- c(count_zero_entries, count)
  
  # Break the loop if the current year is 2022
  if (INDIKATOR_JAHR == 2022) {
    break
  }
}

for (i in 1:length(count_zero_entries)) {
  year <- 2002 + i - 1
  if (year > 2022) {
    break
  }
  cat("Year", year, ": Count =", count_zero_entries[i], "\n")
}



# Initialize an empty vector to store the counts of non-zero entries for each year
count_non_zero_entries <- c()

for (INDIKATOR_JAHR in 2002:2022) {
  # Filter the data for the current year
  subset <- filtered_df_Hybrid[filtered_df_Hybrid$INDIKATOR_JAHR == INDIKATOR_JAHR, ]
  
  # Count the number of entries not equal to 0.0 in INDIKATOR_VALUE for the current year
  count_non_zero <- sum(subset$INDIKATOR_VALUE != 0.0)
  
  # Append the count to the vector
  count_non_zero_entries <- c(count_non_zero_entries, count_non_zero)
  
  # Break the loop if the current year is 2022
  if (INDIKATOR_JAHR == 2022) {
    break
  }
}

# Print the counts of non-zero entries for each year
for (i in 1:length(count_non_zero_entries)) {
  year <- 2002 + i - 1
  if (year > 2022) {
    break
  }
  cat("Year", year, ": Count =", count_non_zero_entries[i], "\n")
}


# Assuming you have the counts of zero entries and non-zero entries for each year as vectors:
# count_zero_entries and count_non_zero_entries

# Years from 2002 to 2022
years <- 2002:2022







# Create a bar plot

# Assuming you have the counts of zero entries and non-zero entries for each year as vectors:
# count_zero_entries and count_non_zero_entries

# Years from 2002 to 2022
years <- 2002:2022

# Create a data frame with counts and corresponding years
data <- data.frame(
  Year = years,
  Zero_Entries = count_zero_entries,
  Non_Zero_Entries = count_non_zero_entries
)



# Convert data to long format for easier plotting
data_long <- tidyr::gather(data, Category, Count, -Year)

# Plot the bar plot
ggplot(data_long, aes(x = as.factor(Year), y = Count, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  labs(
    x = "Jahr",
    y = "ANzahl Gemeinden",
    title = "Entwicklung über die Jahre",
    fill = "Category"
  ) +
  scale_fill_manual(values = c("#008080", "orange")) +
  theme_minimal()+
  annotate(
    geom = "curve", x = 4, y = 35, xend = 6, yend = 17, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = 4.1, y = 35, label = "Rückgang", hjust = "left")



