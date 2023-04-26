
mean_age_by_gender_and_party06 <- GRW06 %>%
  group_by(ListeKurzbez, G) %>%
  summarise(mean_age = mean(Alter, na.rm = TRUE))

# Print the results
print(mean_age_by_gender_and_party)




# 06
mean_age_bar_plot06 <- ggplot(mean_age_by_gender_and_party06, aes(x = ListeKurzbez, y = mean_age, fill = G)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 43, color = "red", linetype = "dashed", size = 1)+
  labs(
    title = "Durchschnittsalter nach Partei und Geschlecht \n (Wahlen 2006)",
    x = "Partei",
    y = "Durchschnittsalter"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("M" = "#5DADE2", "W" = "#EC7063"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the bar plot
print(mean_age_bar_plot06)

### 2010


mean_age_by_gender_and_party10 <- GRW10 %>%
  group_by(ListeKurzbez, G) %>%
  summarise(mean_age = mean(Alter, na.rm = TRUE))

# Print the results
print(mean_age_by_gender_and_party)





mean_age_bar_plot10 <- ggplot(mean_age_by_gender_and_party10, aes(x = ListeKurzbez, y = mean_age, fill = G)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 45, color = "red", linetype = "dashed", size = 1)+
  labs(
    title = "Durchschnittsalter nach Partei und Geschlecht \n (Wahlen 2010)",
    x = "Partei",
    y = "Durchschnittsalter"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("M" = "#5DADE2", "W" = "#EC7063"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the bar plot
print(mean_age_bar_plot10)



### 2014


mean_age_by_gender_and_party14 <- GRW10 %>%
  group_by(ListeKurzbez, G) %>%
  summarise(mean_age = mean(Alter, na.rm = TRUE))

# Print the results
print(mean_age_by_gender_and_party14)





mean_age_bar_plot14 <- ggplot(mean_age_by_gender_and_party14, aes(x = ListeKurzbez, y = mean_age, fill = G)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 45, color = "red", linetype = "dashed", size = 1)+
  labs(
    title = "Durchschnittsalter nach Partei und Geschlecht \n (Wahlen 2014)",
    x = "Partei",
    y = "Durchschnittsalter"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("M" = "#5DADE2", "W" = "#EC7063"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the bar plot
print(mean_age_bar_plot14)


### 2018

library(tidyverse)
mean_age_by_gender_and_party18 <- GRW18 %>%
  group_by(ListeKurzbez, G) %>%
  summarise(mean_age = mean(Alter, na.rm = TRUE))

# Print the results
print(mean_age_by_gender_and_party18)





mean_age_bar_plot18 <- ggplot(mean_age_by_gender_and_party18, aes(x = ListeKurzbez, y = mean_age, fill = G)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 46, color = "red", linetype = "dashed", size = 1)+
  labs(
    title = "Durchschnittsalter nach Partei und Geschlecht \n (Wahlen 2018)",
    x = "Partei",
    y = "Durchschnittsalter"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("M" = "#5DADE2", "W" = "#EC7063"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the bar plot
print(mean_age_bar_plot18)


### 2022

library(tidyverse)
mean_age_by_gender_and_party22 <- GRW22 %>%
  group_by(ListeKurzbez, G) %>%
  summarise(mean_age = mean(Alter, na.rm = TRUE))

# Print the results
print(mean_age_by_gender_and_party22)





mean_age_bar_plot22 <- ggplot(mean_age_by_gender_and_party22, aes(x = ListeKurzbez, y = mean_age, fill = G)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 46, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Durchschnittsalter nach Partei und Geschlecht \n (Wahlen 2022)",
    x = "Partei",
    y = "Durchschnittsalter"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("M" = "#5DADE2", "W" = "#EC7063")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the bar plot
print(mean_age_bar_plot22)


# Print the bar plot
print(mean_age_bar_plot22)


# Load the required package
library(gridExtra)

# Combine the four ggplot2 plots into a grid
combined_plots <- grid.arrange(mean_age_bar_plot06, mean_age_bar_plot10, mean_age_bar_plot14, mean_age_bar_plot18,mean_age_bar_plot22, ncol = 2, nrow = 3)

# Print the combined plots
print(combined_plots)


