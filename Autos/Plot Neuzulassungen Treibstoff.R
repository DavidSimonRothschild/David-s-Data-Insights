library(gridExtra)
library(tidyverse)
library(ggthemes)
EV <- read_csv("https://www.web.statistik.zh.ch/ogd/data/KANTON_ZUERICH_612.csv")

filtered_ev <- subset(EV, GEBIET_NAME == "Zürich - ganzer Kanton")


plot_EV <- ggplot(data = filtered_ev, aes(x = INDIKATOR_JAHR, y = INDIKATOR_VALUE)) +
  geom_col(aes(fill = INDIKATOR_VALUE), color = "black") +  # Color gradient based on INDIKATOR_VALUE
  scale_fill_gradient(low = "white", high = "#00FF00") +  # Specify color range
  ylim(0, 100) +
  theme_minimal()+
  ggtitle("EV")
plot_EV

# Benzin
Benzin <- read_csv("https://www.web.statistik.zh.ch/ogd/data/KANTON_ZUERICH_609.csv")

filtered_Benzin <- subset(Benzin, GEBIET_NAME == "Zürich - ganzer Kanton")



plot_Benzin <- ggplot(data = filtered_Benzin, aes(x = INDIKATOR_JAHR, y = INDIKATOR_VALUE)) +
  geom_col(aes(fill = INDIKATOR_VALUE), color = "black") +  # Color gradient based on INDIKATOR_VALUE
  scale_fill_gradient(low = "white", high = "#FFBF00") +  # Specify color range
  ylim(0, 100) +
  ggtitle("Benzin")+
  theme_minimal()


plot_Benzin
# Diesel

Diesel <- read_csv("https://www.web.statistik.zh.ch/ogd/data/KANTON_ZUERICH_610.csv")

filtered_Diesel <- subset(Diesel, GEBIET_NAME == "Zürich - ganzer Kanton")



library(ggplot2)

plot_Diesel <- ggplot(data = filtered_Diesel, aes(x = INDIKATOR_JAHR, y = INDIKATOR_VALUE)) +
  geom_col(aes(fill = INDIKATOR_VALUE)) + # Set fill aesthetic to the value
  scale_fill_gradient(low = "grey", high = "black") + # Gradient from grey to black
  ylim(0, 100)+theme_minimal()+
  ggtitle("Diesel")

print(plot_Diesel)


# Hybrid

Hybrid <- read_csv("https://www.web.statistik.zh.ch/ogd/data/KANTON_ZUERICH_611.csv")

filtered_Hybrid <- subset(Hybrid, GEBIET_NAME == "Zürich - ganzer Kanton")

plot_Hybrid <- ggplot(data = filtered_Hybrid, aes(x = INDIKATOR_JAHR, y = INDIKATOR_VALUE)) +
  geom_col(aes(fill = INDIKATOR_VALUE)) + # Set fill aesthetic to the value
  scale_fill_gradient(low = "lightcyan", high = "#008080") + # Gradient from lightcyan to teal
  ylim(0, 100)+
  theme_minimal()+
  ggtitle("Hybrid")

# Andere 

Andere<- read_csv("https://www.web.statistik.zh.ch/ogd/data/KANTON_ZUERICH_613.csv")
filtered_Andere <- subset(Andere, GEBIET_NAME == "Zürich - ganzer Kanton")

plot_Andere <- ggplot(data = filtered_Andere, aes(x = INDIKATOR_JAHR, y = INDIKATOR_VALUE)) +
  geom_col(aes(fill = INDIKATOR_VALUE)) + # Set fill aesthetic to the value
  scale_fill_gradient(low = "blue", high = "darkblue") + # Gradient from lightcyan to teal
  ylim(0, 100)+
  theme_minimal()+
  ggtitle("Andere")

plot_Andere

combined_plots <- grid.arrange(plot_EV,plot_Hybrid,plot_Andere, plot_Benzin, plot_Diesel,
                               ncol = 2)

library(dplyr)
library(ggplot2)

# Electric Vehicles
EV <- read_csv("https://www.web.statistik.zh.ch/ogd/data/KANTON_ZUERICH_612.csv")
filtered_ev <- subset(EV, GEBIET_NAME == "Zürich - ganzer Kanton")
filtered_ev$type <- "EV"

# Benzin
Benzin <- read_csv("https://www.web.statistik.zh.ch/ogd/data/KANTON_ZUERICH_609.csv")
filtered_Benzin <- subset(Benzin, GEBIET_NAME == "Zürich - ganzer Kanton")
filtered_Benzin$type <- "Benzin"

# Diesel
Diesel <- read_csv("https://www.web.statistik.zh.ch/ogd/data/KANTON_ZUERICH_610.csv")
filtered_Diesel <- subset(Diesel, GEBIET_NAME == "Zürich - ganzer Kanton")
filtered_Diesel$type <- "Diesel"

# Hybrid
Hybrid <- read_csv("https://www.web.statistik.zh.ch/ogd/data/KANTON_ZUERICH_611.csv")
filtered_Hybrid <- subset(Hybrid, GEBIET_NAME == "Zürich - ganzer Kanton")
filtered_Hybrid$type <- "Hybrid"

# Andere


Andere<- read_csv("https://www.web.statistik.zh.ch/ogd/data/KANTON_ZUERICH_613.csv")
filtered_Andere <- subset(Andere, GEBIET_NAME == "Zürich - ganzer Kanton")
filtered_Andere$type <- "Andere"


# Combining all datasets
all_data <- bind_rows(filtered_ev, filtered_Benzin, filtered_Diesel, filtered_Hybrid,filtered_Andere)

# Plotting the data
# Assuming you have already loaded the ggplot2 library

# Set the x-axis and y-axis limits
x_limits <- c(2002, 2022)
y_limits <- c(0, 100)

# Plot the data
# Assuming you have already loaded the ggplot2 library

# Set the x-axis and y-axis limits
x_limits <- c(2002, 2022)
y_limits <- c(0, 100)


# Plot the data
ggplot(data = all_data, aes(x = INDIKATOR_JAHR, y = INDIKATOR_VALUE, fill = type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("EV" = "#00FF00", "Benzin" = "Red", "Diesel" = "Black", "Hybrid" = "#008080", "Andere"="darkblue")) +
  scale_x_continuous(limits = c(x_limits[1], x_limits[2]), breaks = seq(x_limits[1], x_limits[2], by = 1)) +
  scale_y_continuous(limits = c(y_limits[1], y_limits[2]), breaks = seq(y_limits[1], y_limits[2], by = 10)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("[%]") + xlab("Jahr")


# Load required libraries
library(ggplot2)
library(dplyr)

# Create a function to plot the bar chart for each year
plot_bar_for_year <- function(year) {
  # Filter data for the given year
  filtered_data <- all_data %>% filter(INDIKATOR_JAHR == year)
  
  # Create the bar plot
  p <- ggplot(filtered_data, aes(x = type, y = INDIKATOR_VALUE, fill = type)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c("EV" = "#00FF00", "Benzin" = "Red", "Diesel" = "Black", "Hybrid" = "#008080", "Andere"="darkblue")) +
    labs(title = paste("Number of Vehicles by Type - Year", year),
         x = "Vehicle Type",
         y = "[%]") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("[%]") +
    xlab("INDIKATOR_JAHR")
  
  # Print the plot
  print(p)
}

# Loop through each year from 2002 to 2022
for (year in 2002:2022) {
  plot_bar_for_year(year)
}


# 1. Modify the function to save each plot as a PNG

plot_bar_for_year <- function(year) {
  # Filter data for the given year
  filtered_data <- all_data %>% filter(INDIKATOR_JAHR == year)
  
  # Create the bar plot
  p <- ggplot(filtered_data, aes(x = type, y = INDIKATOR_VALUE, fill = type)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c("EV" = "#00FF00", "Benzin" = "Red", "Diesel" = "Black", "Hybrid" = "#008080", "Andere"="darkblue")) +
    labs(title = paste("Number of Vehicles by Type - Year", year),
         x = "Vehicle Type",
         y = "[%]") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Save the plot to a PNG file
  ggsave(paste0("plot_", year, ".png"), plot = p, width = 1, height = 1)
}

# 2. Loop through each year from 2002 to 2022 and save plots
for (year in 2002:2022) {
  plot_bar_for_year(year)
}

# 3. Convert the PNG files to an animated GIF using the magick package

# If you haven't installed the magick package yet, you can do so using:
# install.packages("magick")

library(magick)

# Gather all PNG files
plot_files <- list.files(pattern = "plot_.*\\.png")
# Sort them to ensure they're in the correct order
plot_files <- sort(plot_files)

# Convert to a gif
animation <- image_read(plot_files)
animation <- image_animate(animation, fps = 1) # Adjust fps (frames per second) to your liking
image_write(animation, "animated_plot.gif")

