library(rvest)
library(dplyr)

# URL of the webpage to scrape
url <- "https://www.stadt-zuerich.ch/ssd/de/index/sport/schwimmen/wassertemperaturen.html"

# Function to scrape data
scrape_data <- function() {
  # Read the HTML content from the URL
  webpage <- read_html(url)
  
  # Extract the relevant temperature data for "Anlage und Gäste"
  data <- webpage %>%
    html_nodes(xpath = '//*[@id="content"]/div[2]/div[2]/div[2]/div[2]/table') %>%
    html_table()
  
  # Convert the extracted data to a data frame
  df <- data[[1]]
  
  # Add a timestamp column
  df$timestamp <- Sys.time()
  
  # Save the data to a CSV file
  write.table(df, file = "wassertemperaturen.csv", sep = ",", row.names = FALSE, col.names = !file.exists("wassertemperaturen.csv"), append = TRUE)
}

# Scrape data
scrape_data()
