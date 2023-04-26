GRW06 <- read_csv("https://data.stadt-zuerich.ch/dataset/politik-gemeinderatswahlen-2006-alle-kandidierenden/download/GRW-2006-alle-Kandidierenden-OGD.csv")
GRW10 <- read_csv("https://data.stadt-zuerich.ch/dataset/politik_gemeinderatswahlen_2010_kandidierende/download/gemeinderatswahlen_2010_kandidierende.csv")
GRW14 <- read_csv("https://data.stadt-zuerich.ch/dataset/politik-gemeinderatswahlen-2014-alle-kandidierenden/download/GRW-2014-alle-Kandidierenden-OGD.csv")
GRW18 <- read_csv("https://data.stadt-zuerich.ch/dataset/politik_gemeinderatswahlen_2018_kandidierende/download/gemeinderatswahlen_2018_kandidierende.csv")
GRW22 <- read_csv("https://data.stadt-zuerich.ch/dataset/politik_gemeinderatswahlen_2022_kandidierende/download/gemeinderatswahlen_2022_kandidierende.csv")



library(ggplot2)
library(tidyverse)
library(gridExtra)
library(janitor)
library(quanteda)

# Partei Liste

farben<- c("Grüne" = "#84B547", "EVP" = "#DEAA28", "CVP" = "#D6862B", "SVP"= "#4B8A3E", "AL"= "#BF3939", "GLP"= "#C4C43D",
            "FDP"= "#3872B5", "SD"= "#228B22", "SP"="#F0554D" ,"HP"= "grey", "EDU"= "#A65F42", "SL"="grey" , "Danowski"= "grey", "Unabhängige"= "grey",
           "PdA"= "#BF3939", "BDP"= "#E6C820", "glp"="#C4C43D", "Die Mitte" ="orange", "EVP"= "D6862B")


# Create your ggplot2 bar chart
# p06 <- ggplot(data = GRW06, aes(x = ListeKurzbez)) +
#   geom_bar(fill = "white", color = "black") +
#   scale_fill_manual(values = farben)


# 2006
plot06 <- ggplot(data = GRW06, aes(x = ListeKurzbez, fill = ListeKurzbez)) + 
  geom_bar(width = 0.7) +
  scale_fill_manual(values = farben) +
  scale_y_continuous(limits = c(0, 125), breaks = seq(0, 125, by = 10))+ theme_minimal()+  
  geom_hline(yintercept = 125, color = "red")+ geom_hline(yintercept = 125, color = "red") +
  geom_text(aes(x = 7.5, y = 127, label = "Maximal 125 Kandidat:innen"),
            color = "red", size = 5, hjust = 1)+
  geom_hline(yintercept = 125, color = "red") +
  xlab("Parteien, Wahlen 2006")+
  ylab("Anzahl Kandidat:innen")

# 2010
plot10 <- ggplot(data = GRW10, aes(x = ListeKurzbez, fill = ListeKurzbez)) + 
  geom_bar(width = 0.7) +
  scale_fill_manual(values = farben) +
  scale_y_continuous(limits = c(0, 125), breaks = seq(0, 125, by = 10))+ theme_minimal()+  
  geom_hline(yintercept = 125, color = "red")+ geom_hline(yintercept = 125, color = "red") +
  geom_text(aes(x = 7.5, y = 127, label = "Maximal 125 Kandidat:innen"),
            color = "red", size = 5, hjust = 1)+
  geom_hline(yintercept = 125, color = "red") +
  xlab("Parteien, Wahlen 2010")+
  ylab("Anzahl Kandidat:innen")

# 2014
plot14 <- ggplot(data = GRW14, aes(x = ListeKurzbez, fill = ListeKurzbez)) + 
  geom_bar(width = 0.7) +
  scale_fill_manual(values = farben) +
  scale_y_continuous(limits = c(0, 125), breaks = seq(0, 125, by = 10))+ theme_minimal()+  
  geom_hline(yintercept = 125, color = "red")+ geom_hline(yintercept = 125, color = "red") +
  geom_text(aes(x = 7.5, y = 127, label = "Maximal 125 Kandidat:innen"),
            color = "red", size = 5, hjust = 1)+
  geom_hline(yintercept = 125, color = "red") +
  xlab("Parteien, Wahlen 2014")+
  ylab("Anzahl Kandidat:innen")


# 2018
plot18 <- ggplot(data = GRW18, aes(x = ListeKurzbez, fill = ListeKurzbez)) + 
  geom_bar(width = 0.7) +
  scale_fill_manual(values = farben) +
  scale_y_continuous(limits = c(0, 125), breaks = seq(0, 125, by = 10))+ theme_minimal()+  
  geom_hline(yintercept = 125, color = "red")+ geom_hline(yintercept = 125, color = "red") +
  geom_text(aes(x = 7.5, y = 127, label = "Maximal 125 Kandidat:innen"),
            color = "red", size = 5, hjust = 1)+
  geom_hline(yintercept = 125, color = "red") +
  xlab("Parteien, Wahlen 2018")+
  ylab("Anzahl Kandidat:innen")

#2022
plot22 <- ggplot(data = GRW22, aes(x = ListeKurzbez, fill = ListeKurzbez)) + 
  geom_bar(width = 0.7) +
  scale_fill_manual(values = farben) +
  scale_y_continuous(limits = c(0, 125), breaks = seq(0, 125, by = 10)) + 
  theme_minimal() +  
  geom_hline(yintercept = 125, color = "red") + 
  geom_text(aes(x = 7.5, y = 127, label = "Maximal 125 Kandidat:innen"),
            color = "red", size = 5, hjust = 1) +
  geom_hline(yintercept = 125, color = "red") +
  xlab("Parteien, Wahlen 2022") +
  ylab("Anzahl Kandidat:innen")

library(gridExtra)
grid.arrange(plot06, plot10, plot14, plot18,plot22)


# Alter

alter_06 <- na.omit(GRW06$Alter <- 2005 - GRW06$GebJ)
alter_10 <- GRW10$Alter <- 2009 - GRW10$GebJ
alter_14 <- GRW14$Alter <- 2013 - GRW14$GebJ
alter_18 <- GRW18$Alter <- 2017 - GRW18$GebJ
alter_22 <- GRW22$Alter <- 2021 - GRW22$GebJ


# Calculate mean and median age for each group
mean_age <- c(mean(alter_06), mean(alter_10), mean(alter_14), mean(alter_18), mean(alter_22))
median_age <- c(median(alter_06), median(alter_10), median(alter_14), median(alter_18), median(alter_22))

# Create a table with the results
age_table <- data.frame(
  Age_Group = c("06", "10", "14", "18", "22"),
  Mean_Age = mean_age,
  Median_Age = median_age
)

library(gt)
library(dplyr)

# Create data frame with mean and median ages
age_df <- data.frame(
  Age_Group = c("06", "10", "14", "18", "22"),
  Mean_Age = mean_age,
  Median_Age = median_age
)

# Rename columns
age_df <- age_df %>%
  rename(Jahr = Age_Group, Durchschnitt = Mean_Age, Median = Median_Age)

# Create gt table
age_table <- age_df %>%
  gt() %>%
  tab_header(title = "Durchschnitt und Median pro Wahljahr")

age_table




ggplot(GRW06, mapping = aes(x= Alter))+geom_bar()

# Calculate the mean and median
mean_alter <- mean(GRW06$Alter, na.rm = TRUE)
median_alter <- median(GRW06$Alter, na.rm = TRUE)

# Create the bar plot and add vertical lines at the mean and median values
ggplot(GRW06, mapping = aes(x = Alter)) +
  geom_bar() +
  geom_vline(xintercept = median_alter, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = mean_alter, linetype = "dashed", color = "blue", size = 1) +
  labs(title = "Age Distribution", x = "Age", y = "Frequency") +
  theme_minimal()

# Calculate the mean and median
mean_alter <- mean(GRW22$Alter, na.rm = TRUE)
median_alter <- median(GRW06$Alter, na.rm = TRUE)

# Create the bar plot and add vertical lines at the mean and median values
alter06 <- ggplot(GRW06, mapping = aes(x = Alter)) +
  geom_bar() +
  geom_vline(xintercept = median_alter, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = mean_alter, linetype = "dashed", color = "blue", size = 1) +
  scale_x_continuous(breaks = seq(18, 90, by = 5), limits = c(18, 90)) +
  scale_y_continuous(breaks = seq(0, 40, by = 5), limits = c(0, 50)) +
  labs(title = "Altersvereteilung 2006", x = "Age", y = "Frequency") +
  theme_minimal()

alter10 <- ggplot(GRW10, mapping = aes(x = Alter)) +
  geom_bar() +
  geom_vline(xintercept = median_alter, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = mean_alter, linetype = "dashed", color = "blue", size = 1) +
  scale_x_continuous(breaks = seq(18, 90, by = 5), limits = c(18, 90)) +
  scale_y_continuous(breaks = seq(0, 40, by = 5), limits = c(0, 50)) +
  labs(title = "Altersvereteilung 2010", x = "Age", y = "Frequency") +
  theme_minimal()

alter14 <- ggplot(GRW14, mapping = aes(x = Alter)) +
  geom_bar() +
  geom_vline(xintercept = median_alter, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = mean_alter, linetype = "dashed", color = "blue", size = 1) +
  scale_x_continuous(breaks = seq(18, 90, by = 5), limits = c(18, 90)) +
  scale_y_continuous(breaks = seq(0, 40, by = 5), limits = c(0, 50)) +
  labs(title = "Altersvereteilung 2014", x = "Age", y = "Frequency") +
  theme_minimal()

alter18 <- ggplot(GRW18, mapping = aes(x = Alter)) +
  geom_bar() +
  geom_vline(xintercept = median_alter, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = mean_alter, linetype = "dashed", color = "blue", size = 1) +
  scale_x_continuous(breaks = seq(18, 90, by = 5), limits = c(18, 90)) +
  scale_y_continuous(breaks = seq(0, 40, by = 5), limits = c(0, 50)) +
  labs(title = "Altersvereteilung 2018", x = "Age", y = "Frequency") +
  theme_minimal()

alter22 <- ggplot(GRW22, mapping = aes(x = Alter)) +
  geom_bar() +
  geom_vline(xintercept = median_alter, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = mean_alter, linetype = "dashed", color = "blue", size = 1) +
  scale_x_continuous(breaks = seq(18, 90, by = 5), limits = c(18, 90)) +
  scale_y_continuous(breaks = seq(0, 40, by = 5), limits = c(0, 50)) +
  labs(title = "Altersvereteilung 2022", x = "Age", y = "Frequency") +
  theme_minimal()

grid.arrange(alter06, alter10, alter14, alter18,alter22)

# Gender

GRW06 <- GRW06 %>% mutate(Year = "GRW06")
GRW10 <- GRW10 %>% mutate(Year = "GRW10")
GRW14 <- GRW14 %>% mutate(Year = "GRW14")
GRW18 <- GRW18 %>% mutate(Year = "GRW18")
GRW22 <- GRW22 %>% mutate(Year = "GRW22")

combined_df <- bind_rows(GRW06, GRW10, GRW14, GRW18, GRW22)

tabulation <- combined_df %>% 
  count(Year, G) %>%
  spread(Year, n, fill = 0) %>%
  adorn_totals("row")

print(tabulation)


long_format <- combined_df %>%
  count(Year, G) %>%
  mutate(Year = factor(Year), G = factor(G))

ggplot(long_format, aes(x = Year, y = n, fill = G)) +
  geom_col() +
  labs(title = "Combined Table", x = "Year", y = "Frequency", fill = "G") +
  theme_minimal()

long_format_percent <- long_format %>%
  group_by(Year) %>%
  mutate(total = sum(n),
         percent = n / total * 100)
ggplot(long_format_percent, aes(x = Year, y = n, fill = G)) +
  geom_col() +
  geom_text(aes(label = paste0(round(percent, 1), "%")), position = position_stack(vjust = 0.5), size = 3.5) +
  labs(title = "Combined Table", x = "Year", y = "Frequency", fill = "G") +
  theme_minimal()

ggplot(long_format_percent, aes(x = Year, y = n, color = G, group = G)) +
  geom_line() +
  geom_point() +
  labs(title = "Combined Table", x = "Year", y = "Frequency", color = "G") +
  theme_minimal()



library(quanteda)

corpus06N <- corpus(GRW06$Nachname)
N_token <- tokens(corpus06N)
N_dfm <- dfm(N_token)
topN <- head(sort(colSums(N_dfm), decreasing = TRUE), 20)

# Load required libraries
library(ggplot2)

# Convert the topN data into a data frame
topN_df <- data.frame(Surname = names(topN), Frequency = topN)

# Create a colorful bar plot
ggplot(data = topN_df, aes(x = reorder(Surname, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = rainbow(length(topN_df$Surname))) +
  theme_minimal() +
  labs(title = "Top 15 Surnames", x = "Surnames", y = "Frequency") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5))






# Load required libraries
library(ggplot2)

# Convert the topN data into a data frame
topV_df <- data.frame(Surname = names(topV), Frequency = topV)

# Create a colorful bar plot
ggplot(data = topV_df, aes(x = reorder(Surname, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = rainbow(length(topN_df$Surname))) +
  theme_minimal() +
  labs(title = "Top 15 Surnames", x = "Surnames", y = "Frequency") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5))


#barplot(topN, main = "Top 10 most frequent words", xlab = "Word", ylab = "Frequency")


W_data <- subset(GRW06, G == "W")

corpus06V <- corpus(W_data)
V_token <- tokens(W_data)
V_dfm <- dfm(W_data)
topV <- head(sort(colSums(V_dfm), decreasing = TRUE), 20)

# Load the ggplot2 package
library(ggplot2)

# Calculate the top 20 most frequent words
topV <- head(sort(colSums(V_dfm), decreasing = TRUE), 20)


# Abschnitt zu bisherige


### Boxplot zu Alter nach candierende

# Adding custom colors
custom_colors <- farben

ggplot(data = GRW06, mapping = aes(x = ListeKurzbez, y = Alter, fill = ListeKurzbez)) +
  geom_point(outlier.color = "black", outlier.shape = 16) +
  geom_hline(yintercept = 46, col = "red", linetype = "dashed", size = 1.5) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Boxplot of Age by ListeKurzbez",
       x = "ListeKurzbez",
       y = "Age",
       fill = "ListeKurzbez") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 12),
        legend.position = "bottom")


custom_colors <- farben

ggplot(data = GRW10, mapping = aes(x = ListeKurzbez, y = Alter, fill = ListeKurzbez)) +
  geom_boxplot(outlier.color = "black", outlier.shape = 16) +
  geom_hline(yintercept = 46, col = "red", linetype = "dashed", size = 1.5) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Boxplot of Age by ListeKurzbez",
       x = "ListeKurzbez",
       y = "Age",
       fill = "ListeKurzbez") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 12),
        legend.position = "bottom")

custom_colors <- farben

ggplot(data = GRW14, mapping = aes(x = ListeKurzbez, y = Alter, fill = ListeKurzbez)) +
  geom_boxplot(outlier.color = "black", outlier.shape = 16) +
  geom_hline(yintercept = 46, col = "red", linetype = "dashed", size = 1.5) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Boxplot of Age by ListeKurzbez",
       x = "ListeKurzbez",
       y = "Age",
       fill = "ListeKurzbez") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 12),
        legend.position = "bottom")

custom_colors <- farben

ggplot(data = GRW18, mapping = aes(x = ListeKurzbez, y = Alter, fill = ListeKurzbez)) +
  geom_boxplot(outlier.color = "black", outlier.shape = 16) +
  geom_hline(yintercept = 46, col = "red", linetype = "dashed", size = 1.5) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Boxplot of Age by ListeKurzbez",
       x = "ListeKurzbez",
       y = "Age",
       fill = "ListeKurzbez") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 12),
        legend.position = "bottom")

custom_colors <- farben

ggplot(data = GRW22, mapping = aes(x = ListeKurzbez, y = Alter, fill = ListeKurzbez)) +
  geom_boxplot(outlier.color = "black", outlier.shape = 16) +
  geom_hline(yintercept = 46, col = "red", linetype = "dashed", size = 1.5) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Boxplot of Age by ListeKurzbez",
       x = "ListeKurzbez",
       y = "Age",
       fill = "ListeKurzbez") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 12),
        legend.position = "bottom")


library(ggplot2)

# Adding custom colors
custom_colors <- farben

sc06 <- ggplot(data = GRW06, mapping = aes(x = ListeKurzbez, y = Alter, color = ListeKurzbez)) +
  geom_point(alpha = 0.6, size = 3, position = position_jitter(width = 0.3, height = 0)) +
  geom_hline(yintercept = 46, col = "red", linetype = "dashed", size = 1.5) +
  geom_hline(yintercept = 18, col = "green", linetype = "solid", size = 1.5) +
  geom_hline(yintercept = 64, col = "blue", linetype = "solid", size = 1.5) +
  scale_color_manual(values = farben) +
  labs(title = "Scatterplot of Age by ListeKurzbez",
       x = "ListeKurzbez",
       y = "Age",
       color = "ListeKurzbez") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 12),
        legend.position = "bottom")+
  coord_cartesian(ylim = c(18, 85))
  
print(sc06)

 ### 
  sc10 <- ggplot(data = GRW10, mapping = aes(x = ListeKurzbez, y = Alter, color = ListeKurzbez)) +
    geom_point(alpha = 0.6, size = 3, position = position_jitter(width = 0.3, height = 0)) +
    geom_hline(yintercept = 46, col = "red", linetype = "dashed", size = 1.5) +
    geom_hline(yintercept = 18, col = "green", linetype = "solid", size = 1.5) +
    geom_hline(yintercept = 64, col = "blue", linetype = "solid", size = 1.5)
  scale_color_manual(values = farben) +
    labs(title = "Scatterplot of Age by ListeKurzbez",
         x = "ListeKurzbez",
         y = "Age",
         color = "ListeKurzbez") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(face = "bold", size = 14),
          axis.title = element_text(size = 12),
          legend.position = "bottom")+
    coord_cartesian(ylim = c(18, 85))
  
print(sc10)



sc14 <- ggplot(data = GRW14, mapping = aes(x = ListeKurzbez, y = Alter, color = ListeKurzbez)) +
  geom_point(alpha = 0.6, size = 3, position = position_jitter(width = 0.3, height = 0)) +
  geom_hline(yintercept = 46, col = "red", linetype = "dashed", size = 1.5) +
  geom_hline(yintercept = 18, col = "green", linetype = "solid", size = 1.5) +
  geom_hline(yintercept = 64, col = "blue", linetype = "solid", size = 1.5)
scale_color_manual(values = farben) +
  labs(title = "Scatterplot of Age by ListeKurzbez",
       x = "ListeKurzbez",
       y = "Age",
       color = "ListeKurzbez") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 12),
        legend.position = "bottom")+
  coord_cartesian(ylim = c(18, 85))

print(sc14)


sc18 <- ggplot(data = GRW18, mapping = aes(x = ListeKurzbez, y = Alter, color = ListeKurzbez)) +
  geom_point(alpha = 0.6, size = 3, position = position_jitter(width = 0.3, height = 0)) +
  geom_hline(yintercept = 46, col = "red", linetype = "dashed", size = 1.5) +
  geom_hline(yintercept = 18, col = "green", linetype = "solid", size = 1.5) +
  geom_hline(yintercept = 64, col = "blue", linetype = "solid", size = 1.5)
scale_color_manual(values = farben) +
  labs(title = "Scatterplot of Age by ListeKurzbez",
       x = "ListeKurzbez",
       y = "Age",
       color = "ListeKurzbez") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 12),
        legend.position = "bottom")+
  coord_cartesian(ylim = c(18, 85))

plot(sc18)

sc22 <- ggplot(data = GRW22, mapping = aes(x = ListeKurzbez, y = Alter, color = ListeKurzbez)) +
  geom_point(alpha = 0.6, size = 3, position = position_jitter(width = 0.3, height = 0)) +
  geom_hline(yintercept = 46, col = "red", linetype = "dashed", size = 1.5) +
  geom_hline(yintercept = 18, col = "green", linetype = "solid", size = 1.5) +
  geom_hline(yintercept = 64, col = "blue", linetype = "solid", size = 1.5)+
scale_color_manual(values = farben) +
  labs(title = "Scatterplot of Age by ListeKurzbez",
       x = "ListeKurzbez",
       y = "Age",
       color = "ListeKurzbez") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 12),
        legend.position = "bottom")+
  coord_cartesian(ylim = c(18, 85))

plot(sc22)


