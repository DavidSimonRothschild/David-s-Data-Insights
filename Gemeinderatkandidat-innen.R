GRW06 <- read_csv("https://data.stadt-zuerich.ch/dataset/politik-gemeinderatswahlen-2006-alle-kandidierenden/download/GRW-2006-alle-Kandidierenden-OGD.csv")
GRW10 <- read_csv("https://data.stadt-zuerich.ch/dataset/politik_gemeinderatswahlen_2010_kandidierende/download/gemeinderatswahlen_2010_kandidierende.csv")
GRW14 <- read_csv("https://data.stadt-zuerich.ch/dataset/politik-gemeinderatswahlen-2014-alle-kandidierenden/download/GRW-2014-alle-Kandidierenden-OGD.csv")
GRW18 <- read_csv("https://data.stadt-zuerich.ch/dataset/politik_gemeinderatswahlen_2018_kandidierende/download/gemeinderatswahlen_2018_kandidierende.csv")
GRW22 <- read_csv("https://data.stadt-zuerich.ch/dataset/politik_gemeinderatswahlen_2022_kandidierende/download/gemeinderatswahlen_2022_kandidierende.csv")



library(ggplot2)
library(tidyverse)
library(gridExtra)
library(janitor)

# Partei Liste

farben<- c("Grüne" = "#84B547", "EVP" = "#DEAA28", "CVP" = "#D6862B", "SVP"= "#4B8A3E", "AL"= "#BF3939", "GLP"= "#C4C43D",
            "FDP"= "#3872B5", "SD"= "#228B22", "SP"="#F0554D" ,"HP"= "grey", "EDU"= "#A65F42", "SL"="grey" , "Danowski"= "grey", "Unabhängige"= "grey",
           "PdA"= "#BF3939", "BDP"= "#E6C820", "GLP"="#C4C43D", "Die Mitte" ="orange", "Volt"="darkviolet")


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

# library(gridExtra)
# grid.arrange(plot06, plot10, plot14, plot18,plot22)


# Alter

GRW06$Alter <- 2005 - GRW06$GebJ
GRW10$Alter <- 2009 - GRW10$GebJ
GRW14$Alter <- 2013 - GRW14$GebJ
GRW18$Alter <- 2017 - GRW18$GebJ
GRW22$Alter <- 2021 - GRW22$GebJ


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
mean_alter <- mean(GRW06$Alter, na.rm = TRUE)
median_alter <- median(GRW06$Alter, na.rm = TRUE)

# Create the bar plot and add vertical lines at the mean and median values
ggplot(GRW06, mapping = aes(x = Alter)) +
  geom_bar() +
  geom_vline(xintercept = median_alter, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = mean_alter, linetype = "dashed", color = "blue", size = 1) +
  scale_x_continuous(breaks = seq(18, 90, by = 5), limits = c(18, 90)) +
  scale_y_continuous(breaks = seq(0, 40, by = 5), limits = c(0, 50)) +
  labs(title = "Age Distribution", x = "Age", y = "Frequency") +
  theme_minimal()

ggplot(GRW10, mapping = aes(x = Alter)) +
  geom_bar() +
  geom_vline(xintercept = median_alter, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = mean_alter, linetype = "dashed", color = "blue", size = 1) +
  scale_x_continuous(breaks = seq(18, 90, by = 5), limits = c(18, 90)) +
  scale_y_continuous(breaks = seq(0, 40, by = 5), limits = c(0, 50)) +
  labs(title = "Age Distribution", x = "Age", y = "Frequency") +
  theme_minimal()

ggplot(GRW14, mapping = aes(x = Alter)) +
  geom_bar() +
  geom_vline(xintercept = median_alter, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = mean_alter, linetype = "dashed", color = "blue", size = 1) +
  scale_x_continuous(breaks = seq(18, 90, by = 5), limits = c(18, 90)) +
  scale_y_continuous(breaks = seq(0, 40, by = 5), limits = c(0, 50)) +
  labs(title = "Age Distribution", x = "Age", y = "Frequency") +
  theme_minimal()

ggplot(GRW18, mapping = aes(x = Alter)) +
  geom_bar() +
  geom_vline(xintercept = median_alter, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = mean_alter, linetype = "dashed", color = "blue", size = 1) +
  scale_x_continuous(breaks = seq(18, 90, by = 5), limits = c(18, 90)) +
  scale_y_continuous(breaks = seq(0, 40, by = 5), limits = c(0, 50)) +
  labs(title = "Age Distribution", x = "Age", y = "Frequency") +
  theme_minimal()

ggplot(GRW22, mapping = aes(x = Alter)) +
  geom_bar() +
  geom_vline(xintercept = median_alter, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = mean_alter, linetype = "dashed", color = "blue", size = 1) +
  scale_x_continuous(breaks = seq(18, 90, by = 5), limits = c(18, 90)) +
  scale_y_continuous(breaks = seq(0, 40, by = 5), limits = c(0, 50)) +
  labs(title = "Age Distribution", x = "Age", y = "Frequency") +
  theme_minimal()

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



library(quanteda)

corpus06N <- corpus(GRW06$Nachname)
N_token <- tokens(corpus06N)
N_dfm <- dfm(N_token)
topN <- head(sort(colSums(N_dfm), decreasing = TRUE), 10)

barplot(topN, main = "Top 10 most frequent words", xlab = "Word", ylab = "Frequency")

corpus06V <- corpus(GRW06$Vorname)
V_token <- tokens(corpus06V)
V_dfm <- dfm(V_token)
topV <- head(sort(colSums(V_dfm), decreasing = TRUE), 10)

barplot(topV, main = "Top 10 most frequent words", xlab = "Word", ylab = "Frequency")

corpus06B <- corpus(GRW06$Beruf)
B_token <- tokens(corpus06B)
B_token <- tokens_remove(B_token, "eth")
B_token <- tokens_remove(B_token, pattern = "\\.")
B_dfm <- dfm(B_token)
topB <- head(sort(colSums(B_dfm), decreasing = TRUE), 35)

barplot(topB, main = "Top 10 most frequent words", xlab = "Word", ylab = "Frequency")
