



## 2006

corpus06N <- corpus(GRW06$Vorname)
N_token <- tokens(corpus06N)
N_dfm <- dfm(N_token)
topN <- head(sort(colSums(N_dfm[, !grepl("[()]", colnames(N_dfm))]), decreasing = TRUE), 15)





# Convert the topN data into a data frame
topN_df <- data.frame(Surname = names(topN), Frequency = topN)

# Create a  bar plot
n06 <- ggplot(data = topN_df, aes(x = reorder(Surname, Frequency), y = Frequency)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Top 15 Surnames", x = "Surnames", y = "Frequency") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, max(topN_df$Frequency), by = 1))
n06

## 2010

corpus10N <- corpus(GRW10$Vorname)
N_token <- tokens(corpus10N)
N_dfm <- dfm(N_token)
topN <- head(sort(colSums(N_dfm[, !grepl("[()]", colnames(N_dfm))]), decreasing = TRUE), 15)





# Convert the topN data into a data frame
topN_df <- data.frame(Surname = names(topN), Frequency = topN)

# Create a  bar plot
n10 <- ggplot(data = topN_df, aes(x = reorder(Surname, Frequency), y = Frequency)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Top 15 Surnames", x = "Surnames", y = "Frequency") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, max(topN_df$Frequency), by = 1))
n10


## 2014

corpus14N <- corpus(GRW14$Vorname)
N_token <- tokens(corpus14N)
N_dfm <- dfm(N_token)
topN <- head(sort(colSums(N_dfm[, !grepl("[()]", colnames(N_dfm))]), decreasing = TRUE), 15)




# Convert the topN data into a data frame
topN_df <- data.frame(Surname = names(topN), Frequency = topN)

# Create a  bar plot
n14 <- ggplot(data = topN_df, aes(x = reorder(Surname, Frequency), y = Frequency)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Top 15 Surnames", x = "Surnames", y = "Frequency") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, max(topN_df$Frequency), by = 1))
n14


## 2018

## 2018

corpus18N <- corpus(GRW18$Vorname)
N_token <- tokens(corpus18N)
N_dfm <- dfm(N_token)
topN <- head(sort(colSums(N_dfm[, !grepl("[()]", colnames(N_dfm))]), decreasing = TRUE), 15)




# Convert the topN data into a data frame
topN_df <- data.frame(Surname = names(topN), Frequency = topN)

# Create a  bar plot
n18 <- ggplot(data = topN_df, aes(x = reorder(Surname, Frequency), y = Frequency)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Top 15 Surnames", x = "Surnames", y = "Frequency") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, max(topN_df$Frequency), by = 1))
n18


## 2022

## 2022

corpus22N <- corpus(GRW22$Vorname)
N_token <- tokens(corpus22N)
N_dfm <- dfm(N_token)
topN <- head(sort(colSums(N_dfm[, !grepl("[()]", colnames(N_dfm))]), decreasing = TRUE), 15)





# Convert the topN data into a data frame
topN_df <- data.frame(Surname = names(topN), Frequency = topN)

# Create a  bar plot
n22 <- ggplot(data = topN_df, aes(x = reorder(Surname, Frequency), y = Frequency)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Top 15 Surnames", x = "Surnames", y = "Frequency") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, max(topN_df$Frequency), by = 1))
n22


gridExtra:grid.arrange(n06, n10, n14, n18,n22)





