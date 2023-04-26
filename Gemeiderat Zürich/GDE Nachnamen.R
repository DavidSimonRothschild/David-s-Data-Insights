
# 2006

corpus06N <- corpus(GRW06$Nachname)
N_token <- tokens(corpus06N)
N_dfm <- dfm(N_token)
topN <- head(sort(colSums(N_dfm[, !grepl("[()]", gsub("\\b(de|von)\\b", "", colnames(N_dfm)))]), decreasing = TRUE), 15)





## Nachnammen

# Convert the topN data into a data frame
topN_df <- data.frame(Nachname = names(topN), Frequency = topN)

# Create a  bar plot
n06 <- ggplot(data = topN_df, aes(x = reorder(Nachname, Frequency), y = Frequency)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Top 15 Nachnammen (2006)", x = "Nachnames", y = "Frequency") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, max(topN_df$Frequency), by = 1))
n06






# Convert the topN data into a data frame
topN_df <- data.frame(Nachname = names(topN), Frequency = topN)

# Create a  bar plot
n10 <- ggplot(data = topN_df, aes(x = reorder(Nachname, Frequency), y = Frequency)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Top 15 Nachnamen (2010)", x = "Nachnames", y = "Frequency") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, max(topN_df$Frequency), by = 1))
n10


## 2014

corpus14N <- corpus(GRW14$Nachname)
N_token <- tokens(corpus14N)
N_dfm <- dfm(N_token)
topN <- head(sort(colSums(N_dfm[, !grepl("[()]", gsub("\\b(de|von)\\b", "", colnames(N_dfm)))]), decreasing = TRUE), 15)

# Convert the topN data into a data frame
topN_df <- data.frame(Nachname = names(topN), Frequency = topN)

# Create a  bar plot
n14 <- ggplot(data = topN_df, aes(x = reorder(Nachname, Frequency), y = Frequency)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Top 15 Nachnamen (2014)", x = "Nachnames", y = "Frequency") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, max(topN_df$Frequency), by = 1))
n14


# 2018

corpus18N <- corpus(GRW18$Nachname)
N_token <- tokens(corpus18N)
N_dfm <- dfm(N_token)
topN <- head(sort(colSums(N_dfm[, !grepl("[()]", gsub("\\b(de|von)\\b", "", colnames(N_dfm)))]), decreasing = TRUE), 15)


# Convert the topN data into a data frame
topN_df <- data.frame(Nachname = names(topN), Frequency = topN)

# Create a  bar plot
n18 <- ggplot(data = topN_df, aes(x = reorder(Nachname, Frequency), y = Frequency)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Top 15 Nachnamen (2018)", x = "Nachnames", y = "Frequency") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, max(topN_df$Frequency), by = 1))
n18



## 2022
corpus22N <- corpus(GRW22$Nachname)
N_token <- tokens(corpus22N)
N_dfm <- dfm(N_token)
topN <- head(sort(colSums(N_dfm[, !grepl("[()]", gsub("\\b(de|von)\\b", "", colnames(N_dfm)))]), decreasing = TRUE), 15)


# Convert the topN data into a data frame
topN_df <- data.frame(Nachname = names(topN), Frequency = topN)


# Create a  bar plot
n22 <- ggplot(data = topN_df, aes(x = reorder(Nachname, Frequency), y = Frequency)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Top 15 Nachnamen (2022)", x = "Nachnames", y = "Frequency") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, max(topN_df$Frequency), by = 1))
n22


grid.arrange(n06, n10, n14, n18,n22)





