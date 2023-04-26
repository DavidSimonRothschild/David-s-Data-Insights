####################################################
library(tidyverse)

# 2006
top_25_Vornames_M_06 <- GRW06 %>%
  filter(G== "M") %>%
  filter(!is.na(Vorname)) %>%
  count(Vorname, sort = TRUE) %>%
  head(25)


# 2010
top_25_Vorname_M_10 <- GRW10 %>%
  filter(G== "M") %>%
  filter(!is.na(Vorname)) %>%
  count(Vorname, sort = TRUE) %>%
  head(25)


# 2014
top_25_Vorname_M_14 <- GRW14 %>%
  filter(G== "M") %>%
  filter(!is.na(Vorname)) %>%
  count(Vorname, sort = TRUE) %>%
  head(25)

# 2018

top_25_Vorname_M_18 <- GRW18 %>%
  filter(G== "M") %>%
  filter(!is.na(Vorname)) %>%
  count(Vorname, sort = TRUE) %>%
  head(25)

# 2022

top_25_Vorname_M_22 <- GRW22 %>%
  filter(G== "M") %>%
  filter(!is.na(Vorname)) %>%
  count(Vorname, sort = TRUE) %>%
  head(25)


Vorname_plot06 <- ggplot(top_25_Vornames_M_06 , aes(x = reorder(Vorname, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 25 Vorname Kandidaten (2006)",
    x = "Vorname",
    y = "Number of Politicians"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0, 30)


Vorname_plot10 <- ggplot(top_25_Vorname_M_10 , aes(x = reorder(Vorname, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 25 Vorname Kandidaten (2010)",
    x = "Vorname",
    y = "Number of Politicians"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0, 30)


Vorname_plot14 <- ggplot(top_25_Vorname_M_14 , aes(x = reorder(Vorname, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 25 Vorname Kandidaten (2014)",
    x = "Vorname",
    y = "Number of Politicians"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0, 30)


Vorname_plot18 <- ggplot(top_25_Vorname_M_18 , aes(x = reorder(Vorname, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 25 Vorname Kandidaten (2018)",
    x = "Vorname",
    y = "Number of Politicians"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0, 30)


Vorname_plot22 <- ggplot(top_25_Vorname_M_22 , aes(x = reorder(Vorname, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 25 Vorname Kandidaten (2022)",
    x = "Vorname",
    y = "Number of Politicians"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0, 30)

gridExtra::grid.arrange(Vorname_plot06, Vorname_plot10, Vorname_plot14, Vorname_plot18,Vorname_plot22)
