####################################################

# 2006
top_25_Vornames_W_06 <- GRW06 %>%
  filter(G=="W") %>%
  filter(!is.na(Vorname)) %>%
  count(Vorname, sort = TRUE) %>%
  head(25)


# 2010
top_25_Vorname_W_10 <- GRW10 %>%
  filter(G== "W") %>%
  filter(!is.na(Vorname)) %>%
  count(Vorname, sort = TRUE) %>%
  head(25)


# 2014
top_25_Vorname_W_14 <- GRW14 %>%
  filter(G== "W") %>%
  filter(!is.na(Vorname)) %>%
  count(Vorname, sort = TRUE) %>%
  head(25)

# 2018

top_25_Vorname_W_18 <- GRW18 %>%
  filter(G== "W") %>%
  filter(!is.na(Vorname)) %>%
  count(Vorname, sort = TRUE) %>%
  head(25)

# 2022

top_25_Vorname_W_22 <- GRW22 %>%
  filter(G== "W") %>%
  filter(!is.na(Vorname)) %>%
  count(Vorname, sort = TRUE) %>%
  head(25)


Vorname_plot06 <- ggplot(top_25_Vornames_W_06 , aes(x = reorder(Vorname, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkviolet") +
  coord_flip() +
  labs(
    title = "Top 25 Vornamee Kandidatinnen (2006)",
    x = "Vorname",
    y = "Number of Politicians"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0, 10)


Vorname_plot10 <- ggplot(top_25_Vorname_W_10 , aes(x = reorder(Vorname, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkviolet") +
  coord_flip() +
  labs(
    title = "Top 25 Vornamee Kandidatinnen (2010)",
    x = "Vorname",
    y = "Number of Politicians"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0, 10)


Vorname_plot14 <- ggplot(top_25_Vorname_W_14 , aes(x = reorder(Vorname, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkviolet") +
  coord_flip() +
  labs(
    title = "Top 25 Vornamee Kandidatinnen (2014)",
    x = "Vorname",
    y = "Number of Politicians"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0, 10)


Vorname_plot18 <- ggplot(top_25_Vorname_W_18 , aes(x = reorder(Vorname, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkviolet") +
  coord_flip() +
  labs(
    title = "Top 25 Vornamee Kandidatinnen (2018)",
    x = "Vorname",
    y = "Number of Politicians"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0, 10)


Vorname_plot22 <- ggplot(top_25_Vorname_W_22 , aes(x = reorder(Vorname, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkviolet") +
  coord_flip() +
  labs(
    title = "Top 25 Vornamee Kandidatinnen (2018)",
    x = "Vorname",
    y = "Number of Politicians"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0, 10)


gridExtra::grid.arrange(Vorname_plot06, Vorname_plot10, Vorname_plot14, Vorname_plot18,Vorname_plot22)
