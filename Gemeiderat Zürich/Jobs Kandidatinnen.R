# 2006
top_25_jobs_W_06 <- GRW06 %>%
  filter(G== "W") %>%
  filter(!is.na(Beruf)) %>%
  count(Beruf, sort = TRUE) %>%
  head(25)


# 2010
top_25_jobs_W_10 <- GRW10 %>%
  filter(G== "W") %>%
  filter(!is.na(Beruf)) %>%
  count(Beruf, sort = TRUE) %>%
  head(25)


# 2014
top_25_jobs_W_14 <- GRW14 %>%
  filter(G== "W") %>%
  filter(!is.na(Beruf)) %>%
  count(Beruf, sort = TRUE) %>%
  head(25)

# 2018

top_25_jobs_W_18 <- GRW18 %>%
  filter(G== "W") %>%
  filter(!is.na(Beruf)) %>%
  count(Beruf, sort = TRUE) %>%
  head(25)

# 2022

top_25_jobs_W_22 <- GRW22 %>%
  filter(G== "W") %>%
  filter(!is.na(Beruf)) %>%
  count(Beruf, sort = TRUE) %>%
  head(25)


library(ggplot2)

job_plot06 <- ggplot(top_25_jobs_W_06 , aes(x = reorder(Beruf, n), y = n)) +
  geom_bar(stat = "identity", fill = "#330066") +
  coord_flip() +
  labs(
    title = "Top 25 Berufe Kandidatinnen (2006)",
    x = "Job",
    y = "NuWber of Politicians"
  ) +
  theme_minimal()


job_plot10 <- ggplot(top_25_jobs_W_10 , aes(x = reorder(Beruf, n), y = n)) +
  geom_bar(stat = "identity", fill = "#330066") +
  coord_flip() +
  labs(
    title = "Top 25 Berufe Kandidatinnen (2010)",
    x = "Job",
    y = "NuWber of Politicians"
  ) +
  theme_minimal() 


job_plot14 <- ggplot(top_25_jobs_W_14 , aes(x = reorder(Beruf, n), y = n)) +
  geom_bar(stat = "identity", fill = "#330066") +
  coord_flip() +
  labs(
    title = "Top 25 Berufe Kandidatinnen (2014)",
    x = "Job",
    y = "NuWber of Politicians"
  ) +
  theme_minimal()


job_plot18 <- ggplot(top_25_jobs_W_18 , aes(x = reorder(Beruf, n), y = n)) +
  geom_bar(stat = "identity", fill = "#330066") +
  coord_flip() +
  labs(
    title = "Top 25 Berufe Kandidatinnen (2018)",
    x = "Job",
    y = "Anzahl"
  ) +
  theme_minimal()


job_plot22 <- ggplot(top_25_jobs_W_22 , aes(x = reorder(Beruf, n), y = n)) +
  geom_bar(stat = "identity", fill = "#330066") +
  coord_flip() +
  labs(
    title = "Top 25 Berufe Kandidatinnen (2022)",
    x = "Job",
    y = "Anzahl"
  ) +
  theme_minimal()


gridExtra::grid.arrange(job_plot06, job_plot10, job_plot14, job_plot18,job_plot22)