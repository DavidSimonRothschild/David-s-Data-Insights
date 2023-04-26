# 2006
top_25_jobs_M_06 <- GRW06 %>%
  filter(G== "M") %>%
  filter(!is.na(Beruf)) %>%
  count(Beruf, sort = TRUE) %>%
  head(25)


# 2010
top_25_jobs_M_10 <- GRW10 %>%
  filter(G== "M") %>%
  filter(!is.na(Beruf)) %>%
  count(Beruf, sort = TRUE) %>%
  head(25)


# 2014
top_25_jobs_M_14 <- GRW14 %>%
  filter(G== "M") %>%
  filter(!is.na(Beruf)) %>%
  count(Beruf, sort = TRUE) %>%
  head(25)

# 2018

top_25_jobs_M_18 <- GRW18 %>%
  filter(G== "M") %>%
  filter(!is.na(Beruf)) %>%
  count(Beruf, sort = TRUE) %>%
  head(25)

# 2022

top_25_jobs_M_22 <- GRW22 %>%
  filter(G== "M") %>%
  filter(!is.na(Beruf)) %>%
  count(Beruf, sort = TRUE) %>%
  head(25)


library(ggplot2)

job_plot06 <- ggplot(top_25_jobs_M_06 , aes(x = reorder(Beruf, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 25 Berufe Kandidaten (2006)",
    x = "Job",
    y = "Number of Politicians"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


job_plot10 <- ggplot(top_25_jobs_M_10 , aes(x = reorder(Beruf, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 25 Berufe Kandidaten (2010)",
    x = "Job",
    y = "Number of Politicians"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


job_plot14 <- ggplot(top_25_jobs_M_14 , aes(x = reorder(Beruf, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 25 Berufe Kandidaten (2014)",
    x = "Job",
    y = "Number of Politicians"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


job_plot18 <- ggplot(top_25_jobs_M_18 , aes(x = reorder(Beruf, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 25 Berufe Kandidaten (2018)",
    x = "Job",
    y = "Number of Politicians"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


job_plot22 <- ggplot(top_25_jobs_M_22 , aes(x = reorder(Beruf, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 25 Berufe Kandidaten (2022)",
    x = "Job",
    y = "Number of Politicians"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

gridExtra::grid.arrange(job_plot06, job_plot10, job_plot14, job_plot18,job_plot22)