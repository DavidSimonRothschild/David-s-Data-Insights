# H P
library(tidyverse)

ggplot(H_P, aes(x=spend, colour=factor(impressions)))+geom_bar()+theme_minimal()

library(plotly)

range(H_P$spend)

range(H_P$impressions)


FBIN_HP <- ggplot(H_P, aes(x=publisher_platforms))+geom_bar()+scale_fill_manual(values = c("facebook" = "blue", "instagram"  = "red", "facebook,instagram" = "purple")) +theme_minimal()


library(quanteda)
library(quanteda.textplots)

hp_c <- corpus(H_P$ad_creative_bodies)

hp_c <- hp_c %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_remove(stopwords("german")) %>%
  tokens_wordstem()

hp_d <- dfm(hp_c)

wcl_hp <- textplot_wordcloud(hp_d, color = "blue")

