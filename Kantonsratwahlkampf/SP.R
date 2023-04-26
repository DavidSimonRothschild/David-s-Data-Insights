# SP
library(lubridate)
library(tidyverse)


SP_ZH_23 <- subset(SP_ZH, ad_delivery_stop_time > as.Date("2023-01-01"))


SP_ZH_23$spend

spending <- c(lower_bound: 0, upper_bound: 99 = "bis 99")

library(data.table)

dt <- data.table(SP_ZH_23)



ggplot(SP_ZH_23, aes(x=spend, colour=factor(impressions)))+geom_bar()+theme_minimal()

library(plotly)

range(SP_ZH_23$spend)

range(SP_ZH_23$impressions)


FBIN_SP <- ggplot(SP_ZH_23, aes(x=publisher_platforms))+geom_bar()+scale_fill_manual(values = c("facebook" = "blue", "instagram"  = "red", "facebook,instagram" = "purple")) +theme_minimal()


library(quanteda)
library(quanteda.textplots)

sp_c <- corpus(SP_ZH_23$ad_creative_bodies)

sp_c <- sp_c %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_remove(stopwords("german")) %>%
  tokens_wordstem()

sp_d <- dfm(sp_c)

wcl_sp <- textplot_wordcloud(sp_d, color = "red")

