

library(tidyverse)
glimpse(FB_ZH)

library(janitor)
ZH_name <- clean_names(FB_ZH)

ZH_numb <- as.factor(ZH_name$amount_spent_chf)

ZH_numb <- as.factor(ZH_name$number_of_ads_in_library)

data.frame(ZH_numb)


ggplot(data = ZH_name, aes(y= amount,x=number_of_ads_in_library, color=number_of_ads_in_library))+geom_col()+theme_minimal()+scale_color_gradient(low = "blue", high = "red")+labs(x = "Anzahl Anzeigen", y = "Ausgaben")


library(gt)

gt_df <- ZH_name %>%
 select(page_name,amount_spent_chf, number_of_ads_in_library) %>%
  filter(amount > 1999) %>%
  gt () %>%
  tab_header(
    title = "More then 1 999") 

#gtsave(gt_df, filename = "my_table.html")
 
library(webshot2)
gt_df %>% gtsave("tab_1.png", expand = 10)


library(tidyverse)


more_then_999 <- filter(ZH_name, amount > 999)
  
more_then_99 <- filter(ZH_name, amount > 99)

ggplot(more_then_999, aes(y=amount_spent_chf, x=page_name))+geom_col()+scale_fill_manual(breaks = "1000","5000")




       