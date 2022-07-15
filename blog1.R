library(tidyverse)
library(TSstudio)
library(plotly)


# Import googel trend Data
# World search request

p <- blog_search_googel %>%
  ggplot( aes(x=Monat, y=blog)) +
  geom_step()
  
  ylab("search result") 


# Trend Data average per Year
  
average <- blog_search_googel

average_plot <- ggplot(data = average, aes(x=Year, y=Value, colour= Value))+geom_line()+theme_light()



fig <- plot_ly(average, type = 'scatter', mode = 'lines')%>%
  add_trace(x = ~Year, y = ~Value)%>%
  layout(showlegend = F)

## Country

country_trend <- dplyr:: filter(country_trend, Index >15)

country_plot <- ggplot(data = country_trend, aes(x=Index, y=Country, colour= Index, (stat = "identity")))+geom_col()+
  theme_light()+labs(x= "group")

country_plot

# Michuzi

Michuz_plot_world <- ggplot(data = michuz_world, aes(y=Value, x=Year, colour= Value, (stat = "identity")))+geom_line()+
  theme_light()+labs(x= "group")


