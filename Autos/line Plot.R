Andere<- read_csv("https://www.web.statistik.zh.ch/ogd/data/KANTON_ZUERICH_613.csv")
filtered_Andere <- subset(Andere, GEBIET_NAME == "Zürich - ganzer Kanton")

View(filtered_Andere)

library(ggplot2)

# Assuming your dataset is named "filtered_Andere"

# Create the plot
ggplot(data = filtered_Andere, mapping = aes(x = INDIKATOR_JAHR, y = INDIKATOR_VALUE)) +
  geom_line(color="blue") +
  xlim(2002, 2022) +  # Set the x-axis limits
  labs(x = "Jahr", y = "[%]", title = "Anzahl alternative Antriebe\n bei Neuzulassungen im Kanton Zürich")+
  theme_minimal()+
  geom_label(aes(x = 2002, y = 1.0, label = "Maximum \n 2007"), 
             hjust = 0, 
             vjust = 0.5, 
             colour = "#555555", 
             fill = "transparent", 
             label.size = NA, 
             family="Helvetica", 
             size = 4)+
  geom_curve(aes(x = 2002, y = 1.1, xend = 2006.5, yend = 1.2), 
                             colour = "#555555", 
                             size=0.3, 
                             curvature = -0.2,
                             arrow = arrow(length = unit(0.03, "npc")))

