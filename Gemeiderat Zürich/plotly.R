# Nicht veröffentlicht

install.packages("plotly")
library(plotly)

sc22 <- ggplot(data = GRW22, mapping = aes(x = ListeKurzbez, y = Alter, color = ListeKurzbez)) +
  geom_point(alpha = 0.6, size = 3, position = position_jitter(width = 0.3, height = 0)) +
  geom_hline(yintercept = 46, col = "red", linetype = "dashed", size = 1.5) +
  geom_hline(yintercept = 18, col = "green", linetype = "solid", size = 1.5) +
  geom_hline(yintercept = 64, col = "blue", linetype = "solid", size = 1.5) +
  scale_color_manual(values = farben) +
  labs(title = "Scatterplot of Age by ListeKurzbez",
       x = "ListeKurzbez",
       y = "Age",
       color = "ListeKurzbez") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 12),
        legend.position = "bottom") +
  coord_cartesian(ylim = c(18, 85))

sc22_plotly <- ggplotly(sc22)
sc22_plotly
