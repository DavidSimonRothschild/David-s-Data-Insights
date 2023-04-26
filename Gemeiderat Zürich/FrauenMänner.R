

GRW06_summary <- GRW06 %>%
  group_by(ListeKurzbez, G) %>%
  summarise(Anzahl = n()) %>%
  ungroup()

mf06 <- ggplot(GRW06_summary, aes(x = ListeKurzbez, y = Anzahl, fill = G)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Anzahl der Frauen und Männer pro Partei (2006)",
       x = "Partei",
       y = "Anzahl",
       fill = "Geschlecht") +
  theme_minimal()+
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1),
                        plot.title = element_text(face = "bold", size = 14),
                        axis.title = element_text(size = 12),
                        legend.position = "bottom")


GRW10_summary <- GRW10 %>%
  group_by(ListeKurzbez, G) %>%
  summarise(Anzahl = n()) %>%
  ungroup()

mf10 <- ggplot(GRW10_summary, aes(x = ListeKurzbez, y = Anzahl, fill = G)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Anzahl der Frauen und Männer pro Partei(2010) ",
       x = "Partei",
       y = "Anzahl",
       fill = "Geschlecht") +
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1),
                        plot.title = element_text(face = "bold", size = 14),
                        axis.title = element_text(size = 12),
                        legend.position = "bottom")


GRW14_summary <- GRW14 %>%
  group_by(ListeKurzbez, G) %>%
  summarise(Anzahl = n()) %>%
  ungroup()

mf14 <- ggplot(GRW14_summary, aes(x = ListeKurzbez, y = Anzahl, fill = G)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Anzahl der Frauen und Männer pro Partei (2014)",
       x = "Partei",
       y = "Anzahl",
       fill = "Geschlecht") +
  theme_minimal()+
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1),
                        plot.title = element_text(face = "bold", size = 14),
                        axis.title = element_text(size = 12),
                        legend.position = "bottom")

GRW18_summary <- GRW18 %>%
  group_by(ListeKurzbez, G) %>%
  summarise(Anzahl = n()) %>%
  ungroup()

mf18 <- ggplot(GRW18_summary, aes(x = ListeKurzbez, y = Anzahl, fill = G)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Anzahl der Frauen und Männer pro Partei (2018)",
       x = "Partei",
       y = "Anzahl",
       fill = "Geschlecht") +
  theme_minimal()+
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1),
                        plot.title = element_text(face = "bold", size = 14),
                        axis.title = element_text(size = 12),
                        legend.position = "bottom")

GRW22_summary <- GRW22 %>%
  group_by(ListeKurzbez, G) %>%
  summarise(Anzahl = n()) %>%
  ungroup()

mf22 <- ggplot(GRW22_summary, aes(x = ListeKurzbez, y = Anzahl, fill = G)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Anzahl der Frauen und Männer pro Partei (2022)",
       x = "Partei",
       y = "Anzahl",
       fill = "Geschlecht") +
  theme_minimal()+
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1),
                        plot.title = element_text(face = "bold", size = 14),
                        axis.title = element_text(size = 12),
                        legend.position = "bottom")

library(gridExtra)
grid.arrange(mf06, mf10, mf14, mf18, mf22, ncol = 2)


