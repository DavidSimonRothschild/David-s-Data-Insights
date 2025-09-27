library(tidyverse)
library(lme4)
library(ggrepel)

library(tidyverse)
library(lme4)
library(ggrepel)

library(readxl)

abstimmung_eigentumsanteil_rohdaten_bereinigt <- read_excel("abstimmung_eigentumsanteil_rohdaten_bereinigt.xlsx", 
                                                            sheet = "Kanton ")                                                            sheet = "Kanton ")
View(abstimmung_eigentumsanteil_rohdaten_bereinigt)

df <- abstimmung_eigentumsanteil_rohdaten_bereinigt

m1 <- lm(Ja_Prozent_Untermiete ~ Eigentumsobjekte_Prozent, data = df)
summary(m1)

m2 <- lm(Ja_Prozent_Eigenbedarf ~ Eigentumsobjekte_Prozent, data = df)
summary(m2)

ggplot(df, aes(x = Eigentumsobjekte_Prozent, y = Ja_Prozent_Untermiete)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Zusammenhang zwischen Eigentumsanteil und Ja-Stimmenanteil bei der Untermietinitiative",
       x = "Anteil Eigentumsobjekte (%)",
       y = "Ja-Stimmenanteil bei Untermietinitiative (%)") +
  theme_minimal()
