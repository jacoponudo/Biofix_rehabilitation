library(readxl)
studente <- 'domenica'
dati_domenica <- read_excel(paste0("Data/dati_", studente, ".xlsx"))

plot(dati_domenica$T, dati_domenica$`SENS RETINICA MEDIA`)

dati_domenica$PAZIENTE[dati_domenica$PAZIENTE=="S.I.F"] <- "S.I.F."

library(ggplot2)
colori <- c("blue", "orange", "purple", "green", 'red', 'black')

ggplot(dati_domenica, aes(x = T, y = `SENS RETINICA MEDIA`, color = PAZIENTE, linetype = OCCHIO)) +
  geom_line(size = 0.6) +
  labs(title = "Serie Storiche per Paziente",
       x = "Trattamenti (T)",
       y = "Sensibilità Retinica Media (dB)") +
  scale_color_manual(values = colori) +
  scale_linetype_manual(values = c("solid", "solid")) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(paste0("Plots/sensibilità_", studente, ".jpg"), width = 8, height = 6)
