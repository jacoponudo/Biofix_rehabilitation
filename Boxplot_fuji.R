library(readxl)
library(ggplot2)
library(dplyr)
library(reshape2)

# Imposta il nome dello studente
studente <- 'alessia'

# Caricamento del dataset
dati_alessia <- read_excel(paste0("Data/dati_", studente, ".xlsx"))
# Assicurarsi che la colonna 'T' sia un fattore (categoria)
dati_alessia$T <- as.factor(dati_alessia$T)
dati_alessia$FUJI <- as.numeric(dati_alessia$FUJI)

# 1. Boxplot di Fuji 2° centrali per periodi (Pre-Stimolazione, T1-T6)
# Calcolo del numero di osservazioni non NA per ciascun periodo
n_osservazioni <- dati_alessia %>%
  group_by(T) %>%
  summarise(N = sum(!is.na(FUJI)) / 2)

# Colori per i boxplot (corretti per essere assegnati a livelli discreti)
colori_fuji2 <- colorRampPalette(c("lightblue", "purple"))(length(unique(dati_alessia$T)))

# Creazione del boxplot
plot_fuji <- ggplot(dati_alessia, aes(x = T, y = FUJI, fill = T, group = T)) +
  geom_boxplot() +
  scale_fill_manual(values = colori_fuji2) +
  labs(title = paste("Confronto Boxplot Fuji 2"),
       x = "Tempo",
       y = "Fuji 2° centrali") +
  theme_minimal() +
  geom_text(data = n_osservazioni, aes(x = T, y = max(dati_alessia$FUJI, na.rm = TRUE), label = paste("N:", N)), vjust = -0.5)

# Salvataggio del grafico nella cartella Plots con il nome dello studente
ggsave(filename = paste0("Plots/boxplot_fuji_", studente, ".jpg"), plot = plot_fuji, width = 8, height = 6)
