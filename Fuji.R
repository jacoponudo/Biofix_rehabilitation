library(readxl)

# Definizione delle variabili per studente e occhio
studente <- 'domenica'
occhio <- 'OD'  # Puoi cambiare in 'OS' per l'occhio sinistro

# Caricamento del dataset con concatenazione corretta per il nome del file
dati_domenica <- read_excel(paste0("Data/dati_", studente, ".xlsx"))

# Correzione del nome del paziente
dati_domenica$PAZIENTE[dati_domenica$PAZIENTE == "S.I.F"] <- "S.I.F."

# Conversione della colonna 'FUJI' in numerico
dati_domenica$FUJI <- as.numeric(dati_domenica$FUJI)

# Filtraggio per l'occhio selezionato (OD o OS)
dati_domenica <- dati_domenica[dati_domenica$OCCHIO == occhio,]

if (studente=='domenica'){
  dati_domenica$FUJI=as.integer(dati_domenica$FUJI*100)}

# Caricamento del pacchetto ggplot2
library(ggplot2)

# Definizione dei colori
colori <- c("blue", "orange", "purple", "darkgreen", "brown", "black")

# Creazione del grafico con rettangoli
ggplot(dati_domenica, aes(x = T, y = FUJI, color = PAZIENTE, linetype = OCCHIO)) +
  # Aggiunta dei rettangoli per gli intervalli
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = 70, alpha = 0.2, fill = "red") +  # Intervallo 0-70
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 70, ymax = 95, alpha = 0.2, fill = "orange") +  # Intervallo 70-96
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 95, ymax = 100, alpha = 0.2, fill = "green") +  # Intervallo 95-100
  geom_line(size = 1) +  # Spessore della linea
  geom_point(size = 2, shape = 21, fill = "white") +  # Aggiunta dei punti
  labs(title = paste0("Serie Storiche per Paziente - Occhio ", ifelse(occhio == 'OD', "Destro", "Sinistro")),
       x = "Cicli di trattamenti (T)",
       y = "Fuji 2° (%)") +
  scale_color_manual(values = colori) +  # Colori personalizzati
  scale_linetype_manual(values = c("solid", "solid")) +  # Tipi di linea
  theme_minimal() +
  theme(legend.position = "bottom")

# Salvataggio del plot con nome personalizzato
ggsave(paste0("Plots/serie_Fuji_", studente, "_occhio_", ifelse(occhio == 'OD', 'destro', 'sinistro'), ".png"), width = 8, height = 6)
