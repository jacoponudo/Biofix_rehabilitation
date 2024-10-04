library(readxl)
library(dplyr)

# Definizione delle variabili per studente e occhio
studente <- 'alessia'
occhio <- 'OS'  # Cambia in 'OS' per l'occhio sinistro

# Load dataset
dati_domenica <- read_excel(paste0("Data/dati_", studente, ".xlsx"))
dati_domenica <- dati_domenica[dati_domenica$OCCHIO == occhio,]

# Barplot
bcea_media <- dati_domenica %>%
  group_by(T) %>%
  summarise(media_BCEA = mean(BCEA, na.rm = TRUE))

barplot(height = bcea_media$media_BCEA, 
        names.arg = bcea_media$T, 
        col = viridis::viridis(length(bcea_media$T), option = "cividis"), 
        main = paste0("Barplot Valore Medio BCEA - Occhio ", ifelse(occhio == 'OD', "Destro", "Sinistro")),
        xlab = "Ciclo Trattamento",
        ylab = "Media di BCEA 95% (°xcm2)",
        border = "white")

# Salvataggio dei grafici
dev.copy(png, filename = paste0("Plots/barplot_BCEA_", studente, "_", ifelse(occhio == 'OD', 'destro', 'sinistro'), ".png"))
dev.off()

# Boxplot
boxplot(BCEA ~ T, 
        data = dati_domenica, 
        col = viridis::viridis(length(unique(dati_domenica$T)), option = "cividis"),
        main = paste0("Boxplot delle Distribuzioni di BCEA - Occhio ", ifelse(occhio == 'OD', "Destro", "Sinistro")),
        xlab = "Ciclo Trattamento",
        ylab = "BCEA 95% (°xcm2)",
        border = "black")
dev.copy(png, filename = paste0("Plots/boxplot_BCEA_", studente, "_", ifelse(occhio == 'OD', 'destro', 'sinistro'), ".png"))
dev.off()
