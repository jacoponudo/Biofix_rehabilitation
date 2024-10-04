# Imposta il nome dello studente
studente <- 'domenica'
library(readxl)

# Carica i dati
dati_domenica <- read_excel(paste0("Data/dati_", studente, ".xlsx"))

# Seleziona i dati per T=0 e T=6
dati_T0 <- dati_domenica[dati_domenica$T == 0,]$AVpl
dati_T6 <- dati_domenica[dati_domenica$T == 6,]$AVpl

# Crea un dataframe per il boxplot
dati_plot <- data.frame(
  Tempo = factor(rep(c("Pre Trattamento", "T6"), each = length(dati_T0))),
  Valori = c(dati_T0, dati_T6)
)

# Esegui il t-test
risultato_test <- t.test(dati_T0, dati_T6)

# Crea la cartella per i grafici se non esiste
if (!dir.exists("Plots")) {
  dir.create("Plots")
}

# Crea e salva il boxplot
png(filename = paste("Plots/Confronto_AVpl_", studente, ".jpg", sep = ""), width = 800, height = 600)

# Crea il boxplot con due colori diversi
boxplot(Valori ~ Tempo, data = dati_plot,
        col = c("lightblue", "lightgreen"),  # Colori diversi per T0 e T6
        main = paste("Confronto Acuità Visiva (Lontano) -", studente),
        xlab = "Tempo",
        ylab = "Acuità Visiva Per Lontano")

# Aggiungi i risultati del t-test
text(x = 1.5, y = max(dati_plot$Valori) + 0.1, 
     labels = paste("t =", round(risultato_test$statistic, 2), 
                    "\np =", format(risultato_test$p.value, digits = 2)), 
     cex = 1.2, pos = 3)

# Chiudi il dispositivo grafico
dev.off()

# Mostra il grafico
print("Boxplot salvato con successo.")
