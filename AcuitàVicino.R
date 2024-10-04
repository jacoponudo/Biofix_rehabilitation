# Nome dello studente
studente <- 'alessia'
library(readxl)
library(ggplot2)

# Crea la cartella se non esiste
if (!dir.exists("Plots")) {
  dir.create("Plots")
}

# Carica i dati
dati_domenica <- read_excel(paste0("Data/dati_", studente, ".xlsx"))

# Seleziona i dati per T=0 e T=6
dati_T0 <- dati_domenica[dati_domenica$T == 0,]$AVpv
dati_T6 <- dati_domenica[dati_domenica$T == 6,]$AVpv

# Creazione di un data frame
df <- data.frame(
  Tempo = rep(c("T0", "T6"), times = c(length(dati_T0), length(dati_T6))),
  Categoria = c(dati_T0, dati_T6)
)

# Conteggio delle occorrenze per ogni categoria e tempo
conteggi <- as.data.frame(table(df))

# Applicazione del test del chi-quadro
chi_square_test <- chisq.test(table(df$Categoria, df$Tempo))

# Estrazione del p-value dal test
p_value <- round(chi_square_test$p.value, 4)

# Creazione del barplot con ggplot2
p <- ggplot(conteggi, aes(x = Categoria, y = Freq, fill = Tempo)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#FBB4AE", "#B3CDE3")) +  # Colori personalizzati
  labs(title = "Confronto Acuità Visiva (Vicino)",
       x = "Grado",
       y = "Conteggio") +
  theme_minimal() +
  # Aggiungi annotazione del risultato del test
  annotate("text", x = Inf, y = Inf, label = paste("p-value =", p_value), 
           hjust = 1.1, vjust = 1.5, size = 5, color = "black")

# Salvataggio del grafico
ggsave(filename = paste("Plots/Confronto_AVpv_", studente, ".jpg", sep = ""), 
       plot = p, 
       width = 8, height = 6, dpi = 300)
