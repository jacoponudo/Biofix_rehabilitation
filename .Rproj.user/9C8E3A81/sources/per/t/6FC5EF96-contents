library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)

studente <- 'alessia'
dati_domenica <- read_excel(paste0("Data/dati_", studente, ".xlsx"))

# Modifica dei nomi delle variabili
dati_domenica <- dati_domenica %>%
  rename(
    `BCEA 95%` = BCEA,
    `Sensibilità retinica media` = `SENS RETINICA MEDIA`,
    `Fuji` = FUJI,
    `Acuità visiva per lontano` = AVpl
  )

# Calcolo dei valori medi per T0 e T6 omettendo i NA
mean_values <- data.frame(
  Variable = c("BCEA 95%", "Sensibilità retinica media", "Fuji", "Acuità visiva per lontano"),
  Pre_trattamento = c(mean(dati_domenica[dati_domenica$T == 0, ]$`BCEA 95%`, na.rm = TRUE),
                      mean(dati_domenica[dati_domenica$T == 0, ]$`Sensibilità retinica media`, na.rm = TRUE),
                      mean(dati_domenica[dati_domenica$T == 0, ]$Fuji, na.rm = TRUE),
                      mean(dati_domenica[dati_domenica$T == 0, ]$`Acuità visiva per lontano`, na.rm = TRUE)),
  Post_6_ciclo = c(mean(dati_domenica[dati_domenica$T == 6, ]$`BCEA 95%`, na.rm = TRUE),
                   mean(dati_domenica[dati_domenica$T == 6, ]$`Sensibilità retinica media`, na.rm = TRUE),
                   mean(dati_domenica[dati_domenica$T == 6, ]$Fuji, na.rm = TRUE),
                   mean(dati_domenica[dati_domenica$T == 6, ]$`Acuità visiva per lontano`, na.rm = TRUE))
)

# Riorganizzare i dati in formato lungo per ggplot
mean_values_long <- pivot_longer(mean_values, cols = c(Pre_trattamento, Post_6_ciclo), names_to = "Time", values_to = "Mean_Value")

# Convertire il fattore 'Time' per avere l'ordine corretto
mean_values_long$Time <- factor(mean_values_long$Time, levels = c("Pre_trattamento", "Post_6_ciclo"))

# Aprire un PDF per salvare i barplot su pagine diverse
pdf(paste0("Plots/barplot_confronto_", studente, ".pdf"))

# Variabili per il loop
variabili <- unique(mean_values_long$Variable)

# Creare un barplot per ciascuna variabile e salvarli su pagine separate nel PDF
for (var in variabili) {
  plot <- ggplot(mean_values_long[mean_values_long$Variable == var, ], aes(x = Time, y = Mean_Value, fill = Time)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(title = paste0("Confronto ", var)) +  # Mantiene il titolo
    scale_fill_manual(values = c("Pre_trattamento" = "#4F81BD", "Post_6_ciclo" = "#C0504D"), 
                      labels = c("Pre trattamento", "Post 6° ciclo di trattamenti")) +
    theme_minimal() +
    theme(legend.position = "none",  # Rimuove la legenda
          axis.title.x = element_blank(),  # Rimuove il titolo dell'asse x
          axis.title.y = element_blank())  # Rimuove il titolo dell'asse y
  
  print(plot)  # Stampa il plot su una nuova pagina del PDF
}

# Chiudere il file PDF
dev.off()
