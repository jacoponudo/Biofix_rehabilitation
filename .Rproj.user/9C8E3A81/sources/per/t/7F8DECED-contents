library(readxl)
library(ggplot2)
library(writexl)

studente <- 'domenica'
dati_domenica <- read_excel(paste0("Data/dati_", studente, ".xlsx"))

# Calcolo delle statistiche e test di confronto
results <- data.frame(
  Variable = c("BCEA", "SENS RETINICA MEDIA", "FUJI", "AVpl"),
  T0_SD = c(sd(dati_domenica[dati_domenica$T == 0, ]$BCEA),
            sd(dati_domenica[dati_domenica$T == 0, ]$`SENS RETINICA MEDIA`),
            sd(dati_domenica[dati_domenica$T == 0, ]$FUJI),
            sd(dati_domenica[dati_domenica$T == 0, ]$AVpl)),
  T0_Mean = c(mean(dati_domenica[dati_domenica$T == 0, ]$BCEA),
              mean(dati_domenica[dati_domenica$T == 0, ]$`SENS RETINICA MEDIA`),
              mean(dati_domenica[dati_domenica$T == 0, ]$FUJI),
              mean(dati_domenica[dati_domenica$T == 0, ]$AVpl)),
  T6_SD = c(sd(na.omit(dati_domenica[dati_domenica$T == 6, ]$BCEA)),
            sd(na.omit(dati_domenica[dati_domenica$T == 6, ]$`SENS RETINICA MEDIA`)),
            sd(na.omit(dati_domenica[dati_domenica$T == 6, ]$FUJI)),
            sd(na.omit(dati_domenica[dati_domenica$T == 6, ]$AVpl))),
  T6_Mean = c(mean(na.omit(dati_domenica[dati_domenica$T == 6, ]$BCEA)),
              mean(na.omit(dati_domenica[dati_domenica$T == 6, ]$`SENS RETINICA MEDIA`)),
              mean(na.omit(dati_domenica[dati_domenica$T == 6, ]$FUJI)),
              mean(na.omit(dati_domenica[dati_domenica$T == 6, ]$AVpl))),
  Test_Statistic = c(t.test(dati_domenica$BCEA[dati_domenica$T == 0], 
                            dati_domenica$BCEA[dati_domenica$T == 6], 
                            var.equal = TRUE)$statistic,
                     t.test(dati_domenica$`SENS RETINICA MEDIA`[dati_domenica$T == 0], 
                            dati_domenica$`SENS RETINICA MEDIA`[dati_domenica$T == 6], 
                            var.equal = TRUE)$statistic,
                     t.test(dati_domenica$FUJI[dati_domenica$T == 0], 
                            dati_domenica$FUJI[dati_domenica$T == 6], 
                            var.equal = TRUE)$statistic,
                     t.test(dati_domenica$AVpl[dati_domenica$T == 0], 
                            dati_domenica$AVpl[dati_domenica$T == 6], 
                            var.equal = TRUE)$statistic),
  p_value = c(t.test(dati_domenica$BCEA[dati_domenica$T == 0], 
                     dati_domenica$BCEA[dati_domenica$T == 6], 
                     var.equal = TRUE)$p.value,
              t.test(dati_domenica$`SENS RETINICA MEDIA`[dati_domenica$T == 0], 
                     dati_domenica$`SENS RETINICA MEDIA`[dati_domenica$T == 6], 
                     var.equal = TRUE)$p.value,
              t.test(dati_domenica$FUJI[dati_domenica$T == 0], 
                     dati_domenica$FUJI[dati_domenica$T == 6], 
                     var.equal = TRUE)$p.value,
              t.test(dati_domenica$AVpl[dati_domenica$T == 0], 
                     dati_domenica$AVpl[dati_domenica$T == 6], 
                     var.equal = TRUE)$p.value)
)

# Salvataggio in un file Excel
write_xlsx(results, path = paste0("statistiche_", studente, ".xlsx"))


library(ggplot2)
library(tidyr)
library(readxl)

studente <- 'alessia'
dati_domenica <- read_excel(paste0("Data/dati_", studente, ".xlsx"))
dati_domenica$AVpl=dati_domenica$AVpl*100
# Calcolare i valori medi per T0 e T6 omettendo i NA
mean_values <- data.frame(
  Variable = c("BCEA", "SENS RETINICA MEDIA", "FUJI", "AVpl"),
  T0_Mean = c(mean(dati_domenica[dati_domenica$T == 0, ]$BCEA, na.rm = TRUE),
              mean(dati_domenica[dati_domenica$T == 0, ]$`SENS RETINICA MEDIA`, na.rm = TRUE),
              mean(dati_domenica[dati_domenica$T == 0, ]$FUJI, na.rm = TRUE),
              mean(dati_domenica[dati_domenica$T == 0, ]$AVpl, na.rm = TRUE)),
  T6_Mean = c(mean(dati_domenica[dati_domenica$T == 6, ]$BCEA, na.rm = TRUE),
              mean(dati_domenica[dati_domenica$T == 6, ]$`SENS RETINICA MEDIA`, na.rm = TRUE),
              mean(dati_domenica[dati_domenica$T == 6, ]$FUJI, na.rm = TRUE),
              mean(dati_domenica[dati_domenica$T == 6, ]$AVpl, na.rm = TRUE))
)

# Riorganizzare i dati in formato lungo per ggplot
mean_values_long <- pivot_longer(mean_values, cols = c(T0_Mean, T6_Mean), names_to = "Time", values_to = "Mean_Value")

# Creare i barplot
plot <- ggplot(mean_values_long, aes(x = Variable, y = Mean_Value, fill = Time)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Confronto tra T0 e T6", x = "Variabile", y = "Valore Medio") +
  scale_fill_manual(values = c("T0_Mean" = "#4F81BD", "T6_Mean" = "#C0504D"), labels = c("Pre Trattamento", "Post Trattamento")) +
  theme_minimal()

# Salvare il plot con il nome dello studente
ggsave(filename = paste0("Plots/barplot_confronto_", studente, ".jpg"), plot = plot, width = 8, height = 6)
