library(readxl)
library(ggplot2)

# Definizione della variabile per studente
studente <- 'alessia'

# Load dataset
dati_domenica <- read_excel(paste0("Data/dati_", studente, ".xlsx"))

# Crea il grafico
p <- ggplot(dati_domenica, aes(x = FUJI, y = BCEA)) +
  geom_point(color = "black", alpha = 0.2) +  # Punti neri con trasparenza
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Regressione lineare
  labs(title = paste0("Correlazione tra FUJI e BCEA - ", studente),
       x = "FUJI",
       y = "BCEA") +
  theme_minimal()

# Esegui il test di correlazione di Pearson
cor_test <- cor.test(dati_domenica$FUJI, dati_domenica$BCEA)

# Aggiungi i risultati del test al grafico
p <- p + annotate("text", x = Inf, y = Inf, 
                  label = paste("Correlazione: r =", round(cor_test$estimate, 2), 
                                "\np-value =", format.pval(cor_test$p.value, digits = 2)), 
                  hjust = 1.1, vjust = 1.1, 
                  size = 4, color = "blue", 
                  fontface = "italic")

# Salva il grafico
ggsave(filename = paste0("Plots/correlazione_FUJI_BCEA_", studente, ".png"), plot = p, width = 8, height = 6)
