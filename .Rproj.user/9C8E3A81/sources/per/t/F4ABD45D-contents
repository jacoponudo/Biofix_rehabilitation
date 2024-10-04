library(readxl)
dati_alessia <- read_excel("Data/dati_alessia.xlsx")

# 1. Boxplot di BCEA per Fuji 2
n_fuji2_pre <- sum(!is.na(dati_alessia$`Fuji 2° centrali  PRE- STIMOLAZIONE `)) / 2
n_fuji2_t1 <- sum(!is.na(dati_alessia$`Fuji 2° centrali % (T1)`)) / 2
n_fuji2_t2 <- sum(!is.na(dati_alessia$`Fuji 2° centrali % (T2)`)) / 2
n_fuji2_t3 <- sum(!is.na(dati_alessia$`Fuji 2° centrali % (T3)`)) / 2
n_fuji2_t4 <- sum(!is.na(dati_alessia$`Fuji 2° centrali % (T4)`)) / 2
n_fuji2_t5 <- sum(!is.na(dati_alessia$`Fuji 2° centrali % (T5)`)) / 2
n_fuji2_t6 <- sum(!is.na(dati_alessia$`Fuji 2° centrali % (T6)`)) / 2
colori_fuji2 <- colorRampPalette(c("lightgreen", "orange"))(7)

boxplot(
  dati_alessia$`Fuji 2° centrali  PRE- STIMOLAZIONE `,
  dati_alessia$`Fuji 2° centrali % (T1)`,
  dati_alessia$`Fuji 2° centrali % (T2)`,
  dati_alessia$`Fuji 2° centrali % (T3)`,
  dati_alessia$`Fuji 2° centrali % (T4)`,
  dati_alessia$`Fuji 2° centrali % (T5)`,
  dati_alessia$`Fuji 2° centrali % (T6)`,
  names = c("PRE-STIMOLAZIONE", "T1", "T2", "T3", "T4", "T5", "T6"),
  col = colori_fuji2,
  main = "Confronto Boxplot Fuji 2",
  ylab = "Fuji 2° centrali",
  xlab = "Tempo"
)
text(x = 1, y = 60, labels = paste("N:", n_fuji2_pre), pos = 3)
text(x = 2, y = 60, labels = paste("N:", n_fuji2_t1), pos = 3)
text(x = 3, y = 60, labels = paste("N:", n_fuji2_t2), pos = 3)
text(x = 4, y = 60, labels = paste("N:", n_fuji2_t3), pos = 3)
text(x = 5, y = 60, labels = paste("N:", n_fuji2_t4), pos = 3)
text(x = 6, y = 60, labels = paste("N:", n_fuji2_t5), pos = 3)
text(x = 7, y = 60, labels = paste("N:", n_fuji2_t6), pos = 3)

# 2. T Test per Fuji 2
library(reshape2)
dati_long_fuji2 <- melt(dati_alessia[, c(6, 11, 14, 17, 20, 23, 26)], variable.name = "Tempo", value.name = "Valori")
dati_long_fuji2 <- na.omit(dati_long_fuji2)
risultati_significativi_fuji2 <- list()
tempi_fuji2 <- unique(dati_long_fuji2$Tempo)
for (i in 1:(length(tempi_fuji2) - 1)) {
  for (j in (i + 1):length(tempi_fuji2)) {
    t_test_fuji2 <- t.test(Valori ~ Tempo, data = dati_long_fuji2[dati_long_fuji2$Tempo %in% c(tempi_fuji2[i], tempi_fuji2[j]), ])
    if (t_test_fuji2$p.value < 0.1) {
      risultati_significativi_fuji2[[paste(tempi_fuji2[i], tempi_fuji2[j], sep = " vs ")]] <- t_test_fuji2$p.value
    }
  }
}
risultati_significativi_fuji2
