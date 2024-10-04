library(readxl)
#install.packages('reshape2')
library(reshape2)
dati_alessia <- read_excel("Data/dati_alessia.xlsx")

## 1. Boxplot di BCEA
n_pre_stimolazione <- sum(!is.na(dati_alessia$`BCEA 95% (°xcm2) PRE-STIMOLAZIONE`)) / 2
n_t1 <- sum(!is.na(dati_alessia$`BCEA 95% (T1)`)) / 2
n_t2 <- sum(!is.na(dati_alessia$`BCEA 95% (T2)`)) / 2
n_t3 <- sum(!is.na(dati_alessia$`BCEA 95% (T3)`)) / 2
n_t4 <- sum(!is.na(dati_alessia$`BCEA 95% (T4)`)) / 2
n_t5 <- sum(!is.na(dati_alessia$`BCEA 95% (T5)`)) / 2
n_t6 <- sum(!is.na(dati_alessia$`BCEA 95% (T6)`)) / 2
colori <- colorRampPalette(c("skyblue", "salmon"))(7)
boxplot(
  dati_alessia$`BCEA 95% (°xcm2) PRE-STIMOLAZIONE`,
  dati_alessia$`BCEA 95% (T1)`,
  dati_alessia$`BCEA 95% (T2)`,
  dati_alessia$`BCEA 95% (T3)`,
  dati_alessia$`BCEA 95% (T4)`,
  dati_alessia$`BCEA 95% (T5)`,
  dati_alessia$`BCEA 95% (T6)`,
  names = c("PRE-STIMOLAZIONE", "T1", "T2", "T3", "T4", "T5", "T6"),
  col = colori,
  main = "Confronto Boxplot BCEA 95%",
  ylab = "BCEA 95%",xlab = "Tempo")
text(x = 1, y =250, labels = paste("N:", n_pre_stimolazione), pos = 3)
text(x = 2, y = 250, labels = paste("N:", n_t1), pos = 3)
text(x = 3, y = 250, labels = paste("N:", n_t2), pos = 3)
text(x = 4, y =250, labels = paste("N:", n_t3), pos = 3)
text(x = 5, y = 250, labels = paste("N:", n_t4), pos = 3)
text(x = 6, y = 250, labels = paste("N:", n_t5), pos = 3)
text(x = 7, y = 250, labels = paste("N:", n_t6), pos = 3)


## 2. T Test
dati_long <- melt(dati_alessia[, c(
  "BCEA 95% (°xcm2) PRE-STIMOLAZIONE",
  "BCEA 95% (T1)",
  "BCEA 95% (T2)",
  "BCEA 95% (T3)",
  "BCEA 95% (T4)",
  "BCEA 95% (T5)",
  "BCEA 95% (T6)"
)], variable.name = "Tempo", value.name = "Valori")
dati_long <- na.omit(dati_long)
risultati_significativi <- list()
tempi <- unique(dati_long$Tempo)
for (i in 1:(length(tempi) - 1)) {
  for (j in (i + 1):length(tempi)) {
    t_test <- t.test(Valori ~ Tempo, data = dati_long[dati_long$Tempo %in% c(tempi[i], tempi[j]), ])
    if (t_test$p.value < 0.1) {
      risultati_significativi[[paste(tempi[i], tempi[j], sep = " vs ")]] <- t_test$p.value
    }
  }
}
risultati_significativi



