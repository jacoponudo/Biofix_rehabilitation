library(readxl)
dati_alessia <- read_excel("Data/dati_alessia.xlsx")

## 1. Distribuzione del genere 
counts <- table(dati_alessia$SESSO)
pie(counts, 
    main = "Distribuzione del Genere", 
    col = c("#ff9999", "#66b3ff"), 
    labels = paste(names(counts), "\n", counts/2, "(", round(100 * prop.table(counts), 1), "%)", sep = ""),
    border = "white")

## 2. Distribuzione del età 
hist(dati_alessia$`ETÀ `,
     main = "Distribuzione delle Età",
     xlab = "Età",
     ylab = "Frequenza",
     col = "#66b3ff",
     border = "white",
     breaks = 10)