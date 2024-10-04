library(readxl)
dati_alessia <- read_excel("Data/dati_alessia.xlsx")

# 1. Boxplot di BCVA
n_bcva_pl_pre <- sum(!is.na(dati_alessia$`BCVA pl PRE STIMOLAZIONE`)) / 2
n_bcva_pl_post <- sum(!is.na(dati_alessia$`BCVA pl POST STIMOLAZIONE`)) / 2
colori_bcva <- colorRampPalette(c("lightgreen", "orange"))(4)
boxplot(
  dati_alessia$`BCVA pl PRE STIMOLAZIONE`,
  dati_alessia$`BCVA pl POST STIMOLAZIONE`,
  names = c("BCVA pl PRE", "BCVA pl POST"),
  col = colori_bcva,
  main = "Confronto Boxplot BCVA",
  ylab = "BCVA", xlab = ""
)
text(x = 1, y = 0.5, labels = paste("N:", n_bcva_pl_pre), pos = 3,col=0)
text(x =2, y = 0.5, labels = paste("N:", n_bcva_pl_post), pos = 3,col=0)

# 1. Test di BCVA 
test_t <- t.test(dati_alessia$`BCVA pl PRE STIMOLAZIONE`, dati_alessia$`BCVA pl POST STIMOLAZIONE`, var.equal = TRUE)
print(test_t)
