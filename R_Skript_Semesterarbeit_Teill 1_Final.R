#eine neue Sicherheitskopie abspeichern 
worldEcoDemo2013_V1.0 <- worldEcoDemo2013

#eine Übersicht über alle vorhandenen Variablen erstellen
summary(worldEcoDemo2013_V1.0)

#fortlaufend nummerierte ID-Spalte generieren, welche die einzelnen Beobachtungen eindeutig kennzeichnet
worldEcoDemo2013_V1.0$ID <- seq_len(nrow(worldEcoDemo2013_V1.0))

#Neue ID-Spalte an Anfang des Datensatzes schieben (ID Spalte dient eindeutiger Identifikation der einzelnen Beobachtungen)
worldEcoDemo2013_V1.0 <- worldEcoDemo2013_V1.0[, c("ID", "Country Name", "Country Code", "WHO Region", "Worldbank Income Group", "GNI per capita (PPP)", "Population Size", "Crude birth rate (per 1000 population)", "Crude death rate (per 1000 population)", "Infant mortality rate (probability of dying between birth and age 1 per 1000 live births)", "Population median age (years)", "Life expectancy at birth (years), both sexes", "Life expectancy at birth (years), male", "Life expectancy at birth (years), female")] 

#auf Vollständigkeit und Lücken prüfen 
which(is.na(worldEcoDemo2013_V1.0), arr.ind=T)

#Datensatz nach Bruttoeinkommen pro Kopf (GNI) sortieren
worldEcoDemo2013_V1.0 <- worldEcoDemo2013_V1.0[order(worldEcoDemo2013_V1.0$`GNI per capita (PPP)`),]

#Neue kat. Variablen für die GNI Bruttoeinkommen pro Kopf nach Klassen (Faktorvariable) > Klassenzahl entspricht ca der Wurzel der 192 Beobachtungen 
worldEcoDemo2013_V1.0$GNI_Einkommensklasse <- cut(worldEcoDemo2013_V1.0$`GNI per capita (PPP)`, c(0, 10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000, 110000, 120000, 1000000), labels = c("0-10k", "10-20k", "20-30k", "30-40k", "40-50k", "50-60k", "60-70k", "70-80k", "80-90k", "90-100k", "100-110k", "110-120k", "120kplus"), right = FALSE)

#Benötigtes Package laden
library(lattice)

#Häufigkeitstabelle der absoluten Häufigkeiten der neuen kategorialen Variablen "Einkommensklasse_GNI"
table(worldEcoDemo2013_V1.0$GNI_Einkommensklasse)

#Säulendiagramm zur Darstellung der absoluten Häufigkeiten
barplot(table(worldEcoDemo2013_V1.0$GNI_Einkommensklasse), xlab = "Einkommensklassen_GNI", ylab = "Bruttoeinkommen pro Kopf in Dollar")

#Häufigkeitstabelle der relativen Häufigkeiten der neuen kategorialen Variablen "Einkommensklasse_GNI"
prop.table(table(worldEcoDemo2013_V1.0$GNI_Einkommensklasse))

#Histogramm zur Darstellung der relativen Häufigkeiten
hist <- histogram(worldEcoDemo2013_V1.0$`GNI per capita (PPP)`, breaks = seq(from=0, to = 130000, by=10000), xlab=list(label="Einkommensklassen_GNI_in_Dollar", cex=1), ylab=list("Anteile in Prozent", cex = 1), col="grey90", type="percent", scales=list(cex=1), layout=c(1,1))
> print(hist)

#Häufigkeitstabelle der kumulierten relativen Häufigkeiten (> Empirische Verteilungsfunktion)
cumsum(prop.table(table(worldEcoDemo2013_V1.0$GNI_Einkommensklasse)))

#Globale Variable für die empirische Verteilungsfunktion definieren:
Empirische_Verteil.funktion <- ecdf(worldEcoDemo2013_V1.0$`GNI per capita (PPP)`)

#Darstellung der Empirischen Verteilungsfunktion
plot(Empirische_Verteil.funktion, verticals=TRUE, main = "Empirische Verteilungsfunktion des Bruttoeinkommens pro Kopf", do.points=FALSE)

#Arithmetisches Mittel / Median / Varianz / SD für die verschiedenen 13 Einkommenklassen_GNI ausrechnen und die Tabellen jeweils als csv exportieren
data = tapply(worldEcoDemo2013_V1.0$`GNI per capita (PPP)`, worldEcoDemo2013_V1.0$GNI_Einkommensklasse, mean)
Arithm.Mittel_EK_KlassenGNI <- data
write.csv(Arithm.Mittel_EK_KlassenGNI, "arithmetischesMittel.csv")

Median_EK_KlassenGNI = tapply(worldEcoDemo2013_V1.0$`GNI per capita (PPP)`, worldEcoDemo2013_V1.0$GNI_Einkommensklasse, median)
write.csv(Median_EK_KlassenGNI, "Median_EKGNI.csv")

Varianz_EK_KlassenGNI = tapply(worldEcoDemo2013_V1.0$`GNI per capita (PPP)`, worldEcoDemo2013_V1.0$GNI_Einkommensklasse, var)
write.csv(Varianz_EK_KlassenGNI, "Varianz_EKGNI.csv")

SD_EK_KlassenGNI = tapply(worldEcoDemo2013_V1.0$`GNI per capita (PPP)`, worldEcoDemo2013_V1.0$GNI_Einkommensklasse, sd)
write.csv(SD_EK_KlassenGNI, "SD_EKGNI.csv")

#Arithmetisches Mittel, Median, Varianz und Standardabweichung für die Variable GNI per capita PPP
mean(Datensatz_ohneNAinGNIpercapita$`GNI per capita (PPP)`)
median(Datensatz_ohneNAinGNIpercapita$`GNI per capita (PPP)`)
var(Datensatz_ohneNAinGNIpercapita$`GNI per capita (PPP)`)
sd(Datensatz_ohneNAinGNIpercapita$`GNI per capita (PPP)`)

#Boxplot für Bruttoeinkommen pro Kopf
bwplot(worldEcoDemo2013_V1.0$`GNI per capita (PPP)`, xlab=list(label="Bruttoeinkommen pro Kopf in Dollar", cex=1.0),scales(list(cex=1.0)))

#Erstellung Multiboxplot nach WHO Regionen
boxplot(worldEcoDemo2013_V1.0$`GNI per capita (PPP)`[worldEcoDemo2013_V1.0$`WHO Region`=="AFR"], worldEcoDemo2013_V1.0$`GNI per capita (PPP)`[worldEcoDemo2013_V1.0$`WHO Region`=="AMR"], worldEcoDemo2013_V1.0$`GNI per capita (PPP)`[worldEcoDemo2013_V1.0$`WHO Region`=="EMR"], worldEcoDemo2013_V1.0$`GNI per capita (PPP)`[worldEcoDemo2013_V1.0$`WHO Region`=="EUR"], worldEcoDemo2013_V1.0$`GNI per capita (PPP)`[worldEcoDemo2013_V1.0$`WHO Region`=="SEAR"], worldEcoDemo2013_V1.0$`GNI per capita (PPP)`[worldEcoDemo2013_V1.0$`WHO Region`=="WPR"],xlab = "WHO_Regionen", ylab = "Bruttoeinkommen pro Kopf in Dollar", names = c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR"), main = "Bruttoeinkommen pro Kopf in Dollar nach WHO Regionen")

#fünf-Punkte Zusammenfassung der Var. GNI per capita
summary(worldEcoDemo2013_V1.0$`GNI per capita (PPP)`)





