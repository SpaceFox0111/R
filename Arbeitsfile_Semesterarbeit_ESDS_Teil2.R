#Basierend auf den Arbeiten der ersten Semesterarbeit das bestehende Datenset verwenden und als Version 2 abspeichern
worldEcoDemo2013_V2.0 <- worldEcoDemo2013_V1.0

#Teil 1 - WHO-Region und Worldbank Income Group

#Zusammenhang von WHO Region und Worldbank Income Group - Kontingenztabelle erstellen
table(worldEcoDemo2013_V2.0$`WHO Region`, worldEcoDemo2013_V2.0$`Worldbank Income Group`)

#Randhäufigkeiten hinzufügen
addmargins(table(worldEcoDemo2013_V2.0$`WHO Region`, worldEcoDemo2013_V2.0$`Worldbank Income Group`))

#Dreidimensionales Säulendiagramm der gemeinsamen absoluten Häufigkeiten
require(epade)
bar3d.ade(table(worldEcoDemo2013_V2.0$`WHO Region`, worldEcoDemo2013_V2.0$`Worldbank Income Group`), xlab="World Income Group", ylab="Absolute H.", zlab="WHO Region", alpha=0.1)

#Relative bHäufigkeitstabelle
Tabelle1 <- table(worldEcoDemo2013_V2.0$`WHO Region`, worldEcoDemo2013_V2.0$`Worldbank Income Group`)
prop.table(Tabelle1, margin=1)

#BEDINGTE Relative bHäufigkeitstabelle
prop.table(Tabelle1, margin=2)

#Säulendiagramm für die bedingten relativen Häufigkeiten
Tabelle3 <- prop.table(Tabelle1, margin=2)
bar3d.ade(Tabelle3, xlab="World Income Group", ylab="Rel.bedingt.Häufigk.", zlab="WHO Region", alpha=0.1)

#Kontingenzkoeffizienten rechnen (X2-Koeff.)
require(vcd)
Tabelle2 <- table(worldEcoDemo2013_V2.0$`WHO Region`, worldEcoDemo2013_V2.0$`Worldbank Income Group`)
assocstats(Tabelle2)

#korrigierter Kontingenzkoeff.
sqrt(((16-1)/16))
0.615/0.9682458


#Teil 2 Zusammenhang GNI_Einkommensklasse Ranking & Lebenserwartung für beide Geschlechter

#Neue Rankingvariable GNI_Einkommensklasse
worldEcoDemo2013_V2.0$GNI_Einkommensklasse_Ranking <- cut(worldEcoDemo2013_V2.0$`GNI per capita (PPP)`, c(0, 10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000, 110000, 120000, 1000000), labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), right = FALSE)

#Faktorvariable in eine numerische Variable umwandeln inklusive Überprüfung
worldEcoDemo2013_V2.0$GNI_Einkommensklasse_Ranking <- as.numeric(as.character(worldEcoDemo2013_V2.0$GNI_Einkommensklasse_Ranking))
is.numeric((worldEcoDemo2013_V2.0$GNI_Einkommensklasse_Ranking))

#Sicherheitskopie Datenset
worldEcoDemo2013_V3.0 <- worldEcoDemo2013_V2.0

#Datenset definieren (GNI_EK Klasse Ranking und Lebenserwartung)
teildaten11 <- data.frame(worldEcoDemo2013_V3.0$GNI_Einkommensklasse_Ranking, worldEcoDemo2013_V3.0$`Life expectancy at birth (years), both sexes`)

#mit Tidyr Packet NA Werte bereinigen (für Spearman cor("spearman") Funktion nötig)
library(tidyr)
teildaten11 <- teildaten11 %>% drop_na()

#Spearmans Korr zwischen 2 variablen
cor(teildaten11, method="spearman")
cor.test(worldEcoDemo2013_V3.0$GNI_Einkommensklasse_Ranking, worldEcoDemo2013_V3.0$`Life expectancy at birth (years), both sexes`, method="spearman")

#Boxplot GNI_Einkommensklasse Ranking und Lebenserwartung beider Geschlechter
boxplot(worldEcoDemo2013_V3.0$`Life expectancy at birth (years), both sexes`[worldEcoDemo2013_V3.0$GNI_Einkommensklasse_Ranking=="1"], worldEcoDemo2013_V3.0$`Life expectancy at birth (years), both sexes`[worldEcoDemo2013_V3.0$GNI_Einkommensklasse_Ranking=="2"], worldEcoDemo2013_V3.0$`Life expectancy at birth (years), both sexes`[worldEcoDemo2013_V3.0$GNI_Einkommensklasse_Ranking=="3"], worldEcoDemo2013_V3.0$`Life expectancy at birth (years), both sexes`[worldEcoDemo2013_V3.0$GNI_Einkommensklasse_Ranking=="4"], worldEcoDemo2013_V3.0$`Life expectancy at birth (years), both sexes`[worldEcoDemo2013_V3.0$GNI_Einkommensklasse_Ranking=="5"], worldEcoDemo2013_V3.0$`Life expectancy at birth (years), both sexes`[worldEcoDemo2013_V3.0$GNI_Einkommensklasse_Ranking=="6"], worldEcoDemo2013_V3.0$`Life expectancy at birth (years), both sexes`[worldEcoDemo2013_V3.0$GNI_Einkommensklasse_Ranking=="7"], worldEcoDemo2013_V3.0$`Life expectancy at birth (years), both sexes`[worldEcoDemo2013_V3.0$GNI_Einkommensklasse_Ranking=="8"], worldEcoDemo2013_V3.0$`Life expectancy at birth (years), both sexes`[worldEcoDemo2013_V3.0$GNI_Einkommensklasse_Ranking=="9"], worldEcoDemo2013_V3.0$`Life expectancy at birth (years), both sexes`[worldEcoDemo2013_V3.0$GNI_Einkommensklasse_Ranking=="10"], worldEcoDemo2013_V3.0$`Life expectancy at birth (years), both sexes`[worldEcoDemo2013_V3.0$GNI_Einkommensklasse_Ranking=="11"], worldEcoDemo2013_V3.0$`Life expectancy at birth (years), both sexes`[worldEcoDemo2013_V3.0$GNI_Einkommensklasse_Ranking=="12"], worldEcoDemo2013_V3.0$`Life expectancy at birth (years), both sexes`[worldEcoDemo2013_V3.0$GNI_Einkommensklasse_Ranking=="13"],xlab = "GNI_Einkommensklasse_Ranking", ylab = "Lebenserwartung beider Geschlecher", names = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"), main = "Multiboxplot Lebenserwartung nach Rängen Einkommensklassen")

#Säulendiagramm zur Darstellung der absoluten Häufigkeiten
barplot(table(worldEcoDemo2013_V3.0$GNI_Einkommensklasse_Ranking), xlab="GNI_Einkommensklasse_Ranking", ylab="Verteilung absoluter Häufigkeiten")


#Teil 3 - Zusammenhangsanalyse metrischer Merkmale

#Globale Variablen bilden
Lebenserwartung_F <- worldEcoDemo2013_V3.0$`Life expectancy at birth (years), female`
Geburtenrate <- worldEcoDemo2013_V3.0$`Crude birth rate (per 1000 population)`

#Streudiagramm 
plot(Geburtenrate, Lebenserwartung_F)

#Regressionsmodell
teildaten6 <- data.frame(worldEcoDemo2013_V3.0$`Crude birth rate (per 1000 population)`, worldEcoDemo2013_V3.0$`Life expectancy at birth (years), female`)
lin.reg3 <- tm(teildaten6)
summary(lin.reg3)

#Streudiagramm mit Regressionsgerade
plot(Geburtenrate, Lebenserwartung_F)
abline(lin.reg3, col="red")

#Korrelation Bravais Pearson
cor.test(worldEcoDemo2013_V3.0$`Crude birth rate (per 1000 population)`, worldEcoDemo2013_V3.0$`Life expectancy at birth (years), female`)

#Kleinste Quadrate Schätzer
coefficients(lin.reg3)

#Bestimmtheitsmass 
summary(lin.reg3)$r.squared

#Graphische Darstellung der linearen Regression
plot(lin.reg3, which=1)

