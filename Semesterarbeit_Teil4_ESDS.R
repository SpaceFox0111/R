#Semesterarbeit Teil 4 - 2. Teil "Ausfälle"

#Sicherheitskopie der Originaltabelle erstellen
ausfaelle1 <- ausfaelle

#Bestimmen Sie eine neue Variable, welche das Total aller Ausfälle pro Jahr angibt.
ausfaelle1$summejahr <- rowSums(ausfaelle1[ , c(2:13)], na.rm=TRUE)

#Erwartungswert der Ausfälle der letzten 10 Jahre
#Dh über die letzten 10 Jahre gab es durchschnittlich 22.1 Ausfälle

Erwartungswert_Ausfaelle <- mean(ausfaelle1$summejahr)

#die Poissonverteilung in einem Stabdiagramm darstellen
success <- ausfaelle1$summejahr
plot(success, dpois(success, lambda=16.6),
     type='h',
     main='Poisson Distribution (lambda = 16.6)',
     ylab='Probability',
     xlab ='# Ausfälle',
     lwd=3)

#Wahrscheinlichkeit, dass ich in einem Jahr zwischen 15 und 20 Ausfälle ereignen

ppois(14, lambda = 16.6)
ppois(20, lambda = 16.6)
ppois(20, lambda = 16.6) - ppois(14, lambda = 16.6)


#die mittlere Anzahl Jahre, die zwischen zwei Jahren mit mindestens 25 Ausfällen vergeht

#Wahrscheinlichkeit, dass sich in einem Jahr 25 Ausfälle ereignen.
1 - ppois(24, lambda = 16.6)



#Semesterarbeit Teil 4 - 3. Teil Zusammenhänge

#Aufgabe a) 

#Erzeugt 1000 binomialverteilte Zufallszahlen mit den Parametern n = 10 und p = 0.2
set.seed(10)
beob1000$binom <- rbinom(1000, 10, 0.2)


#Werte der Wahrscheinlichkeitsfunktion von 𝑋 berechnen wir mit dbinom() und stellen sie in einem Balkendiagramm dar
x <- beob1000$binom
plot(x, dbinom(x,10,0.2),
type='h',
main='Binom Distribution B(10,0.2)',
ylab='Probability',
xlab ='# Ziehungen',
lwd=3)


#drei weitere Variablen gemäss selbst gewählten geeigneten hypergeometrischen Verteilungen 

beob1000$hyper1 <- rhyper(1000,10,40,10)
beob1000$hyper2 <- rhyper(1000,50,200,10)
beob1000$hyper3 <- rhyper(1000,250,1000,10)


x1 <- beob1000$hyper1
plot(x1, dhyper(x1, 10, 40, 10),
     type='h',
     main='Hyper 1 Distribution n/N = 20%',
     ylab='Probability',
     xlab ='# Ziehungen',
     lwd=3)

x2 <- beob1000$hyper2
plot(x2, dhyper(x2, 50, 200, 10),
     type='h',
     main='Hyper 2 Distribution n/N = 4%',
     ylab='Probability',
     xlab ='# Ziehungen',
     lwd=3)

x3 <- beob1000$hyper3
plot(x3, dhyper(x3, 250, 1000, 10),
     type='h',
     main='Hyper 3 Distribution n/N = 0.8%',
     ylab='Probability',
     xlab ='# Ziehungen',
     lwd=3)

#Aufgabe b)
#Zufallsvariable Poissonverteilt

beob1000$pois <- rpois(1000, lambda = 5)

x4 <- beob1000$pois
plot(x4, dpois(x4, 5),
     type='h',
     main='Poisson Distribution lambda = 5',
     ylab='Probability',
     xlab ='# Ziehungen',
     lwd=3)

#drei Variablen gemäss selbst gewählten geeigneten bionomial Verteilungen 

beob1000$binom1 <- rbinom(1000, 10, 0.5)
beob1000$binom2 <- rbinom(1000, 20, 0.25)
beob1000$binom3 <- rbinom(1000, 1000, 0.005)

x5 <- beob1000$binom1

plot(x5, dbinom(x5,10,0.5),
     type='h',
     main='Binom1 Distribution B(10,0.5)',
     ylab='Probability',
     xlab ='# Ziehungen',
     lwd=3)

x6 <- beob1000$binom2

plot(x6, dbinom(x6,20,0.25),
     type='h',
     main='Binom2 Distribution B(20,0.25)',
     ylab='Probability',
     xlab ='# Ziehungen',
     lwd=3)

x7 <- beob1000$binom3

plot(x7, dbinom(x7,1000,0.005),
     type='h',
     main='Binom3 Distribution B(1000,0.005)',
     ylab='Probability',
     xlab ='# Ziehungen',
     lwd=3)

