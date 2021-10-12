# Allgemeine Übung -------------------------------------------------------------

# Erhöhe jeden zweiten Wert des Vektors um das doppelte
# des vorherigen Wert im Vektor 1:10

c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

c(1, 4, 3, 10, 5, 16, 7, 22, 9, 28)


c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)+
  c(0, 2, 0, 6, 0, 10, 0, 14, 0)


## Erzeuge einen Vektor der aufsteigenden Zahlen an den Positionen der 
## Vokale im letters Vektor enthält und 0 für Konsonaten.
## Z.B. c("a", "g", "e", "f", "a") 
## wird zu c(1, 0, 2, 0, 3)

ziel <- c()
erg <- c("a", "g", "e", "f", "a",  "e", "f", "a")

for(i in 1:length(erg)){
  if(erg[i] %in% c("a", "e", "i", "o", "u")){
    
    ziel[i] <- 1
  } else {
    ziel[i] <-  0
  }
}

ziel

ziel2 <- c()

for(i in 1:length(erg)){
  if(erg[i] %in% c("a", "e", "i", "o", "u")){
    
    ziel2[i] <- cumsum(ziel)[i]
  } else {
    ziel2[i] <- ziel[i]
  }
}

ziel2

# Fragen zu Covid Daten ----

## Wie viele Covid Cases gab es bis zum Zeitpunkt 
## an dem die erste Impfung in Asien 
## verabreicht wurde ?


## Was ist das Maximum an Impfungen je Land an einzelnem Tag?
## An welchem Tag trat dieses Maximum auf?
## Welchen Anteil macht dieser Wert von den Gesamtimpfungen des Kontients
## an dem zugehörigen Tag aus? 

## Berechne den median der neuen Fälle und filter dann alle Zeilen, 
## bei denen es in 2021 mehr neue Fälle als den Median gab.
## Ordne die in absteigender Reihenfolge und berechne den Anstieg / Abstieg
## in Bezug auf den Vortag je Land


## In welchem Kontinent gibt es die meisten Länder, deren HDI geringer als 0,549
## ist? (Geringe Entwicklung)

## Zwischen welchen beiden Tagen war je Land
## der größte Unterschied an neuen Fallzahlen zu beobachten? 


## Kontrolliere, ob es zu jeder Datum und Land Kombination nur einen Eintrag gibt


## Welcher %-Anteil der Geimpften in Frankreich ist am 24.05.2021 vollständig
## geimpft?

