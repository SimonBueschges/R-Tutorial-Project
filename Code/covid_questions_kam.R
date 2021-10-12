# Allgemeine Übung -------------------------------------------------------------

# Erhöhe jeden zweiten Wert des Vektors um das doppelte
# des vorherigen Wert im Vektor 1:10

Ziel <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
Ergebnis <- c()

for (i in Ziel) {
  if (i %% 2 == 0) {
    i <- i + ((i - 1) * 2)
  }
  else {
    i <- i
  }
  Ergebnis <- c(Ergebnis, i)
}
}

Ergebnis

## Erzeuge einen Vektor der aufsteigenden Zahlen an den Positionen der
## Vokale im letters Vektor enthält und 0 für Konsonaten.
## Z.B. c("a", "g", "e", "f", "a")
## wird zu c(1, 0, 2, 0, 3)

Startvektor <- c("a", "g", "e", "f", "a")
Vokalvektor <- c("a", "e", "i", "o", "u")
Ergebnisvektor <- c()

for (i in Startvektor) {
  if (i %in% Vokalvektor) {
    i = 1
  }
  else {
    i = 0
  }
  Ergebnisvektor <- c(Ergebnisvektor, i)
}
Ergebnisvektor

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

Covid_neu <- Covid %>%
  select(location, date, new_cases) %>%
  mutate(Covid_neu.after = median(new_cases, na.rm = TRUE)) %>%
  filter(str_detect(date, "2021")) %>% 
  filter(new_cases > Covid_neu.after) %>% 
  mutate(date = as.Date(date)) %>% 
  arrange(location,desc(date)) %>% 
  group_by(location) %>% 
  ##filter(location=="Asia") %>% 
  mutate(Neu = new_cases-lag(new_cases))

## In welchem Kontinent gibt es die meisten Länder, deren HDI geringer als 0,549
## ist? (Geringe Entwicklung)

## Zwischen welchen beiden Tagen war je Land
## der größte Unterschied an neuen Fallzahlen zu beobachten?


## Kontrolliere, ob es zu jeder Datum und Land Kombination nur einen Eintrag gibt


## Welcher %-Anteil der Geimpften in Frankreich ist am 24.05.2021 vollständig
## geimpft?
