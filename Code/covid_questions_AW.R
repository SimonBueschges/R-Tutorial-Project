# Allgemeine Übung -------------------------------------------------------------

# Erhöhe jeden zweiten Wert des Vektors um das doppelte
# des vorherigen Wert im Vektor 1:10

x <-   c(1:10)
y = 2
i <-  x %% y

ergebnis <- c()

for (k in x) {
  if (k %% 2 == 0) {
    ergebnis[k]<-x[k] + 2*x[k-1]
  } else {
    ergebnis[k]<- x[k]
  }
  
}


ergebnis
## Erzeuge einen Vektor der aufsteigenden Zahlen an den Positionen der
## Vokale im letters Vektor enthält und 0 für Konsonaten.
## Z.B. c("a", "g", "e", "f", "a")
## wird zu c(1, 0, 2, 0, 3)



# Fragen zu Covid Daten ----

## Wie viele Covid Cases gab es bis zum Zeitpunkt
## an dem die erste Impfung in Asien
## verabreicht wurde ?

covtab2 <-
  select(Covid,
         location,
         date,
         total_cases,
         new_cases,
         total_vaccinations) %>%
  filter(location == "Asia") %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  filter(total_vaccinations == min(total_vaccinations, na.rm = TRUE)) %>%
  View()

## Was ist das Maximum an Impfungen je Land an einzelnem Tag?
## An welchem Tag trat dieses Maximum auf?
## Welchen Anteil macht dieser Wert von den Gesamtimpfungen des Kontients
## an dem zugehörigen Tag aus?

covtab3 <- Covid %>% 
  mutate(date = as.Date(date)) %>%
  group_by(location) %>% 
  filter(max(new_vaccinations, na.rm = TRUE)) %>% 
  select(Covid, continent, location, date, total_vaccinations, new_vaccinations)

rm(max_impf_je_tag)

max_impf_je_tag2 <- Covid %>%
  select(continent, location, date, new_vaccinations,)

max_impf_je_tag <- Covid %>%
  group_by(location) %>%
  filter(new_vaccinations == max(new_vaccinations, na.rm = TRUE)) %>%
  select(continent, location, date, new_vaccinations) %>%
  inner_join(max_impf_je_tag2, by = c("continent" = "location", "date")) %>% 
  mutate(share = new_vaccinations.x / new_vaccinations.y) %>% 
  ungroup() %>% 
  arrange(desc(share))


Jaaaaa <-  Covid %>% 
  select(continent, location) %>% 
  distinct(continent, location, .keep_all = TRUE)  %>% 
  mutate(anzahl_a = str_count(location, "a|A")) 

neworder <- 
  Jaaaaa %>%
  mutate(new_location =
           factor(location) %>%
           fct_reorder(anzahl_a, max)) 

max_impf_je_tag2 <- Covid %>%
  select(continent, location, date, new_vaccinations, )

max_impf_je_tag <- Covid %>%
  group_by(location) %>%
  filter(new_vaccinations == max(new_vaccinations, na.rm = TRUE)) %>%
  select(continent, location, date, new_vaccinations) %>%
  inner_join(max_impf_je_tag2, by = c("continent" = "location", "date")) %>%
  mutate(share = new_vaccinations.x / new_vaccinations.y) %>%
  ungroup() %>%
  arrange(desc(share))

k <-
  max_impf_je_tag %>%
  mutate(new_loc =
           factor(location) %>%
           fct_reorder(share, max))

k %>%
  ggplot(mapping = aes(x = share, y = new_loc)) +
  geom_col() +
  facet_wrap( ~ continent, scales = "free")



f <-
  max_impf_je_tag %>%
  inner_join(Jaaaaa) %>% 
  mutate(new_loc =
           factor(location) %>%
           fct_reorder(anzahl_a, max)) 

f %>% 
  ggplot(mapping = aes(x = share, y = new_loc)) +
  geom_col(fill = "Green")+
  facet_wrap(~continent,scales = "free")



max_impf_je_tag3 <- Covid %>%
  distinct(location) %>%
  slice_sample(n=5)


max_impf_je_tag4 <-  max_impf_je_tag3 %>% 
  left_join(Covid, by = "location")%>%
  count(location)



rm(max_impf_je_tag3)
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
  arrange(location, desc(date)) %>%
  group_by(location) %>%
  ##filter(location=="Asia") %>%
  mutate(Neu = new_cases - lag(new_cases))

## In welchem Kontinent gibt es die meisten Länder, deren HDI geringer als 0,549
## ist? (Geringe Entwicklung)

## Zwischen welchen beiden Tagen war je Land
## der größte Unterschied an neuen Fallzahlen zu beobachten?


## Kontrolliere, ob es zu jeder Datum und Land Kombination nur einen Eintrag gibt
kontrolle <- Covid %>% 
  select(location, date, total_cases) %>% 
  distinct(location, date, .keep_all = TRUE)

## Welcher %-Anteil der Geimpften in Frankreich ist am 24.05.2021 vollständig
## geimpft?

impf_FR <- Covid %>% 
  select(location, date, total_vaccinations, people_fully_vaccinated) %>% 
  filter(location == "France", date == "2021-05-24") %>% 
  mutate(proz.anteil = people_fully_vaccinated / total_vaccinations * 100)
