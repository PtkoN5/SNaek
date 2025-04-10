# R Packages die wir brauchen für Abgabe 2
install.packages("testthat")     # Muss für 3.4 benutzt werden
install.packages("ggplot2")      # Muss für 4.2/4.4 benutzt werden
install.packages("plotly")       # Muss Benutz werden für 4.6
install.packages("dplyr")        # Muss für 4.5 benutzt werden
library(readr)



#3.1
n <- 8 # Anzahl der Personen die Wichteln
ierations <- 1000 # Anzahl der Iterationen
counter <- 0 # Zähler für die Anzahl der Personen die ihr eigenes Geschenk haben

for (i in 1:ierations) {
  geschenke <- sample(1:n, n, replace = FALSE) # Zufällige Zuordnung der Geschenke
  geschenke <-sum(geschenke == 1:n) # Anzahl der Personen die ihr eigenes Geschenk haben
  if (geschenke == 0) {
    counter <- counter + 1
  }
}

wahrscheinlichkeit <- counter / ierations
print(wahrscheinlichkeit)

#3.2
#Umbenenung der Funktion und hinzufügen von k als Parameter
wichtel_ungluecklich <- function(n, k, iterations = 1e6) {
  ungluecksfälle <- 0     #Zähler für die Anzahl der Personen die ihr eigenes Geschenk haben
  
  #Simulationsfunktion
  for (i in 1:ierations) {
    geschenke <- sample(1:n, n, replace = FALSE)
    
    geschenke <- sum(geschenke == 1:n)
    if (geschenke == 0) {
      counter <- counter + 1
    }
  }
  wahrscheinlichkeit <- counter / ierations # Berechnung der Wkeit für min. k Personen die ihr eigenes Geschenk haben
  return(wahrscheinlichkeit) 
}
#Berechnung von der Warscheinlichkeit, für 8 Personen und 2 ierations
wahrscheinlichkeit <- wichtel_ungluecklich(8, 2)
#Antwort auf die Wahrscheinlichkeit
wahrscheinlichkeit


#3.4
library(testthat)

# Funktion aus vorheriger Implementierung
wichtel_unglueck <- function(n, k, iterationen = 1e6) {
  erfolg <- 0  # Zählt die gültigen Fälle
  
  for (i in 1:iterationen) {
    geschenke <- sample(1:n)  # Zufällige Geschenkverteilung
    rueckgabe <- sum(geschenke == 1:n)  # Zählt Personen mit eigenem Geschenk
    
    if (rueckgabe <= k) {  # Prüft, ob höchstens k Personen ihr eigenes Geschenk bekommen
      erfolg <- erfolg + 1
    }
  }
  return(erfolg / iterationen)  # Wahrscheinlichkeit zurückgeben
}
# Testfälle definieren
test_that("wichtel_unglueck funktioniert korrekt", {
  
  # Testfall 1: Wenn k = n, sollte die Wahrscheinlichkeit 1 sein (immer erfüllt)
  expect_equal(wichtel_unglueck(5, 5, iterationen = 10000), 1, tolerance = 0.01)
  
  # Testfall 2: Wenn k = 0, sollte die Wahrscheinlichkeit nahe 1/e sein (ca. 0.3679 für große n)
  expect_equal(wichtel_unglueck(8, 0, iterationen = 100000), 1/exp(1), tolerance = 0.02)
  
  # Testfall 3: Wenn n = 1, sollte die Wahrscheinlichkeit 0 sein (ein Geschenk, immer zurück)
  expect_equal(wichtel_unglueck(1, 0, iterationen = 10000), 0, tolerance = 0.01)
  
  # Testfall 4: Wenn n groß ist, sollte die Wahrscheinlichkeit für k = 0 weiterhin etwa 1/e sein
  expect_equal(wichtel_unglueck(20, 0, iterationen = 100000), 1/exp(1), tolerance = 0.02)
  
})


#3.5 Einlesen der Daten und Visualisierung
setwd("CoMet") #Setzten der Workdirectory
data.frame <- read.csv(file = "bike_sharing_data_(with_NAs).csv") #Einlesen der CSV Datei
filtered_data <- data.frame[data.frame$group==65,] #Filtern der Daten nach Gruppe 65

head(filtered_data) #Anzeigen der ersten 6 Zeilen


anyNA(filtered_data)  #Überprüfung ob NA Werte vorhanden sind

sum(is.na(filtered_data)) #Anzahl der NA Werte

colSums(is.na(filtered_data)) #Anzahl der NA Werte in den Spalten

filtered_data[!complete.cases(filtered_data),] #Anzeigen der NA Werte


# Anzeigen und ersetzten der Fehlenden Werte
filtered_data$day_of_year #Anzeigen der NA Werte in der Spalte day_of_year
which(is.na(filtered_data$day_of_year)) #Anzeigen der NA Werte in der Spalte day_of_year
filtered_data$day_of_year[152] <- 152 #Ersetzen des NA Wertes
filtered_data$day_of_year[186] <- 186 #Ersetzen des NA Wertes

filtered_data$day_of_week #Anzeigen der NA Werte in der Spalte day_of_week
which(is.na(filtered_data$day_of_week)) #Anzeigen der NA Werte in der Spalte day_of_week  
filtered_data$day_of_week[11] <- 4 #Ersetzen des NA Wertes
filtered_data$day_of_week[72] <- 2 #Ersetzen des NA Wertes

filtered_data$month_of_year #Anzeigen der NA Werte in der Spalte month_of_year
which(is.na(filtered_data$month_of_year)) #Anzeigen der NA Werte in der Spalte month_of_year
filtered_data$month_of_year[207] <- 7 #Ersetzen des NA Wertes
filtered_data$month_of_year[234] <- 8 #Ersetzen des NA Werte

filtered_data$count #Anzeigen der NA Werte in der Spalte courses
which(is.na(filtered_data$count)) #Anzeigen der NA Werte in der Spalte courses
filtered_data$count[317] <- 80 #Ersetzen des NA Wertes

filtered_data$average_temperature #Anzeigen der NA Werte in der Spalte average_temperature
which(is.na(filtered_data$average_temperature)) #Anzeigen der NA Werte in der Spalte average_temperature
filtered_data$average_temperature[1] <- 44 #Ersetzen des NA Wertes
filtered_data$average_temperature[310] <- 51 #Ersetzen des NA Wertes

filtered_data$precipitation #Anzeigen der NA Werte in der Spalte precipitation
which(is.na(filtered_data$precipitation)) #Anzeigen der NA Werte in der Spalte precipitation
filtered_data$precipitation[179] <- 0.01 #Ersetzen des NA Wertes

filtered_data$windspeed #Anzeigen der NA Werte in der Spalte wind_speed
which(is.na(filtered_data$windspeed)) #Anzeigen der NA Werte in der Spalte wind_speed
filtered_data$windspeed[319] <- 9.20 #Ersetzen des NA Wertes

filtered_data$min_temperature #Anzeigen der NA Werte in der Spalte min_temperature
which(is.na(filtered_data$min_temperature)) #Anzeigen der NA Werte in der Spalte min_temperature
filtered_data$min_temperature[295] <- 45 #Ersetzen des NA Wertes

filtered_data$max_temperature #Anzeigen der NA Werte in der Spalte max_temperature
which(is.na(filtered_data$max_temperature)) #Anzeigen der NA Werte in der Spalte max_temperature
filtered_data$max_temperature[136] <- 70 #Ersetzen des NA Wertes


#test ob alle NAs ersetzt wurden 
anyNA(filtered_data) 

#Anpassung von Fahrenheit in Celsius
filtered_data$min_temperature <- (filtered_data$min_temperature -32) * 5/9
filtered_data$average_temperature <- (filtered_data$average_temperature -32)* 5/9
filtered_data$max_temperature <- (filtered_data$max_temperature - 32) * 5/9


monthly_counts <- aggregate(count ~ month_of_year, data = filtered_data, sum)

# 2. Bestimmung des Monats mit der höchsten Anzahl an Ausleihen
highest_month <- monthly_counts[which.max(monthly_counts$count), ]

# Ausgabe des Ergebnisses
print(paste("Monat mit der höchsten Anzahl ausgeliehener Fahrräder:",
            highest_month$month_of_year,  "mit",highest_month$count,"Ausleihen."))



write.csv(filtered_data, file = "Gruppe65") #Speichern der gefilterten Daten

#4.2
#Erstellen von Plots mit ggplot2
library(ggplot2)
ggplot(filtered_data, aes(x = average_temperature, y = count)) +
  geom_point() +
  labs(
    title = "Zusammenhang zwischen Temperatur und Fahrradausleihen",
    x = "Temperatur in °C",
    y = "Anzahl ausgeliehener Fahrräder" 
  ) +
  theme_minimal()

ggplot(filtered_data, aes(x = precipitation, y = count)) +
  geom_point() +
  labs(
    title = "Zusammenhang zwischen der Niederschlagsmenge und Fahrradausleihen",
    x = "Niederschlagsmenge",
    y = "Anzahl ausgeliehener Fahrräder" 
  ) +
  theme_minimal()


ggplot(filtered_data, aes(x = windspeed, y = count)) +
  geom_point() +
  labs(
    title = "Zusammenhang zwischen Windgeschwindigkeit in km/h und Fahrradausleihen",
    x = "Windgeschwindigkeit in km/h",
    y = "Anzahl ausgeliehener Fahrräder" 
  ) +
  theme_minimal()

ggplot(filtered_data, aes(x = date, y = count)) +
  geom_point() +
  labs(
    title = "Zusammenhang zwischen Zeit und Fahrradausleihen",
    x = "Zeit",
    y = "Anzahl ausgeliehener Fahrräder" 
  ) +
  theme_minimal()

#4.3
# Laden der benötigten Bibliotheken
library(ggplot2)

# 1. Erstellen von zwei Datensätzen
rainy_days <- filtered_data[filtered_data$precipitation > 0, ]  # Tage mit Niederschlag
non_rainy_days <- filtered_data[filtered_data$precipitation == 0, ]  # Tage ohne Niederschlag

# 2. Visualisierung der Daten
# Grafik für Tage mit Niederschlag
ggplot(rainy_days, aes(x = average_temperature, y = count)) +
  geom_point(color = "blue") +  # Punkte für die Anzahl der Ausleihen
  labs(title = "Anzahl ausgeliehener Fahrräder an Tagen mit Niederschlag",
       x = "Durchschnittstemperatur (°C)",
       y = "Anzahl ausgeliehener Fahrräder") +
  theme_minimal()

# Grafik für Tage ohne Niederschlag
ggplot(non_rainy_days, aes(x = average_temperature, y = count)) +
  geom_point(color = "green") +  # Punkte für die Anzahl der Ausleihen
  labs(title = "Anzahl ausgeliehener Fahrräder an Tagen ohne Niederschlag",
       x = "Durchschnittstemperatur (°C)",
       y = "Anzahl ausgeliehener Fahrräder") +
  theme_minimal()
#4.4
# 1. Visualisierung der Verteilung der Anzahl ausgeliehener Fahrräder
ggplot(filtered_data, aes(x = count)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Verteilung der Anzahl ausgeliehener Fahrräder",
       x = "Anzahl ausgeliehener Fahrräder",
       y = "Häufigkeit") +
  theme_minimal()

# 2. Visualisierung der Verteilung der Temperatur (in Grad Celsius)
ggplot(filtered_data, aes(x = average_temperature)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Verteilung der Durchschnittstemperatur (°C)",
       x = "Durchschnittstemperatur (°C)",
       y = "Häufigkeit") +
  theme_minimal()

# 3. Visualisierung der Verteilung der Niederschlagsmenge
ggplot(filtered_data, aes(x = precipitation)) +
  geom_histogram(binwidth = 0.1, fill = "lightcoral", color = "black") +
  labs(title = "Verteilung der Niederschlagsmenge",
       x = "Niederschlagsmenge (mm)",
       y = "Häufigkeit") +
  theme_minimal()

# 4. Visualisierung der Verteilung der Windgeschwindigkeit
ggplot(filtered_data, aes(x = windspeed)) +
  geom_histogram(binwidth = 1, fill = "lightyellow", color = "black") +
  labs(title = "Verteilung der Windgeschwindigkeit (km/h)",
       x = "Windgeschwindigkeit (km/h)",
       y = "Häufigkeit") +
  theme_minimal()
#4.5
# Laden der benötigten Bibliotheken
library(ggplot2)
library(dplyr)

# 1. Hinzufügen einer Jahreszeiten-Spalte
filtered_data <- filtered_data %>%
  mutate(season = case_when(
    month_of_year %in% c(3, 4, 5) ~ "Frühling",
    month_of_year %in% c(6, 7, 8) ~ "Sommer",
    month_of_year %in% c(9, 10, 11) ~ "Herbst",
    month_of_year %in% c(12, 1, 2) ~ "Winter"
  ))

# 2. Visualisierung der Verteilung der Anzahl ausgeliehener Fahrräder nach Jahreszeiten
ggplot(filtered_data, aes(x = count, fill = season)) +
  geom_density(alpha = 0.5) +  # Kerndichteschätzer mit Transparenz
  labs(title = "Verteilung der Anzahl ausgeliehener Fahrräder nach Jahreszeiten",
       x = "Anzahl ausgeliehener Fahrräder",
       y = "Dichte") +
  scale_fill_manual(values = c("Frühling" = "lightgreen", 
                               "Sommer" = "lightblue", 
                               "Herbst" = "orange", 
                               "Winter" = "lightcoral")) +
  theme_minimal()

#4.6
# Laden der benötigten Bibliotheken
library(plotly)

# 1. Erstellung des 3D-Scatterplots
fig <- plot_ly(data = filtered_data, 
               x = ~average_temperature, 
               y = ~windspeed, 
               z = ~count, 
               color = ~count,  # Farbe basierend auf der Anzahl ausgeliehener Fahrräder
               colors = colorRamp(c("blue", "red")),  # Farbskala von blau (wenig) nach rot (viel)
               type = "scatter3d", 
               mode = "markers") %>%
  layout(title = "3D-Scatterplot: Temperatur, Windgeschwindigkeit und Anzahl ausgeliehener Fahrräder",
         scene = list(
           xaxis = list(title = "Durchschnittstemperatur (°C)"),
           yaxis = list(title = "Windgeschwindigkeit (km/h)"),
           zaxis = list(title = "Anzahl ausgeliehener Fahrräder")
         ))

# 2. Anzeige des Plots
fig
