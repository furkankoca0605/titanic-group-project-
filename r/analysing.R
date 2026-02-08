# Anwenden der Funktionen aus functions_1.R 

source("r/functions_1.R")
titanic <- read.csv("data/titanic_clean.csv")

## Analyse metrische Variablen 
metr_stat(titanic, Age)
metr_stat(titanic, Fare)

## Analyse kategorielle Variablen (z.B. Klasse, Geschlecht, Einstiegshafen)
freq_missing(titanic,var)

## Vergleich kategoriell - kategoriell (z.B. Überlebt/Geschlecht, Überlebt/Deck, ...)
#Survived als Variable vorgegeben und ist bereits eingegeben
surv_wide(titanic, var())
pearson_cat(titanic, var1, var2)

## Vergleich metrisch - dichotom
met_dic_stat(titanic, Age, Survived)
met_dic_stat(titanic, Fare, Survived)

## Vergleich mehrere kategorielle Variablen


## Vergleich metrisch - kategoriell
met_cat_stat(titanic, Fare, Embarked)
met_cat_stat(titanic, Fare, Pclass)

##Zur Visualisierung
#Anwendung mit Variablen
plot_cats(
  data = titanic,
  x_var = "Pclass",
  fill_var = "Survived",
  facet_var = "Sex",
  facet2_var = "Embarked"
)
             
