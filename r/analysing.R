# Anwenden der Funktionen aus functions_1.R 

source("r/functions_1.R")
source("r/functions_2.R")
titanic <- read.csv("data/titanic_clean.csv")

## Analyse metrische Variablen 
metr_stat(titanic, "Age")
metr_stat(titanic, "Fare")

## Analyse kategorielle Variablen (z.B. Klasse, Geschlecht, Einstiegshafen)
vars_cat <- c("Deck", "Sex", "Embarked", "Side", "Survived", "Title", "Pclass")
freq_tab <- freq_missing(titanic, vars_cat, include_na = TRUE)
freq_tab

## Vergleich kategoriell - kategoriell (z.B. Überlebt/Geschlecht, Überlebt/Deck, ...)
#Survived als Variable vorgegeben und ist bereits eingegeben
surv_sex <- surv_wide(titanic, "Sex")
surv_sex

chi_surv_sex <- pearson_cat(titanic, "Survived", "Sex")
chi_surv_sex

## Vergleich metrisch - dichotom
met_dic_stat(titanic, "Age", "Survived")
met_dic_stat(titanic, "Fare", "Survived")

## Vergleich mehrere kategorielle Variablen


## Vergleich metrisch - kategoriell
met_cat_stat(titanic, "Fare", "Embarked")
met_cat_stat(titanic, "Fare", "Pclass")

##Zur Visualisierung
#Anwendung mit Variablen
plot_cats(
  data = titanic,
  x_var = "Pclass",
  fill_var = "Survived",
  facet_var = "Sex",
  facet2_var = "Embarked"
)
             
