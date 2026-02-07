# (i)			deskriptive Statistiken (metrische Variablen)


# (ii)		deskriptive Statistiken (kategoriale Variablen)


# (iii)		deskriptive bivariate Statistiken (zwei kategoriale Variablen)


# (iv)		deskriptive bivariate Statistiken (kategoriale & dichotome Variablen)


# (v)			geeignete Visualisierung von drei/vier kategorialen Variablen

# Visualisierung für kategoriale Variablen Folgt:
library(tidyverse)

#Titanic Data Frame aus 1 einlesen
  df<- titanic

#Funktion für Variablen 
plot_cats <- function(data,
                      x_var,
                      fill_var,
                      facet_var = NULL,
                      facet2_var = NULL,
                      position = "fill") {
  
  vars <- c(x_var, fill_var, facet_var, facet2_var)
  vars <- vars[!is.null(vars)]
  
#Fehlende Variablen erkennen 
  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Diese Variablen fehlen im Datensatz: ", paste(missing_vars, collapse = ", "))
  }
  
  #Erstellen von einem Diagramm vorbereiten
  p <- ggplot(df, aes(x = .data[[x_var]], fill = .data[[fill_var]])) +
    geom_bar(position = position, color = "white") +
    labs(
      x = x_var,
      fill = fill_var,
      y = ifelse(position == "fill", "Anteil", "Anzahl")
    ) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank())
  
  # 3te und 4te Variable für Zeilen und Spalten
  if (!is.null(facet_var) && !is.null(facet2_var)) {
    p <- p + facet_grid(rows = vars(.data[[facet_var]]),
                        cols  = vars(.data[[facet2_var]]))
  }
  
  p
}

#Anwendung mit Variablen
plot_cats(
  data = titanic,
  x_var = "Pclass",
  fill_var = "Survived",
  facet_var = "Sex",
  facet2_var = "Embarked"
)


