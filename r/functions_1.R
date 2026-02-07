# (i)			deskriptive Statistiken (metrische Variablen)


# (ii)		deskriptive Statistiken (kategoriale Variablen)
#Verweist auf andere Skripte, um die notwendigen Dateien zu finden
source("C:\\Users\\nroes\\OneDrive\\R WA\\functions_2.R")
source("C:\\Users\\nroes\\OneDrive\\R WA\\data_cleaning.R")

#data frame aus Skript 1 in neuen df zum arbeiten
df <- titanic

#Variablen die benutzt werden sollen als Vektor
vars <- c("Deck","Sex","Embarked","Side","Survived","Title","Pclass")

#Sorgt dafür, dass auch Variablen die nicht ein Faktor sind benutzt werden können
df[vars] <- lapply(df[vars], factor)

#Funktion wird definiert 
freq_missing <- function(df, vars, include_na = TRUE, digits = 3) {
  out <- lapply(vars, function(v) {
    #Liste pro Variable
    x <- df[[v]]
    
    #Berechnung für Gesamte Anzahl und Fehlende Werte    
    fehlende_n  <- sum(is.na(x))
    n_total <- length(x)
    #BErechnung für relative und Absolute Häufigkeit    
    tab  <- table(x, useNA = if (include_na) "ifany" else "no")
    prop <- prop.table(tab)
    
    #Bennent die Spalten
    data.frame(
      variable  = v,
      level     = names(tab),
      anteil_absolut      = as.integer(tab),
      anteil_relativ     = round(as.numeric(prop), digits),
      n_fehlend = fehlende_n,
      n_total   = n_total,
      stringsAsFactors = FALSE
    )
  })
  #Alle einzelnen VAriablenlisten zu einer 
  do.call(rbind, out)
}
#Ruft die Funktion auf und speichrt den fertigen df
final <- freq_missing(df, vars, include_na = TRUE)
final

#Speichert den df als CSV datei
write.csv2(final, "C:\\Users\\nroes\\OneDrive\\R WA\\titanic_ii.csv", row.names = FALSE)

# (iii)		deskriptive bivariate Statistiken (zwei kategoriale Variablen)


# (iv)		deskriptive bivariate Statistiken (kategoriale & dichotome Variablen)


# (v)			geeignete Visualisierung von drei/vier kategorialen Variablen

# Visualisierung für kategoriale Variablen Folgt:
library(tidyverse)
source("functions_2.R")
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


