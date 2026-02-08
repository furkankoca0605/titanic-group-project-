source("r/functions_2.R")
titanic <- read.csv("data/titanic_clean.csv")

# (i)			deskriptive Statistiken (metrische Variablen)

# metr.stat - gibt deskriptive Statistiken für eine metrische Variable v aus einem Datensatz d aus
metr_stat = function(d, v){
  x = get_var(d, v)
  if(var_type(x) != "metric") stop("x muss eine metrische Variable sein")
  name = as.character(substitute(v))
  x = remove_na(x)

  
  # einfacher Vektor als Übersicht
  summary(x)

  # Histogramm
  hist(x, xlab = "", ylab = name, main = paste("Histogram of", name)) 
  
  # Boxplot
  boxplot(x, ylab = name, main = paste("Boxplot of", name)) 
}


# (ii)		deskriptive Statistiken (kategoriale Variablen)

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
write.csv2(final, "data/titanic_ii.csv", row.names = FALSE)

# (iii)		deskriptive bivariate Statistiken (zwei kategoriale Variablen)

#data frame aus Skript 1 in neuen df zum arbeiten
df <- titanic

#Variablen die benutzt werden sollen als Vektor
vars <- c("Survived","Pclass","Sex","Embarked","Title","Deck","Side")

#Sorgt dafür, dass auch Variablen die nicht ein Faktor sind benutzt werden können
df[vars] <- lapply(df[vars], factor)

#Funktion wird definiert 
surv_wide <- function(df, var, digits=3){
  x <- addNA(df[[var]])
  y <- addNA(df$Survived)
  tab <- table(level=x, survived=y, useNA="ifany")
  n_tot <- rowSums(tab)
  
  #Trennen überlebt oder nicht  
  n_yes <- if ("Yes" %in% colnames(tab)) tab[, "Yes"] else rep(0, nrow(tab))
  n_no  <- if ("No"  %in% colnames(tab)) tab[, "No"]  else rep(0, nrow(tab))
  
  #Bennent die Spalten 
  data.frame(
    variable = var,
    level    = rownames(tab),
    n_total  = as.integer(n_tot),
    absolut_ja    = as.integer(n_yes),
    absolut_nein     = as.integer(n_no),
    relativ_ja    = round(as.numeric(n_yes / n_tot), digits),
    relativ_nein     = round(as.numeric(n_no  / n_tot), digits),
    stringsAsFactors = FALSE
  )
}

#Ruft die Funktion auf und speichrt den fertigen df
finaliii <- do.call(rbind, lapply(setdiff(vars,"Survived"), \(v) surv_wide(df, v)))
finaliii
#Weitere Funktion
#Korrelation mit NA cramer und Pearson
#Funktion wird definiert
pearson_cat <- function(df, v1, v2, include_na=TRUE, digits=3){
  x <- if (include_na) droplevels(addNA(df[[v1]])) else droplevels(df[[v1]])
  y <- if (include_na) droplevels(addNA(df[[v2]])) else droplevels(df[[v2]])
  tab <- table(x, y, useNA = if (include_na) "ifany" else "no")
# Nullränder enternen  
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]  
  
  tst <- suppressWarnings(chisq.test(tab, correct = FALSE))
  n <- sum(tab); r <- nrow(tab); k <- ncol(tab)
  V <- sqrt(as.numeric(tst$statistic) / (n * (min(r, k) - 1)))
  
  data.frame(var1=v1, var2=v2, n=n,
             chi2=round(as.numeric(tst$statistic), digits),
             df=as.integer(tst$parameter),
             p_value=round(tst$p.value, digits),
             cramers_v=round(V, digits),
             stringsAsFactors = FALSE)
}

# alle Kombinationen berechnen
pairs <- combn(vars, 2, simplify = FALSE)
corr_cat_all <- do.call(rbind, lapply(pairs, \(p) pearson_cat(df, p[1], p[2], include_na=TRUE)))

# finaliii + corr_cat_all untereinander binden und speichern
a <- transform(finaliii, source = "titaniciii")
b <- transform(corr_cat_all, source = "corr_cat_all")

cols <- union(names(a), names(b))
a[setdiff(cols, names(a))] <- NA
b[setdiff(cols, names(b))] <- NA

all_out <- rbind(a[cols], b[cols])

#Speichert die df als CSV dateien
write.csv2(finaliii, "C:\\Users\\nroes\\OneDrive\\R WA\\titanic_iii.csv",
           row.names = FALSE)
write.csv2(corr_cat_all, "C:\\Users\\nroes\\OneDrive\\R WA\\titanic_korrelation.csv",
           row.names = FALSE)

# (iv)		deskriptive bivariate Statistiken (kategoriale & dichotome Variablen)

# met_dic_stat   - gibt deskriptive Statistiken für Zusammenhang einer 
#                  metrischen Variable vm und einer dichotomen Variable vc aus einem Datensatz d aus
met_dic_stat = function(d, vm, vc){
  x = get_var(d, vm)
  y = get_var(d, vc)  
  if(var_type(x) != "metric") stop("x muss eine metrische Variable sein")
  if(var_type(y) != "categorical") stop("y muss eine dichotome Variable sein")
  names = as.character(c(substitute(vm), substitute(vc)))
  x = remove_na(x)
  y = clean_factor(y)
  uy = unique(y)

  
  # Scatter-Plot mit zwei versch. Farben
  plot(x = seq(0, 100, length.out = length(x[y == uy[1]])), y = x[y == uy[1]], ylim = range(x), 
       col = "blue", xlab = "", ylab = names[1])
  points(x = seq(0, 100, length.out = length(x[y == uy[2]])), y = x[y == uy[2]], col = "green")
  legend(x = 0, y = max(x), legend = uy, fill = c("blue", "green"))

  # doppeltes Histogramm
  par(mfrow = c(1, 2))
  hist(x[y == uy[1]], ylim = c(0, max(x)), xlab = uy[1], main = paste("Histogram of", uy[1], names[1]))
  hist(x[y == uy[2]], ylim = c(0, max(x)), xlab = uy[2], main = paste("Histogram of", uy[2], names[1]))
  par(mfrow = c(1, 1))
  
  # doppelter Boxplot
  boxplot(x ~ y, xlab = names[2], ylab = names[1], main = paste("Boxplot of", names[1]))   
}


# (v)			geeignete Visualisierung von drei/vier kategorialen Variablen

# Visualisierung für kategoriale Variablen Folgt:

library(tidyverse)

# Visualisierung für kategoriale Variablen
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
  
  #Titanic Data Frame aus 1 einlesen
  df<- titanic
  
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

