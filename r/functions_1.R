source("functions_2.R")

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
  boxplot(x, horizontal = TRUE, xlab = name, main = paste("Boxplot of", name)) 
}


# (ii)		deskriptive Statistiken (kategoriale Variablen)


# (iii)		deskriptive bivariate Statistiken (zwei kategoriale Variablen)


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
  boxplot(x ~ y, horizontal = TRUE, xlab = names[1], ylab = names[2], main = paste("Boxplot of", names[1]))   
}


# (v)			geeignete Visualisierung von drei/vier kategorialen Variablen


