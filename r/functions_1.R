# (i)			deskriptive Statistiken (metrische Variablen)

# metr.stat - gibt deskriptive Statistiken für eine metrische Variable x aus
metr.stat = function(x){

  # einfacher Vektor mit Min, Max, Mean, Median, Quart, Sd

  v = c(Min = min(x), Max = max(x), Mean = mean(x), Median = median(x), 
        Quart = quantile(x)[c(2, 4)], StandDev = sd(x))

  # Boxplot
  
  b = boxplot(x)
  ### Name sollte noch in Boxplot rein...

  
  return({b; v})
}


# (ii)		deskriptive Statistiken (kategoriale Variablen)


# (iii)		deskriptive bivariate Statistiken (zwei kategoriale Variablen)


# (iv)		deskriptive bivariate Statistiken (kategoriale & dichotome Variablen)

# biv_kat_dit.stat - gibt deskriptive Statistiken für Zusammenhang einer 
#                    metrischen Variable x und einer dichotomen Variable y aus
biv_kat_dit.stat = function(x, y){

  # (Scatter-)Plot mit zwei versch. Farben




  # Säulendiagramm mit doppelten Säulen


  

  # Zwei Boxplots nebeneinander


  
}



# (v)			geeignete Visualisierung von drei/vier kategorialen Variablen


