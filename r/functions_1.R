source("functions_2.R")
# (i)			deskriptive Statistiken (metrische Variablen)

# metr.stat - gibt deskriptive Statistiken für eine metrische Variable x aus
metr_stat = function(x){
  if(var_type(x) != "metric") stop("x muss eine metrische Variable sein")
  
  x = remove_na(x)
  
  # einfacher Vektor mit Min, Max, Mean, Median, Quart, Sd

  c(Min = min(x), Max = max(x), Mean = mean(x), Median = median(x), 
        Quart = quantile(x)[c(2, 4)], StandDev = sd(x))

  # Histogramm
  
  hist(x)  ### Name im Plot hinzufügen (aus Datensatz herauslesen?)!
  
  
  # Boxplot
  
  boxplot(x)    ### Name im Plot hinzufügen (aus Datensatz herauslesen?)!

}


# (ii)		deskriptive Statistiken (kategoriale Variablen)


# (iii)		deskriptive bivariate Statistiken (zwei kategoriale Variablen)


# (iv)		deskriptive bivariate Statistiken (kategoriale & dichotome Variablen)

# met_dic_stat   - gibt deskriptive Statistiken für Zusammenhang einer 
#                  metrischen Variable x und einer dichotomen Variable y aus
met_dic_stat = function(x, y){
  if(var_type(x) != "metric") stop("x muss eine metrische Variable sein")
  if(var_type(y) != "categorical") stop("y muss eine dichotome Variable sein")
  
  x = remove_na(x)
  y = clean_factor(y)

  
  # (Scatter-)Plot mit zwei versch. Farben

  plot(x[y == unique(y)[1]], col = "blue")    ### xlim, ylim fehlen + Beschriftung
  points(x[y == unique(y)[1]], col = "green")


  # doppeltes Histogramm

  par(mfrow = c(1,2))
  hist(x[y == unique(y)[1]])    ### auch hier Beschriftung hinzufügen
  hist(x[y == unique(y)[2]])
  par(mfrow = c(1,1))
  

  # Zwei Boxplots nebeneinander

  boxplot(x ~ y) ### unvollständig, Beschriftung etc. fehlt
  
}



# (v)			geeignete Visualisierung von drei/vier kategorialen Variablen


