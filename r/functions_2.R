# interne Funktionen für functions_1.R



# 1) Spalte sicher aus Datensatz auslesen
get_var <- function(data, var) {
  if (!is.data.frame(data)) stop("data muss ein data.frame sein.")
  if (!is.character(var) || length(var) != 1) stop("var muss ein einzelner Spaltenname sein.")
  if (!(var %in% names(data))) stop(paste0("Variable '", var, "' existiert nicht im Datensatz."))
  data[[var]]
}

# 2) Fehlende Werte entfernen (NA)

remove_na <- function(x) {
  x[!is.na(x)]
}

# 3) Kategoriale Variablen bereinigen und faktorisieren



clean_factor <- function(x) {
  if (is.factor(x)) return(x)
  x <- as.character(x)
  x[x == ""] <- NA
  factor(x)
}

# 4) Variablentyp erkennen (metrisch vs. kategorial)


var_type <- function(x) {
  if (is.numeric(x) || is.integer(x)) {
    ux <- unique(x[!is.na(x)])
    if (length(ux) > 2) return("metric")
  }
  return("categorical")
}










