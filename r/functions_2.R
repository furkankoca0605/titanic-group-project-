# interne Funktionen für functions_1.R

(weitere kommem noch)

get_var <- function(data, var) {
  if (!is.data.frame(data)) stop("data muss ein data.frame sein.")
  if (!is.character(var) || length(var) != 1) stop("var muss ein einzelner Spaltenname sein.")
  if (!(var %in% names(data))) stop(paste0("Variable '", var, "' existiert nicht im Datensatz."))
  data[[var]]
}


remove_na <- function(x) {
  x[!is.na(x)]
}
