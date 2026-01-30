titanic <- read.csv("data/titanic_raw.csv", stringsAsFactors = FALSE)

# Anrede aus dem Name extrahieren
titanic$Title <- sub(".*,\\s*([^\\.]+)\\..*", "\\1", titanic$Name)

# die Titel zusammenfassen
titanic$Title[titanic$Title %in% c("Mlle", "Ms")] <- "Miss"
titanic$Title[titanic$Title %in% c("Mme")] <- "Mrs"

# Adelstitel
titanic$Title[titanic$Title %in%
                c("Lady", "the Countess", "Sir", "Don", "Jonkheer")] <- "Noble"

# Militär / akademisch / religiös
titanic$Title[titanic$Title %in%
                c("Capt", "Col", "Major", "Dr", "Rev")] <- "Officer"

# Als factor speichern
titanic$Title <- factor(titanic$Title)

# imputieren 
for (t in levels(titanic$Title)) {
  median_age <- median(titanic$Age[titanic$Title == t], na.rm = TRUE)
  titanic$Age[is.na(titanic$Age) & titanic$Title == t] <- median_age
}

# Cabin: leere Strings -> NA
titanic$Cabin[titanic$Cabin == ""] <- NA

# Deck = erster Buchstabe der Cabin
titanic$Deck <- substr(titanic$Cabin, 1, 1)
titanic$Deck <- factor(titanic$Deck)

# Kabinennummer extrahieren
cabin_number <- as.numeric(gsub("[^0-9]", "", titanic$Cabin))

# Schiffsseite bestimmen
titanic$Side <- ifelse(
  is.na(cabin_number),
  NA,
  ifelse(cabin_number %% 2 == 0, "Starboard", "Port")
)
titanic$Side <- factor(titanic$Side)

# Zielvariable
titanic$Survived <- factor(
  titanic$Survived,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

# Geschlecht
titanic$Sex <- factor(titanic$Sex)

# Embarked: leere Strings -> NA, dann Factor
titanic$Embarked[titanic$Embarked == ""] <- NA
titanic$Embarked <- factor(titanic$Embarked)

# Passagierklasse als geordneter Faktor
titanic$Pclass <- factor(
  titanic$Pclass,
  levels = c(1, 2, 3),
  ordered = TRUE
)

# Unnötige Variablen entfernen
titanic <- titanic[, !(names(titanic) %in% c("PassengerId", "Name", "Ticket", "Cabin"))]

# Cleanen Datensatz speichern
write.csv(titanic, "data/titanic_clean.csv", row.names = FALSE)
