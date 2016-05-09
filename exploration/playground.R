library(data.table)
library(R.utils)
library(dplyr)

if (!file.exists("./data/train")) {
    gunzip("./data/train.gz")
}
if (!file.exists("./data/test")) {
    gunzip("./data/test.gz")
}
if (!file.exists("./data/train")) {
    gunzip("./data/sample_submission.gz")
}
if (!file.exists("./data/destinations")) {
    gunzip("./data/destinations.gz")
}

# train <- fread("./data/train")
test <- fread("./data/test")
sample_submission <- fread("./data/sample_submission")

nbUsers <- length(unique(train$user_id))
selectedUsers <- sample(nbUsers, size = 10000)
selectedLines <- which(train$user_id %in% selectedUsers)
TRAIN <- train[selectedLines, ]

save(TRAIN, file = "./data/TRAIN10k.RData")

# ------ Statistiques descriptives du jeu de données --------------------------

load("./data/TRAIN10k.RData")

TRAIN <- TRAIN[, c("date", "srch_ci", "srch_co") := 
                   list(as.Date(substr(date_time, 1, 10)),
                        as.Date(srch_ci),
                        as.Date(srch_co))]
TRAIN <- TRAIN[, c("delai_ci", "duree_sejour") :=
                   list(srch_ci - date,
                        srch_co - srch_ci)]
TRAIN <- TRAIN[, mois := months(srch_ci)]
TRAIN <- TRAIN[, adultchild := paste(srch_adults_cnt,
                                     srch_children_cnt,
                                     sep = "-")]

min(TRAIN$date_time)
max(TRAIN$date_time)
# Dates de 2013 à 2014
hist(TRAIN$date, breaks = "days", main = "Date de visite sur le site",
     xlab = "Date")
hist(TRAIN$date, breaks = "months", main = "Date de visite sur le site",
     xlab = "Date")

table(TRAIN$site_name)
length(table(TRAIN$site_name))
# 41 sites différents dans ce dataset. Probablement plus dans le jeu complet.

table(TRAIN$posa_continent)
max(table(TRAIN$posa_continent)) / nrow(TRAIN)
# 5 continents différents. 1 continent compte pour 75% des données

table(TRAIN$user_location_country)
# Près de 250 pays

length(unique(TRAIN$user_location_region))
# 660 régions

length(unique(TRAIN$user_location_city))
# 6980 villes. Presque une par user.

summary(TRAIN$orig_destination_distance)
sum(is.na(TRAIN$orig_destination_distance)) / nrow(TRAIN)
# Min  | 1st qu | med  | mean | 3rd  | Max   | NA
# 0.01 | 296.60 | 1105 | 1963 | 2524 | 11660 | 116591
# 37 % de données manquantes

summary(unclass(table(TRAIN$user_id)))
# En moyenne 32 lignes par individus
# Min = 2, Max = 425, Mediane = 15

table(TRAIN$is_mobile)[1] / nrow(TRAIN)
# 86 % non-mobile ; 14 % mobile

table(TRAIN$is_package)[1] / nrow(TRAIN)
# 75 % non-package ; 25% package

length(unique(TRAIN$channel))
table(TRAIN$channel)
# 11 channels

summary(as.numeric(TRAIN$delai_ci))
sum(TRAIN$delai_ci < 0, na.rm = T)
# Les gens s'y prennent 55 jours à l'avance en moyenne
# Median = 31, Max = 2200
# Beaucoup de -1 : Explication par le décalage horaire (heure très matinale)
# Des valeurs absurdes : Inversion MM-DD avec DD-MM
# Proposition : Virer ces valeurs absurdes.

table(TRAIN$mois)
# Check-ins plutôt équilibrés dans l'année, avec seulement légèrement
# plus de réservations en juin-juillet-août

summary(as.numeric(TRAIN$duree_sejour))
sum(TRAIN$duree_sejour < 0, na.rm = T)
# Valeurs aberrantes avec inversion MM-DD et DD-MM

summary(TRAIN$srch_adults_cnt)
sum(TRAIN$srch_adults_cnt == 2) / nrow(TRAIN)
sum(TRAIN$srch_adults_cnt == 1) / nrow(TRAIN)
table(TRAIN$srch_adults_cnt)
# 65 % avec 2 adultes. 21 % avec 1 adulte.

table(TRAIN$srch_children_cnt)
sum(TRAIN$srch_children_cnt == 0) / nrow(TRAIN)
# 79 % avec 0 enfant.

round(sort(table(TRAIN$adultchild), decreasing = T) / nrow(TRAIN), 2)
# 2-0  1-0  2-1  2-2  4-0
# 0.51 0.19 0.07 0.06 0.04

x <- table(TRAIN$srch_rm_cnt)
barplot(x, col = "salmon", main = "Nombre de chambres")

length(table(TRAIN$srch_destination_id))
x <- sort(table(TRAIN$srch_destination_id), decreasing = T)
barplot(x[x / nrow(TRAIN) > 0.005], col = "pink", main = "Top destinations")

x <- table(TRAIN$srch_destination_type_id)
barplot(x, col = "tomato", main = "Types de destination")

x <- table(TRAIN$hotel_continent)
barplot(x, col = "tomato", main = "Continent de l'hôtel")

x <- sort(table(TRAIN$hotel_country), decreasing = T)
barplot(x[x / nrow(TRAIN) > 0.01], col = "tomato", main = "Pareto Pays hotel")

x <- sort(table(TRAIN$hotel_market), decreasing = T)
barplot(x[x / nrow(TRAIN) > 0.01], col = "tomato", main = "Hotel market")

x <- sort(table(TRAIN$hotel_cluster), decreasing = T)
barplot(x, col = "lightblue", main = "Hotel clusters")





































