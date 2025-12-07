#Script écrit par XXX le XX.XX.XXXX
#mis à jour le XX.XX.XXXX par YYYY

#1. CHARGER LES PACKAGES ####

library(tidyverse) #pour manipuler les tables de données
library(sf) #pour traiter les données géoréférencées (SIG)
library(geodata) #pour charger les données worldclim et contours de pays depuis gadm. Nécessite d'être online.
library(GGally) #pour fonction ggpairs, permet de représenter les corrélations entre variables


#2. CHARGER LES DONNEES ####

triturus <- read_tsv("data_triturus.txt")

variables <- read_tsv("variables_envir.txt")

france <- gadm(country='FRA', level = 2, path = "gadm_data") %>% 
  st_as_sf()

region.nat <- st_read("Regions_Naturelles_Grand_Est_ODNAT_2018/Regions_Naturelles_N2_GE_V10_ODONAT_2018.shp")

#3. EXPLORATION RAPIDE DES DONNEES ####

triturus
length(unique(triturus$ID_pond))
summary(triturus)


variables
colnames(variables)
length(unique(variables$ID_pond))
summary(variables)

france

region.nat

#4. MODIFICATIONS ET AJOUT DE VARIABLES ENVIRONNEMENTALES ####

##4.1. On passe les méthodes et l'ensoleillement en facteurs, et les dates en date ####
variables <- variables %>% 
  mutate(method1_2021 = as.factor(method1_2021),
         method2_2021 = as.factor(method2_2021),
         method3_2021 = as.factor(method3_2021),
         method1_2024 = as.factor(method1_2024),
         method2_2024 = as.factor(method2_2024),
         method3_2024 = as.factor(method3_2024),
         date1_2021 = as.Date(date1_2021, format = "%d/%m/%Y"),
         date2_2021 = as.Date(date2_2021, format = "%d/%m/%Y"),
         date3_2021 = as.Date(date3_2021, format = "%d/%m/%Y"),
         date1_2024 = as.Date(date1_2024, format = "%d/%m/%Y"),
         date2_2024 = as.Date(date2_2024, format = "%d/%m/%Y"),
         date3_2024 = as.Date(date3_2024, format = "%d/%m/%Y"),
         ensoleillement = as.factor(ensoleillement))
summary(variables)

##4.2. On intègre les régions naturelles ####
###L'idée est d'assigner chaque mare à une région naturelle
###On commence par créer une couche SIG des mares dans le même système de projection géographique que les régions naturelles :
mares <- variables %>% 
  select(ID_pond, X_L93, Y_L93) %>% 
  st_as_sf(coords = c("X_L93", "Y_L93")) %>% #On transforme le tibble en élément géoréférencé en assignant des coordonnées géographiques.
  st_set_crs(2154) #on définit le système de projection correspondant à Lambert 93

##4.3 On intersecte la couche des mares avec les régions naturelles ####
mares.regnat <- mares %>% 
  st_intersection(region.nat) %>% 
  select(ID_pond, ID_N2, Nom) #On sélectionne les colonnes d'intérêt pour simplifier la couche

##4.4 On intègre les informations de région naturelles dans le tableau de variables ####
variables <- variables %>% 
  mutate(RegNat = as.factor(mares.regnat$Nom[match(ID_pond, mares.regnat$ID_pond)]),
         RegNat_code = as.factor(mares.regnat$ID_N2[match(ID_pond, mares.regnat$ID_pond)]))
summary(variables)


#5. VISUALISATION CARTOGRAPHIQUE DES MARES ####
##5.1 Ajuster le syst de projection des mares ####
mares.wgs84 <- mares %>%
  st_transform(4326) #transformer en WGS84

##5.2 Ajuster le syst de projection des régions naturelles ####
region.nat.wgs84 <- region.nat %>%
  st_transform(4326)

##5.3 Sélectionner les départements du Grand Est ####
gd.est <- france %>% 
  filter(NAME_1 == "Grand Est")
gd.est

##5.4 Plot mares ####
ggplot() +
  geom_sf(data = gd.est) +
  geom_sf(data = region.nat, aes(fill = ID_N2)) +
  geom_sf(data = mares.wgs84, size = .5)

#OK, ça a l'air d'avoir fonctionné
#On remarque un déséquilibre dans l'échantillonnage par strate


#6. EXPLORATION VARIABLES ENVIRONNEMENTALES ####
colnames(variables)

ggpairs(variables, columns = c(38:46), #sélection des colonnes qui correspondent aux variables environnementales
        upper=list(continuous = wrap("cor", size = 2)), #paramètres des cases en haut de la diagonale
        lower = list(continuous = wrap("points", size = .3))) + #paramètres des cases en bas de la diagonale
  theme(axis.text = element_text(size = 6), strip.text = element_text(size = 6)) #paramètres du texte des axes et des titres


#À VOUS DE CONTINUER LE DEROULÉ ! ####