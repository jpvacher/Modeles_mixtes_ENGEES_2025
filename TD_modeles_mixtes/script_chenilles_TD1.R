#----------------------------------------------------------------------------#
#Script rédigé par Jean-Yves Barnagaud
# version 2023
# ce script permet la replication de l'exemple du cours sur les modeles mixtes
#mise à jour JP Vacher 2025
#----------------------------------------------------------------------------#

rm(list=ls()) #pour effacer les données chargées

# CHARGER LES PACKAGES ####
library(sf) #pour traiter les données géographiques (SIG)
library(ggplot2) #pour les figures
library(viridis) #pour les couleurs vertes dans ggplot2
library(ggeffects) #pour fonction ggpredict
library(questionr) #pour odds.ratio function
library(lme4) #pour glmer
library(broom.mixed) #pour fonction tidy
library(DHARMa) # pour fonction testDispersion
library(sjPlot) #pour fonction plot_model
library(MuMIn) #pour fonction r.squaredGLMM


#-----------------------------------------------------------#
#### Le modele lineaire mixte : chenille processionnaire ####
#-----------------------------------------------------------#

# CHARGER LES DONNEES ####
#getwd() #permet de voir dans quel directory on se trouve
setwd("TD_modeles_mixtes") #à taper si on n'est pas dans le bon directory
chenilles <- read.csv2("chenille_processionnaire.csv",row.names=1, encoding="UTF-8")
chenilles$prop_attaq <- chenilles$nbpinsattac / chenilles$nbpins

# EXPLORATION DES DONNEES ####

## carte des regions ####
regions <- st_read("regions.geojson")

ggplot(regions) +
  geom_sf(aes(fill=jointure_g))+
  geom_point(data=chenilles,mapping=aes(x=longitude,y=latitude,colour=region),size=2)+
  scale_color_viridis(discrete=TRUE) +
  scale_fill_viridis(discrete=TRUE) + 
  labs(color="Régions")+ 
  theme_classic()

## graph proportion d'arbres infestes par an ####

ggplot(chenilles) +
  aes(x = factor(annee), 
      y = prop_attaq, 
      group = placette, 
      color = region) +
  geom_line(show.legend = FALSE) +
  labs(x = "Années", y = "Proportion de pins infestés") +
  facet_wrap(~region, ncol = 1) + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90))+
  scale_color_viridis_d()

#Aucune tendance claire ne se dégage à échelle régionale
#Ce qui nous intéresse, c’est la tendance sur l’ensemble de l’aire d’étude (et non région par région)
#Ceci d’autant plus que certaines régions sont mieux échantillonnées que d’autres
#Et que certaines placettes sont mieux échantillonnées que d’autres


## on rescale les annees par commodite ####
chenilles$annee_resc <- chenilles$annee-min(chenilles$annee)+1

#GLM ####

## on commence juste sur une region ####
chenilles.nord <- subset(chenilles,region=="Centre Nord semi-oceanique")
mod.nord <- glm(cbind(nbpinsattac,nbpins-nbpinsattac)~annee_resc,family=binomial,data=chenilles.nord)

par(mfrow = c(2,2))
plot(mod.nord)
summary(mod.nord)

#p.nord <- ggpredict(mod.nord,terms = "annee_resc") %>%
#  plot(residuals = T, log.y = T)
#p.nord

## GLM sur toutes les regions ###
mod0 <- glm(cbind(nbpinsattac,nbpins-nbpinsattac)~annee_resc,family=binomial,data=chenilles)

## residus ####
par(mfrow=c(2,2))
plot(mod0)

## interpretation ####
summary(mod0)
odds.ratio(mod0)

## cartographie des residus ####
chenilles$res.mod0 <- residuals(mod0)
#p0 <- plot(ggpredict(mod0, terms = c("annee_resc")), residuals = T)

p1a <- ggplot(regions) +
  geom_sf() +
  geom_point(data = chenilles, 
             mapping = aes(x = longitude, y = latitude, colour = res.mod0),
             size = 2) +
  scale_color_viridis(discrete = F) + 
  labs(color = "Résidus du \n GLM binomial") + 
  theme_classic()

p1b <- ggplot(chenilles) +
  aes(x = placette, y = res.mod0) +
  geom_boxplot() +
  labs(x = "Placettes", y = "Résidus du GLM binomial") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90))

p1a + p1b

# GLMM ####

#On veut une estimation populationnelle de la tendance, tenant compte de la stratification
#Le but va être de représenter l’emboîtement des strates dans le modèle
#Pour ça, le préalable est de repérer quel est cet emboîtement : 
#Les données annuelles sont échantillonnées dans des placettes
#Les placettes sont réparties aléatoirement dans des régions
#Les régions sont réparties dans la zone d’étude


## GLMM avec effet placettes dans GRECO ####
mod2 <- glmer(cbind(nbpinsattac,nbpins-nbpinsattac)~annee_resc+(1|region/placette),family=binomial,data=chenilles)
#On remarque que l'on part de la strate la plus proche de la population vers la strate la plus proche de l'individu

summary(mod2)

# residus ####

plot(mod2, type=c("p","smooth"), col.line=1)
plot(mod2,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1,xlab="Valeurs prédites",ylab="sqrt(abs(résidus))")

plot(mod2,xlab="Valeurs prédites",ylab="Résidus de Pearson du GLMM")
#En théorie, on devrait vérifier les résidus de chaque strate : en pratique, on ne vérifie généralement que ceux de la première
#L’hétéroscédasticité détectée à droite n’est en réalité liée qu’à une petite vingtaine de points – on laisse tomber


#qqnorm(scale(residuals(mod2)))
#abline(0,1)

## interpretation ####
summary(mod2)

##Odds – ratios (IC de Wald, approximatif mais rapide) ####
tidy(mod2,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="Wald")

##Odds – ratios (IC par profilage, plus correct mais lent) ####
tidy(mod2,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

## surdispersion ####
simulationOutput <- simulateResiduals(fittedModel = mod2)
testDispersion(simulationOutput)

simulationOutput <- simulateResiduals(fittedModel = mod2, re.form = NULL)
testDispersion(simulationOutput,alternative="greater")
testDispersion(simulationOutput, alternative = "less", plot = FALSE) # seulement sous-dispersion
testDispersion(simulationOutput, alternative = "greater", plot = FALSE) # seulement surdispersion

## graphiques de résultats : effets fixes ####
p3 <- plot(ggpredict(mod2,terms="annee_resc"),residuals=T)+
  labs(x="Années",y="Taux d'infestation",title="")
p3

p4 <- plot(ggpredict(mod2,type="re"),residuals=T)
p4

p5 <- plot(ggpredict(mod2,terms=c("annee_resc","region"),type="re"),residuals=T)
p5

p6 <- plot(ggpredict(mod2,terms=c("annee_resc","placette"),type="re"),residuals=T)
p6

## effets aleatoires ####
plot_model(mod2, type = "re", show.values = TRUE)

## R2 ###
r.squaredGLMM(mod2)

## figure sur effets aleatoires ####
rpoints <- st_sample(regions, 700) %>% # random points, as a list ...
  st_sf() %>%  # ... to data frame ...
  st_transform(4326)  # ... and a metric CRS

ggplot(regions) +
  geom_sf() + 
  geom_sf(data = subset(regions, jointure_g %in% c("A", "B", "F", "G", "J")), 
          aes(fill = jointure_g)) +
  geom_point(data = chenilles, 
             mapping = aes(x = longitude, y = latitude), 
             colour = "red", 
             pch = 17, 
             size = 2) +
  geom_sf(data = rpoints, pch = 3, col = 'white', alpha = 0.67) +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) + 
  labs(color = "Régions") + 
  theme_classic() +
  theme(legend.position = "none")

#La structure de l’échantillonnage est hiérarchique : on cherche à représenter cette hiérarchie
#On fait l’hypothèse que notre réseau de placettes est un tirage aléatoire dans une plus grande population de placettes
#Donc, on ne connaît pas tous les niveaux de la variable placette, mais juste quelques-uns qu’on pense représentatifs de l’ensemble de la population de placettes
#Idem pour les régions



#Un intérêt du modèle hiérarchique : la propagation des erreurs
#L’effet des régions déficitaires est tamponné par les autres régions dans chaque couche stochastique
