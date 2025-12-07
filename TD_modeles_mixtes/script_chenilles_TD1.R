#----------------------------------------------------------------------------#
#Script rédigé par Jean-Yves Barnagaud
# version 2023
# ce script permet la replication de l'exemple du cours sur les modeles mixtes
#mise à jour JP Vacher 2025
#----------------------------------------------------------------------------#

rm(list=ls()) #pour effacer les données chargées

# CHARGER LES PACKAGES ####
library(lme4)
library(ggeffects)
library(patchwork)
library(DHARMa)
library(maptools)
library(questionr)
library(ggplot2)
library(viridis)
library(sf)
library(sjPlot)
library(cowplot)
library(lme4)
library(nlme)
library(mgcv)
library(ggeffects)
library(MuMIn)
library(performance)
library(lmerTest)
library(glmmTMB)
library(see)
library(visreg)
library(reshape2)


#-----------------------------------------------------------#
#### Le modele lin?aire mixte : chenille processionnaire ####
#-----------------------------------------------------------#

# CHARGER LES DONNEES ####
chenilles <- read.csv2("chenille_processionnaire.csv",row.names=1, encoding="UTF-8")
chenilles$prop_attaq <- chenilles$nbpinsattac / chenilles$nbpins

## exploration des donn?es

# carte des r?gions
regions <- st_read("regions.geojson")

ggplot(regions)+
  geom_sf(aes(fill=jointure_g))+
  geom_point(data=chenilles,mapping=aes(x=longitude,y=latitude,colour=region),size=2)+
  scale_color_viridis(discrete=TRUE) +
  scale_fill_viridis(discrete=TRUE) + 
  labs(color="R?gions")+ 
  theme_classic()

##### proportion d'arbres infest?s par an

ggplot(chenilles) +
  aes(x = factor(annee), 
      y = prop_attaq, 
      group = placette, 
      color = region) +
  geom_line(show.legend = FALSE) +
  labs(x = "Ann?es", y = "Proportion de pins infest?s") +
  facet_wrap(~region, ncol = 1) + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90))+
  scale_color_viridis_d()


# on rescale les ann?es par commodit?
chenilles$annee_resc <- chenilles$annee-min(chenilles$annee)+1

#### avec un GLM ####

# on commence juste sur une r?gion
chenilles.nord <- subset(chenilles,region=="Centre Nord semi-oceanique")
mod.nord <- glm(cbind(nbpinsattac,nbpins-nbpinsattac)~annee_resc,family=binomial,data=chenilles.nord)

par(mfrow = c(2,2))
plot(mod.nord)

summary(mod.nord)
p.nord <- ggpredict(mod.nord,terms = "annee_resc")%>%
  plot(residuals=T,log.y=T)
p.nord

# GLM sur toutes les r?gions
mod0 <- glm(cbind(nbpinsattac,nbpins-nbpinsattac)~annee_resc,family=binomial,data=chenilles)

# r?sidus
par(mfrow=c(2,2))
plot(mod0)

# interpr?tation
summary(mod0)
library(questionr)
odds.ratio(mod0)

# cartographie des r?sidus
chenilles$res.mod0 <- residuals(mod0)
p0 <- plot(ggpredict(mod0, terms = c("annee_resc")), residuals = T)

p1a <- ggplot(regions) +
  geom_sf() +
  geom_point(data = chenilles, 
             mapping = aes(x = longitude, y = latitude, colour = res.mod0),
             size = 2) +
  scale_color_viridis(discrete = F) + 
  labs(color = "R?sidus du \n GLM binomial") + 
  theme_classic()

p1b <- ggplot(chenilles) +
  aes(x = placette, y = res.mod0) +
  geom_boxplot() +
  labs(x = "Placettes", y = "R?sidus du GLM binomial") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90))

p1a + p1b

## mod?le mixte

# GLMM avec effet placettes dans GRECO 
mod2 <- glmer(cbind(nbpinsattac,nbpins-nbpinsattac)~annee_resc+(1|region/placette),family=binomial,data=chenilles)
summary(mod2)

# r?sidus
plot(mod2, type=c("p","smooth"), col.line=1)
plot(mod2,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1,xlab="Valeurs pr?dites",ylab="sqrt(abs(r?sidus))")

plot(mod2,xlab="Valeurs pr?dites",ylab="R?sidus de Pearson du GLMM")
qqnorm(scale(residuals(mod2)))
abline(0,1)

# interpr?tation
summary(mod2)

library(broom.mixed)
tidy(mod2,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="Wald")
tidy(mod2,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

# surdispersion
library(DHARMa)
simulationOutput <- simulateResiduals(fittedModel = mod2)
testDispersion(simulationOutput)

simulationOutput <- simulateResiduals(fittedModel = mod2, re.form = NULL)
testDispersion(simulationOutput,alternative="greater")
testDispersion(simulationOutput, alternative = "less", plot = FALSE) # seulement sous-dispersion
testDispersion(simulationOutput, alternative = "greater", plot = FALSE) # seulement surdispersion

# graphiques de r?sultats : effets fixes
p3 <- plot(ggpredict(mod2,terms="annee_resc"),residuals=T)+
  labs(x="Ann?es",y="Taux d'infestation",title="")
p3

p4 <- plot(ggpredict(mod2,type="re"),residuals=T)
p4

p5 <- plot(ggpredict(mod2,terms=c("annee_resc","region"),type="re"),residuals=T)
p5

p6 <- plot(ggpredict(mod2,terms=c("annee_resc","placette"),type="re"),residuals=T)
p6

# effets al?atoires
library(sjPlot)
plot_model(mod2, type = "re", show.values = TRUE)

# R?
library(MuMIn)
r.squaredGLMM(mod2)

# figure sur effets al?atoires
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
  labs(color = "R?gions") + 
  theme_classic() +
  theme(legend.position = "none")
