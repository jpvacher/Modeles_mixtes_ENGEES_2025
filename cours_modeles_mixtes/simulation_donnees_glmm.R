#Script inspiré d'un cours de Jean-Yves Barnagaud
#Modifié par JP Vacher 2025

#CHARGER LES LIBRARIES ####
library(tidyverse)
library(lme4)

#SIMULER UN JEU DE DONNEES ####
## 20 strates ####
strate <- paste("S", 1:20, sep = "")

## moyenne de l'intercept de chaque strate ####
mean <- rnorm(20, 5, 10)

## pente unique pour toutes les strates ####
alpha <- -5

## une variable explicative mesuree de la meme maniere dans toutes les strates, avec 30 points par bloc ####
Var <- rnorm(30, 0, 1)

## un residu non structure avec une certaine variance ####
eps <- rnorm(30, 0, 1)

## on genere la variable de reponse dans chaque strate ####
var.rep <- NULL
for(i in 1:length(mean)) {
  y = mean[i] + alpha * Var + eps
  var.rep = c(var.rep, y)
}

##on genere une table avec les données simulées ####
data.sim <- tibble(Reponse = var.rep,
                      Strate = rep(strate,each=30),
                      Predicteur = rep(Var, times = length(mean)))
data.sim #on regarde à quoi ça ressemble
#summary(data.sim) #si on veut regarder la structure des données


#COMPARAISON DES MODELES ####

##Modele naif ####
#Première approche, on ajuste un modèle linéaire simple :
mod.naif <- lm(data = data.sim, Reponse ~ Predicteur)
par(mfrow=c(2,2))
plot(mod.naif) #evaluation graphique du modele
par(mfrow = c(1,1))
boxplot(residuals(mod.naif)~data.sim$Strate)

coef(mod.naif)
#(Intercept)  Predicteur 
#.  4.962526   -5.490665 
confint(mod.naif)
#                2.5 %    97.5 %
#(Intercept)  3.996593  5.928459
#Predicteur  -6.799083 -4.182248


###vrai intercept : 5.2####
moy.sim.a <- mean(mean)
#[1] 5.237132

###vrai intervale de confiance : [-0.13 – 10.61] ####
ic.sim.a <- paste("[",(round(mean(mean) - 1.96 * (sd(mean)/sqrt(length(mean))),2)), "–",
                 round(mean(mean) + 1.96 * (sd(mean)/sqrt(length(mean))),2), "]")
#[1] "[ -0.13 – 10.61 ]"

#l'intervale de confiance du modèle est trop resserre (erreur de type 1 importante, on rejette H0 alors qu'elle est vraie)
#mais l'estimate (pente) est bonne, -5.49, alors que la vraie valeur est -5.

##Modele 2 on ajoute l'effet strate (facteur fixe) ####
mod.add <- lm(data = data.sim, Reponse ~ Predicteur + Strate)
head(confint(mod.add))
#                 2.5 %      97.5 %
#(Intercept)   3.999399   4.4416045
#Predicteur   -5.558002  -5.4233287
#StrateS10    28.121777  28.7469683
#StrateS11    -1.303721  -0.6785298
#StrateS12   -30.353661 -29.7284696
#StrateS13     7.327503   7.9526949

## est-ce que l'intervalle de confiance de la strate S1 est bon? ####
data.s1 <- subset(data.sim, Strate == "S1")

moy.sim <- mean(data.s1$Reponse)
sd.sim <- sd(data.s1$Reponse)
ic.sim.s1 <- paste("[", round(moy.sim - 1.96 * (sd.sim/sqrt(nrow(data.s1))),2), "-",
                  round(moy.sim + 1.96 * (sd.sim/sqrt(nrow(data.s1))),2), "]")
#"[ 2.3 – 5.27 ]"

# l'estimation est a nouveau un peu etroite
# Il faut reflechir un instant a la maniere dont le jeu de donnees est construit : 
# on a simule des valeurs populationnelles pour mean et alpha, donc il faut que
#l'inference se situe aussi a l'echelle populationnelle


#MODELE MIXTE ####
##Modele 3 : on intègre l'effet strate dans le modèle comme facteur aléatoire : hiérarchie ####
mod.lmm <- lmer(data = data.sim, Reponse ~ Predicteur + (1|Strate))
confint(mod.lmm)
#.                 2.5 %     97.5 %
#.sig01       9.0172196 16.8713856
#.sigma       0.5820702  0.6530976
#(Intercept) -0.5333130 10.4583646
#Predicteur  -5.5579149 -5.4234161

#L'intervale de confiance de l'intercept est proche du vrai intervalle de confiance


#NOUVEAU JEU DE DONNEES ####
## maintenant, on fait varier la pente alpha d'une strate a l'autre (précédemment, alpha était fixé à -5) ####
alpha2 <-  rnorm(20, -5, 5)

##on génère une nouvelle variable de réponse avec alpha variable ####
var.rep2 <- NULL
for(i in 1:length(mean)){
  y = mean[i]+ alpha2[i]* Var + eps
  var.rep2 = c(var.rep2, y)
}

##on l'intègre dans le jeu de données ####
data.sim$Reponse2 <- var.rep2
#summary(data.sim) #si on veut voir la nouvelle strucure des données

## on relance le modele mixte avec un effet aleatoire sur l'intercept ####
#(= on ne tient pas compte du fait que l'effet de la variable explicative change d'un bloc a l'autre)

mod.lmm2 <- lmer(Reponse2 ~ Predicteur + (1|Strate), data = data.sim)

##on regarde les intervalles de confiance ####
confint(mod.lmm2)
#.                2.5 %    97.5%
#.sig01       9.0405903 16.943598
#.sigma       3.2674219  3.666131
#(Intercept) -0.5606127 10.485664
#Predicteur  -5.0599147 -4.304912

## on calcule le vrai intervalle de confiance de alpha ###

moy.sim.alpha2 <- mean(alpha2)
ic.sim.alpha2 <- paste("[", round(mean(alpha2) - 1.96 * (sd(alpha2)/sqrt(length(alpha2))),2), "–",
                  round(mean(alpha2) + 1.96 * (sd(alpha2)/sqrt(length(alpha2))),2), "]")

moy.sim.alpha2
#-4.191748
ic.sim.alpha2
#"[ -6.24 – -2.14 ]"

#m.rand3 <- lmer(Reponse2 ~ Predicteur + (Predicteur|Strate), data = data.sim)
#confint(m.rand3)
                

#PLOT DATA ####

data.sim1 <- data.sim %>% 
  filter(Strate == "S1")
data.sim3 <- data.sim %>% 
  filter(Strate == "S3")
data.sim7 <- data.sim %>% 
  filter(Strate == "S7")

ggplot(data.sim, aes(x = Reponse2)) +
  theme_bw() +
  geom_histogram(aes(y = ..density..), fill = alpha("white", 0)) +
  stat_function(fun = dnorm, args = list(mean = mean(data.sim1$Reponse), 
                                         sd = sd(data.sim1$Reponse)), col = "red", lty='dashed') +
  stat_function(fun = dnorm, args = list(mean = mean(data.sim7$Reponse), 
                                         sd = sd(data.sim7$Reponse)), col = "forestgreen", lty='dashed') +
  stat_function(fun = dnorm, args = list(mean = mean(data.sim3$Reponse), 
                                         sd = sd(data.sim3$Reponse)), col = "dodgerblue", lty='dashed') +
  stat_function(fun = dnorm, args = list(mean = mean(data.sim$Reponse), 
                                         sd = sd(data.sim$Reponse)), col = "black", ) +
  xlim(-50,50) +
  ylim(0,.11) +
  theme(legend.position = "none")
