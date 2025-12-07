#----------------------------------------#
#### Communautes d'oiseaux forestiers ####
# effets aleatoires sur l'intercept #
#Auteur : Jean-Yves Barnagaud 2025 #
#Adaptation JP Vacher 2025 #
#----------------------------------------#

# CHARGER LES PACKAGES ####
library(tidyverse)
library(lme4)
library(cowplot)

# CHARGER LES DONNEES ####
dperche1 <- read_tsv("richesse_avifaune_perche.txt") %>% 
  mutate(point = factor(point), 
         massif = factor(massif), 
         essence = factor(essence))
print(dperche1, n = 5)

## MODELE GLM ####

# Richesse specifique en fonction des variables visees, tous massifs confondus
mod.perche1 <- glm(rs ~ Hdom + ST + essence, data = dperche1, family = poisson)

# contrele residus
par(mfrow = c(2,2))
plot(mod.perche1) # quelques points extremes mais qui ne sont pas derangeants (on reste bien en dessous des niveaux de leverage qui influent sur les coefficients)

# l'effet massif est bien visible
boxplot(residuals(mod.perche1, type = "pearson") ~ dperche1$massif, main = "residus ~ massif")
summary(aov(residuals(mod.perche1, type="pearson") ~ dperche1$massif))

# Richesse specifique en fonction des variables visees, avec effet fixe massif
mod.perche2 <- glm(rs ~ Hdom + ST + essence + massif, data = dperche1, family = poisson)
summary(mod.perche2) #suffisamment de points par massif pour un effet fixe, tous les effets sont bien estimes malgre les heterogeneites d'echantillonnage


#MODELE MIXTE ####
## on passe en modele mixte pour representer la hierarchie de l'echantillonnage et concentrer cette hierarchie en un terme unique de variance

# ce modele genere un warning
mod.perche3 <- glmer(rs ~ Hdom + ST + essence + (1|massif), data = dperche1, family = poisson)

# etude des variables
summary(dperche1$Hdom)
summary(dperche1$ST)

# le warning est genere par le fait qu'on a des variables sur de grandes echelles de valeurs. Une solution est de centrer - reduire (recommande)
dperche1$sHdom <- as.vector(scale(dperche1$Hdom))
dperche1$sST <- as.vector(scale(dperche1$ST))

# variables centrees-reduites
summary(dperche1$sHdom)
summary(dperche1$sST)

# on refait le modele avec les variables centrees reduites
mod.perche3 <- glmer(rs ~ sHdom + sST + essence + (1|massif), data = dperche1, family = poisson)


# diagnostic sur residus
p1 <- plot(mod.perche3) 

res <- data.frame(res=residuals(mod.perche3))
p2 <- ggplot(res, aes(sample = res)) +
  stat_qq() +
  stat_qq_line() 


p3 <- ggplot(data.frame(lev = hatvalues(mod.perche3), pearson = residuals(mod.perche3,type = "pearson")),
          aes(x = lev,y = pearson)) +
  geom_point() +
  theme_bw() 

plot_grid(p1,p2,p3)

# effet blocs bien traitee
boxplot(residuals(mod.perche3) ~ dperche1$massif)

# Re
r.squaredGLMM(mod.perche3)

# sortie numerique
summary(mod.perche3)

# sorties graphiques des effets avec sjPlot : avec type="eff", predicteurs constants e 0
p.mix1 <- plot_model(mod.perche3, type = "eff", terms = "sST")
p.mix2 <- plot_model(mod.perche3, type = "eff", terms = "sHdom")
p.mix3 <- plot_model(mod.perche3,type = "eff", terms = "essence")
p.mix4 <- plot_model(mod.perche3,type = "re")
cowplot::plot_grid(p.mix1, p.mix2, p.mix3, p.mix4)


# modele initial (GLM) avec variables centrees-reduites
mod.perche1b=glm(rs~sHdom+sST+essence,data=dperche1,family=poisson)

# comparaison des intervalles de confiance (sauf intercept pour faciliter la representation graphique)
conf.massif=confint(mod.perche3,method="Wald")[-c(1,2),] # on utilise les IC de Wald parce qu'il y a un gros df mais il vaudrait mieux calculer les IC profile (! long)
conf.nomassif=confint(mod.perche1b,method="Wald")[-1,]

# graphiquement : les differences d'IC ne sont pas tres fortes parce que l'effet massif est faible, mais on voit quand meme de petits decalages
# ces decalages seraient bien plus forts si le ddl etait plus faible et avec des gros desequilibres d'echantillonnage entre massifs
plot(x=0,y=0,type="n",xlim=c(1,13),xaxt="n",xlab="pentes",ylab="intervalle de confiance (Wald)",ylim=c(-0.4,0.4))
axis(side=1,at=seq(1.5,12.5,2),labels=c("Hdom","ST","DOU","EPC","PS","SP"))
abline(h=0,col="steelblue",lty="dotted")
segments(x0=seq(1,12,2),x1=seq(1,12,2),y0=conf.massif[,1],y1=conf.massif[,2],lwd=2)
segments(x0=seq(2,13,2),x1=seq(2,13,2),y0=conf.nomassif[,1],y1=conf.nomassif[,2],lwd=2,col="darkred")
legend("topright",bty="n",lty="solid",col=c("black","darkred"),legend=c("effet aleatoire massif","pas d'effet massif"))

#--------------------------------------#
### Modele mixte e pentes aleatoires ###
#--------------------------------------#

## GLM classique : terme d'interaction
mod.perche4 <- glm(rs ~ massif*(sHdom+sST+essence), data = dperche1, family = poisson)

par(mfrow=c(2,2))
plot(mod.perche4)
summary(mod.perche4)

# interactions non estimables
table(dperche1[,c("massif","essence")])

# GLMM à pentes aleatoires
mod.perche5 <- glmer(rs~sHdom + sST + essence + (sHdom + sST + essence|massif),
                     data=dperche1,family=poisson,
                     control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun=2e5))) # identique à (1+sHdom+sST+essence|massif). 
#Le changement d'algorithme est rendu necessaire par la complexite du modele
#Dans le cours, j'ai simplifié en retirant essence du terme aléatoire

# intervalles de confiance
confint(mod.perche5,method="Wald") # à nouveau : penser à passer en "profile" pour avoir des IC plus corrects, mais attention à la duree de calcul

# sorties graphiques du modele à intercept aleatoire
pmix1 <- plot_model(mod.perche3,type="eff",terms="sST",title="Surface terriere, intercept aleatoire")+ylim(10,20)
pmix2 <- plot_model(mod.perche3,type="eff",terms="sHdom",title="Hauteur dominante, intercept aleatoire")+ylim(0,20)
pmix3 <- plot_model(mod.perche3,type="eff",terms="essence",title="Essence, intercept aleatoire") # VID n'est present que sur Perche-Trappe

# sorties graphiques du modele à intercept et pentes aleatoires
pmixslope1 <- plot_model(mod.perche5,type="eff",terms="sST",title="Surface terriere, intercept et pentes aleatoires")+ylim(10,20)
pmixslope2 <- plot_model(mod.perche5,type="eff",terms="sHdom",title="Hauteur dominante, intercept et pentes aleatoires")+ylim(0,20)
pmixslope3 <- plot_model(mod.perche5,type="eff",terms="essence",title="Essence, intercept et pentes aleatoires")

#Tout sur une même figure :
cowplot::plot_grid(pmix1,pmix2,pmix3,pmixslope1,pmixslope2,pmixslope3)