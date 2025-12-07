#Packages ##########
library(readxl)
library(lattice)
library(nlme) #mixed model lme glme
library(emmeans)

################################################################################################################
# I) FILE PREPARATION ####
################################################################################################################
setwd()
data.vipera <- read_excel("Vipera_males.xlsx", sheet = "dataset")
summary(data.vipera)
str(data.vipera)

data.vipera$VO2 <- data.vipera$`VO2 (ml/h)`# data on resting metabolic rates
data.vipera$EWL <- data.vipera$`EWL (mg/h)`# data on water loss

data.vipera["Temperature"] <- factor(data.vipera$Temperature) ## make temp a factor (15, 25 and 33 degrees)
data.vipera["Species"] <- factor(data.vipera$Species) 
data.vipera["Viper_ID"] <- factor(data.vipera$Viper_ID)
data.vipera["TL"] <- as.numeric(data.vipera$TL)

data.vipera$Mass1_scale <- scale(data.vipera$Mass1, scale = T, center = T)

data5sp <- data.vipera

# Reorder species ####
data5sp$Species<-relevel(data5sp$Species,"Vipera berus")
data5sp$Species<-relevel(data5sp$Species,"Vipera seoanei")
data5sp$Species<-relevel(data5sp$Species,"Vipera aspis")
data5sp$Species<-relevel(data5sp$Species,"Vipera latastei")
data5sp$Species<-relevel(data5sp$Species,"Vipera ammodytes")

################################################################################################################
# I) Check data quality among species and lineage ####
################################################################################################################

#Check relation between BM and SVL for the 5 species in the full data set ####
xyplot(Mass1~SVL|Species,panel = function(x, y) {
  panel.grid(h = -1, v = 2)
  panel.xyplot(x, y)
  panel.loess(x, y, span=1)
},data=data5sp) 

#Explore relation between variables at each temperature ############ 
#Relation between VO2 and BM ####
xyplot(VO2~Mass1|Species*Temperature,panel = function(x, y) { 
  panel.grid(h = -1, v = 2)
  panel.xyplot(x, y)
  panel.loess(x, y, span=1)
},data=data5sp)

#Relation between EWL and SVL####
xyplot(EWL~SVL|Species*Temperature,panel = function(x, y) {
  panel.grid(h = -1, v = 2)
  panel.xyplot(x, y)
  panel.loess(x, y, span=1)
},data=data5sp) 

#Check Relation between VO2 and EWL for the 5 species in the full data set ####
xyplot(EWL~VO2|Species,panel = function(x, y) {
  panel.grid(h = -1, v = 2)
  panel.xyplot(x, y)
  panel.loess(x, y, span=1)
},data=data5sp) 

################################################################################################################
# II) Analysis VO2 ####
################################################################################################################

model.O2_1 <- lme(VO2 ~ Species*Temperature + Mass1_scale*Temperature, random=list(~1|Viper_ID), na.action=na.omit, data=data5sp)
model.O2_2 <- lme(VO2 ~ Species*Temperature + scale(SVL)*Temperature, random=list(~1|Viper_ID), na.action=na.omit, data=data5sp)

AIC(model.O2_1, model.O2_2) 

# model.O2_1 lower AIC

anova(model.O2_1, type = "marginal")
summary(model.O2_1)
emmeans(model.O2_1, list(pairwise ~ Species*Temperature), adjust="tukey")

# save emmeans values for equation

################################################################################################################
# III) Analysis EWL ####
################################################################################################################

model.EWL_1 <- lme(EWL ~Species*Temperature + Temperature*Mass1_scale, random=list(~1|Viper_ID), na.action=na.omit, data=data5sp)
model.EWL_2 <- lme(EWL ~Species*Temperature + Temperature*scale(SVL), random=list(~1|Viper_ID), na.action=na.omit, data=data5sp)

AIC(model.EWL_1, model.EWL_2)

# model.EWL_2 lower AIC

anova(model.EWL_2, type = "marginal")
summary(model.EWL_2)
plot(model.EWL_2)
emmeans(model.EWL_2, list(pairwise ~ Species*Temperature) ,adjust="tukey")

# save emmeans values for equation

################################################################################################################
# IV) Analysis Q10 ####
################################################################################################################

data.vipera <- read_excel("Vipera_males.xlsx", sheet = "Q10")
str(data.vipera)

data.vipera["Species"] <- factor(data.vipera$Species) 
data.vipera["Viper_ID"] <- factor(data.vipera$Viper_ID)
data.vipera["Q10_interval"] <- factor(data.vipera$Q10_interval)
data.vipera["Q10_O2"] <- as.numeric(data.vipera$Q10_O2)
data.vipera["Q10_EWL"] <- as.numeric(data.vipera$Q10_EWL)

data.o2 <- data.vipera
data.ewl <- data.vipera

# VO2
model.Q10_O2 <- lme(Q10_O2 ~ Q10_interval*Species + 1, random=list(~1|Viper_ID), na.action = na.omit, data = data.o2)

anova(model.Q10_O2, type = "marginal")

summary(model.Q10_O2)

# TEWL
model.Q10_EWL <- lme(Q10_EWL ~ Q10_interval*Species + 1, random=list(~1|Viper_ID), na.action = na.omit, data = data.ewl)

anova(model.Q10_EWL, type = "marginal")

summary(model.Q10_EWL)
