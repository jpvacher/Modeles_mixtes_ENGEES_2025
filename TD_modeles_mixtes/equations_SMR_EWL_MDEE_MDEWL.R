rm(list=ls())

### 0. Packages ####
library(readxl)
library(dplyr)
library(ggplot2)
library(lattice)
library(nlme)
library(emmeans)

setwd()

### 1. FILE PREPARATION ####
## 1.1 Data set importation ####
data.vipera <- read_excel("Vipera_males.xlsx", sheet = "emmeans")
summary(data.vipera)
str(data.vipera)

VB <- subset(data.vipera, data.vipera$Species == "Vipera berus")
VA <- subset(data.vipera, data.vipera$Species == "Vipera aspis")
VS <- subset(data.vipera, data.vipera$Species == "Vipera seoanei")
VL <- subset(data.vipera, data.vipera$Species == "Vipera latastei")
VAm <- subset(data.vipera, data.vipera$Species == "Vipera ammodytes")

### 2. Creation of the equation for the thermal reactions norms for each species ####
## 2.1 - Creation of the equation for the metabolic thermal reaction norms for each species ####
# 2.1 a) Creation of objects to build up the equations (metabolic rates) for each species ####

par(mfrow = c(1, 1), bg="cornsilk" )

#berus
xb <- c(VB$Temperature)
yb <- c(VB$emmeanO2)
seb <- c(VB$SE_O2)
#aspis
xa <- c(VA$Temperature)
ya <- c(VA$emmeanO2)
sea <- c(VA$SE_O2)
#seoanei
xs <- c(VS$Temperature)
ys <- c(VS$emmeanO2)
ses <- c(VS$SE_O2)
#latastei
xl <- c(VL$Temperature)
yl <- c(VL$emmeanO2)
sel <- c(VL$SE_O2)
#ammodytes
xam <- c(VAm$Temperature)
yam <- c(VAm$emmeanO2)
seam <- c(VAm$SE_O2)

tb <- seq(min(xb, xs, xa, xl, xam), max(xb, xs, xa, xl, xam), length.out = 100)

# 2.1 b) Use of linear regression to identify the equation parameters for each species (metabolic rates) ####
nlmoda <- nls(ya ~ const + A * xa, trace=TRUE, 
              start=list(const = 0.0074295311, A = 0.0017367916))
nlmoda 
summary(nlmoda)

nlmodb <- nls(yb ~ const + A * xb, trace=TRUE, 
              start=list(const = 0.0074295311, A = 0.0017367916))
nlmodb
summary(nlmodb)

nlmods <- nls(ys ~  const - A*xs, trace=TRUE, 
              start=list(const = 0.0074295311, A = 0.0017367916)) 
nlmods #Parameters for V seoanei ####
summary(nlmods)

nlmodl <- nls(yl ~  const - A*xl, trace=TRUE, 
              start=list(const = 0.0074295311, A = 0.0017367916)) 
nlmodl #Parameters for V latastei ####
summary(nlmodl)

nlmodam <- nls(yam ~  const - A*xam, trace=TRUE, 
               start=list(const = 0.0074295311, A = 0.0017367916)) 
nlmodam #Parameters for V ammodytes ####
summary(nlmodam)

#Check the Model Predictions and Actual Data Alignment
yb <- predict(nlmodb, list(xb = xb))
ya <- predict(nlmoda, list(xa = xa))
ys <- predict(nlmods, list(xs = xs))
yl <- predict(nlmodl, list(xl = xl))
yam <- predict(nlmodam, list(xam = xam))

# Figure 2
# 2.1 c) Graphic representation of the metabolic thermal reaction norms for each species ####

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#CC79A7", "#009E73", "#0072B2", "#D55E00", "#F0E442")

# vam -> #000000
# va -> #E69F00
# vs -> #F0E442
# vl -> #009E73
# vb -> #56B4E9

jpeg(file="O2consumption_emmeans.jpeg",
     width = 30, height = 20, units="cm", res=300)

par(mar = c(5,5.2,3,3), mfrow = c(1, 1), bg="white", col.lab="black", col.axis="black", fg="black"  )

VO2_5species <- 0.0297675 - 0.00465853*tb  # This is to calibrate the graph format
plot(VO2_5species ~ tb, type="n", xaxt = "n", xlab= "Body temperature (°C)", ylab="Mass adjusted VO2 (ml.h-1)",
     ylim=c(0, 12), xlim=c(14.5, 33.5), cex.lab = 2, cex.axis = 1.5)

lines(tb, predict(nlmodb,list(xb = tb)),lty=5, col= "#56B4E9", lwd=2) # lines predicted values for V.berus
lines(tb, predict(nlmods,list(xs = tb)),lty=4, col= "#F0E442", lwd=2) # lines predicted values for V.seoanei
lines(tb, predict(nlmoda,list(xa = tb)),lty=3, col= "#E69F00", lwd=2) # lines predicted values for V.aspis
lines(tb, predict(nlmodl,list(xl = tb)),lty=2, col= "#009E73", lwd=2) # lines predicted values for V.latastei
lines(tb, predict(nlmodam,list(xam = tb)),lty=1, col= "#000000", lwd=2) # lines predicted values for V.ammodytes

axis(side=1, at=c(15, 25,33), labels=c("15","25","33"), cex.axis = 1.5)

legend(x = "topleft", inset = 0,
       c("Vipera berus", "Vipera seoanei", "Vipera aspis", "Vipera latastei","Vipera ammodytes"),
       col = c("#56B4E9","#F0E442","#E69F00","#009E73","#000000"), lty=c(5,4,3,2,1), y.intersp = 2, text.font = 3,
       pch = c(19,19,19,19,19),lwd = 1, cex = 1.5, bty = "n")

xi <-  c(15,25,33)

points(xi, yb, type = "p", col = "#56B4E9", pch = 19,cex = 1.5,lwd = 2,lty=4)
points(xi, ys, type = "p", col = "#F0E442", pch = 19,cex = 1.5,lwd = 2,lty=4)
points(xi, ya, type = "p", col = "#E69F00", pch = 19,cex = 1.5,lwd = 2,lty=4)
points(xi, yl, type = "p", col = "#009E73", pch = 19,cex = 1.5,lwd = 2,lty=4)
points(xi, yam, type = "p", col = "#000000", pch = 19,cex = 1.5,lwd = 2,lty=1)

arrows(xi, yb, xi, yb + seb,
       code = 2, col = "#56B4E9", angle = 90, length = .1,lwd = 2,lty=1)
arrows(xi, ys, xi, ys - ses,
       code = 2, col = "#F0E442", angle = 90, length = .1,lwd = 2,lty=1)
arrows(xi, ya, xi, ya + sea,
       code = 2, col = "#E69F00", angle = 90, length = .1,lwd = 2,lty=1)
arrows(xi, yl, xi, yl - sel,
       code = 2, col = "#009E73", angle = 90, length = .1,lwd = 2,lty=1)
arrows(xi, yam, xi, yam - seam,
       code = 2, col = "#000000", angle = 90, length = .1,lwd = 2,lty=1)

dev.off()

## 2.2 - Creation of the equation for the water loss rate thermal reaction norms for each species ####
# 2.2 a) Creation of objects to build up the equations (water loss rate) for each species ####

par(mfrow = c(1, 1), bg="cornsilk" )

#berus
xb_EWL <- c(VB$Temperature)
yb_EWL <- c(VB$emmeanEWL)
seb_EWL <- c(VB$SE_EWL)
#aspis
xa_EWL <- c(VA$Temperature)
ya_EWL <- c(VA$emmeanEWL)
sea_EWL <- c(VA$SE_EWL)
#seoanei
xs_EWL <- c(VS$Temperature)
ys_EWL <- c(VS$emmeanEWL)
ses_EWL <- c(VS$SE_EWL)
#latastei
xl_EWL <- c(VL$Temperature)
yl_EWL <- c(VL$emmeanEWL)
sel_EWL <- c(VL$SE_EWL)
#ammodytes
xam_EWL <- c(VAm$Temperature)
yam_EWL <- c(VAm$emmeanEWL)
seam_EWL <- c(VAm$SE_EWL)

tb <- seq(min(xb_EWL, xs_EWL, xa_EWL, xl_EWL, xam_EWL), max(xb_EWL, xs_EWL, xa_EWL, xl_EWL, xam_EWL), length.out = 100)

# 2.1 b) Use of logistic and gompertz regression to identify the equation parameters for each species  (water loss rates) ####

nlmoda_EWL <- nls(ya_EWL ~  const - A*xa_EWL,
                  trace=TRUE, start=list(const = 0.0074295311, A = 0.0017367916)) 
nlmoda_EWL #Parameters for V aspis ####
summary(nlmoda_EWL)

nlmodb_EWL <- nls(yb_EWL ~  const - A*xb_EWL, trace=TRUE, 
                  start=list(const = 0.0074295311, A = 0.0017367916)) 
nlmodb_EWL #Parameters for V berus ###
summary(nlmodb_EWL)

nlmods_EWL <- nls(ys_EWL ~  const - A*xs_EWL, trace=TRUE, 
                  start=list(const = 0.0074295311, A = 0.0017367916)) 
nlmods_EWL #Parameters for V seoanei ####
summary(nlmods_EWL)

nlmodl_EWL <- nls(yl_EWL ~  const - A*xl_EWL, trace=TRUE, 
                  start=list(const = 0.0074295311, A = 0.0017367916)) 
nlmodl_EWL #Parameters for V latastei ####
summary(nlmodl_EWL)

nlmodam_EWL <- nls(yam_EWL ~  const - A*xam_EWL, trace=TRUE, 
                   start=list(const = 0.0074295311, A = 0.0017367916)) 
nlmodam_EWL #Parameters for V ammodytes ####
summary(nlmodam_EWL)

#Check the Model Predictions and Actual Data Alignment
yb_EWL <- predict(nlmodb_EWL, list(xb_EWL = xb_EWL))
ya_EWL <- predict(nlmoda_EWL, list(xa_EWL = xa_EWL))
ys_EWL <- predict(nlmods_EWL, list(xs_EWL = xs_EWL))
yl_EWL <- predict(nlmodl_EWL, list(xl_EWL = xl_EWL))
yam_EWL <- predict(nlmodam_EWL, list(xam_EWL = xam_EWL))

# 2.1 c) Graphic representation of the water loss rate thermal reaction norms for each species ####

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#CC79A7", "#009E73", "#0072B2", "#D55E00", "#F0E442")

# vam -> #000000
# va -> #E69F00
# vs -> #F0E442
# vl -> #009E73
# vb -> #56B4E9

jpeg(file="EWL_emmeans.jpeg",
     width = 30, height = 20, units="cm", res=300)

par(mar = c(5,5.2,3,3), mfrow = c(1, 1), bg="white", col.lab="black", col.axis="black", fg="black"  )

EWL_5species <- 0.0297675 - 0.00465853*tb # This is to calibrate the graph format
plot(EWL_5species ~ tb, type="n", xaxt = "n", xlab= "Body temperature (°C)", ylab="SVL adjusted TEWL (mg.h-1)",
     ylim=c(-4, 80), xlim=c(14.5, 33.5), cex.lab = 2, cex.axis = 1.5)

lines(tb, predict(nlmodb_EWL,list(xb_EWL =tb)),lty=5, col= "#56B4E9", lwd=2) # lines predicted values for V.berus
lines(tb, predict(nlmods_EWL,list(xs_EWL =tb)),lty=4, col= "#F0E442", lwd=2) # lines predicted values for V.seoanei
lines(tb, predict(nlmoda_EWL,list(xa_EWL =tb)),lty=3, col= "#E69F00", lwd=2) # lines predicted values for V.aspis
lines(tb, predict(nlmodl_EWL,list(xl_EWL =tb)),lty=2, col= "#009E73", lwd=2) # lines predicted values for V.latastei
lines(tb, predict(nlmodam_EWL,list(xam_EWL =tb)),lty=1, col= "#000000", lwd=2) # lines predicted values for V.ammodytes

axis(side=1, at=c(15, 25,33), labels=c("15","25","33"), cex.axis = 1.5)

legend(x="topleft", inset = 0,
       c("Vipera berus", "Vipera seoanei","Vipera aspis","Vipera latastei","Vipera ammodytes"),
       col=c("#56B4E9","#F0E442","#E69F00","#009E73","#000000"), lty = c(5,4,3,2,1), y.intersp = 2, text.font = 3,
       pch =c(19,19,19,19,19), lwd = 1, cex = 1.5, bty = "n")

xi <-  c(15,25,33)

points(xi, yb_EWL, type = "p", col = "#56B4E9", pch = 19,cex = 1.5,lwd = 2,lty=4)
points(xi, ys_EWL, type = "p", col = "#F0E442", pch = 19,cex = 1.5,lwd = 2,lty=4)
points(xi, ya_EWL, type = "p", col = "#E69F00", pch = 19,cex = 1.5,lwd = 2,lty=4)
points(xi, yl_EWL, type = "p", col = "#009E73", pch = 19,cex = 1.5,lwd = 2,lty=4)
points(xi, yam_EWL, type = "p", col = "#000000", pch = 19,cex = 1.5,lwd = 2,lty=1)

arrows(xi, yb_EWL, xi, yb_EWL + seb_EWL,
       code = 2, col = "#56B4E9", angle = 90, length = .1,lwd = 2,lty=1)
arrows(xi, ys_EWL, xi, ys_EWL + ses_EWL,
       code = 2, col = "#F0E442", angle = 90, length = .1,lwd = 2,lty=1)
arrows(xi, ya_EWL, xi, ya_EWL - sea_EWL,
       code = 2, col = "#E69F00", angle = 90, length = .1,lwd = 2,lty=1)
arrows(xi, yl_EWL, xi, yl_EWL - sel_EWL,
       code = 2, col = "#009E73", angle = 90, length = .1,lwd = 2,lty=1)
arrows(xi, yam_EWL, xi, yam_EWL - seam_EWL,
       code = 2, col = "#000000", angle = 90, length = .1,lwd = 2,lty=1)

dev.off()


### 3. Calculation of the predicted values for each species, at the Tbody from Lourdais2003 ####

## 3.1 Calculation of the predicted values for each species at each hour over the full dataset of 67days ####
# 3.1a) Importation of Lourdais2003 dataset ####

data.Lourdais2003=read.csv("ViperTb2003.csv",header=TRUE,sep=";")
summary(data.Lourdais2003)
str(data.Lourdais2003)
data.Lourdais2003 <- data.Lourdais2003 %>% 
  rename(Temperature = MeanNR)
data.Lourdais2003$Heat_Day <- factor(data.Lourdais2003$Heat_Day, levels = c("N", "HD", "VHD"))
str(data.Lourdais2003)

equation_values <- c("SMR_VB", "EWL_VB", "SMR_VS", "EWL_VS", 
                     "SMR_VA", "EWL_VA", "SMR_VL", "EWL_VL", 
                     "SMR_VAm", "EWL_VAm")
data.Lourdais2003[equation_values][data.Lourdais2003[equation_values] == "equation data"] <- NA
data.Lourdais2003[equation_values] <- lapply(data.Lourdais2003[equation_values], as.numeric)
str(data.Lourdais2003)

xyplot(Temperature~as.numeric(HeureNum)|Heat_Day,data=data.Lourdais2003)

data.Lourdais2003_Mean_Temperature <- data.Lourdais2003 %>% 
  group_by(HeureNum, Heat_Day) %>% 
  summarise(Mean_Temperature = mean(Temperature))
str(data.Lourdais2003_Mean_Temperature)
xyplot(Mean_Temperature~as.numeric(HeureNum)|Heat_Day,data=data.Lourdais2003_Mean_Temperature)

# 3.1b) Calculation of predicted SMR values based on the metabolic thermal reaction norms equations built for each species ####

data.Lourdais2003$SMR_VB[is.na(data.Lourdais2003$SMR_VB)] <- predict(nlmodb,list(xb =data.Lourdais2003$Temperature)) # fill SMR_VB with predicted values from VB equation
data.Lourdais2003$SMR_VS[is.na(data.Lourdais2003$SMR_VS)] <- predict(nlmods,list(xs =data.Lourdais2003$Temperature)) # fill SMR_VS with predicted values from VS equation
data.Lourdais2003$SMR_VA[is.na(data.Lourdais2003$SMR_VA)] <- predict(nlmoda,list(xa =data.Lourdais2003$Temperature)) # fill SMR_VA with predicted values from VA equation
data.Lourdais2003$SMR_VL[is.na(data.Lourdais2003$SMR_VL)] <- predict(nlmodl,list(xl =data.Lourdais2003$Temperature)) # fill SMR_VL with predicted values from VL equation
data.Lourdais2003$SMR_VAm[is.na(data.Lourdais2003$SMR_VAm)] <- predict(nlmodam,list(xam =data.Lourdais2003$Temperature)) # fill SMR_VAm with predicted values from VAM equation
str(data.Lourdais2003)

# 3.1c) Calculation of predicted EWL values based on the water loss thermal reaction norms equations built for each species ####

data.Lourdais2003$EWL_VB[is.na(data.Lourdais2003$EWL_VB)] <- predict(nlmodb_EWL,list(xb_EWL =data.Lourdais2003$Temperature)) # fill SMR_VB with predicted values from VB equation
data.Lourdais2003$EWL_VS[is.na(data.Lourdais2003$EWL_VS)] <- predict(nlmods_EWL,list(xs_EWL =data.Lourdais2003$Temperature)) # fill SMR_VS with predicted values from VS equation
data.Lourdais2003$EWL_VA[is.na(data.Lourdais2003$EWL_VA)] <- predict(nlmoda_EWL,list(xa_EWL =data.Lourdais2003$Temperature)) # fill SMR_VA with predicted values from VA equation
data.Lourdais2003$EWL_VL[is.na(data.Lourdais2003$EWL_VL)] <- predict(nlmodl_EWL,list(xl_EWL =data.Lourdais2003$Temperature)) # fill SMR_VL with predicted values from VL equation
data.Lourdais2003$EWL_VAm[is.na(data.Lourdais2003$EWL_VAm)] <- predict(nlmodam_EWL,list(xam_EWL =data.Lourdais2003$Temperature)) # fill SMR_VAm with predicted values from VAM equation
str(data.Lourdais2003)

# 3.2c) Creation of a dataframe with daily cumulated SMR and EWL for each day during the entire day (diurnal and nocturnal) for each species at each given day ####
# data set with daily cumulated SMR and EWL for each day during the entire day (diurnal and nocturnal), with SMR and EWL columns for each species

data.Lourdais2003_dailycalculation_fullday <- data.Lourdais2003 %>%
  group_by(DateF) %>%
  summarize(
    Heat_Day = first(Heat_Day),
    daily_cumulated_SMR_VB = sum(SMR_VB, na.rm = TRUE),  
    daily_cumulated_EWL_VB = sum(EWL_VB, na.rm = TRUE),  
    daily_cumulated_SMR_VS = sum(SMR_VS, na.rm = TRUE),
    daily_cumulated_EWL_VS = sum(EWL_VS, na.rm = TRUE),
    daily_cumulated_SMR_VA = sum(SMR_VA, na.rm = TRUE),
    daily_cumulated_EWL_VA = sum(EWL_VA, na.rm = TRUE),
    daily_cumulated_SMR_VL = sum(SMR_VL, na.rm = TRUE),
    daily_cumulated_EWL_VL = sum(EWL_VL, na.rm = TRUE),
    daily_cumulated_SMR_VAm = sum(SMR_VAm, na.rm = TRUE),
    daily_cumulated_EWL_VAm = sum(EWL_VAm, na.rm = TRUE)
  )
str(data.Lourdais2003_dailycalculation_fullday)
summary(data.Lourdais2003_dailycalculation_fullday)
head(data.Lourdais2003_dailycalculation_fullday)

# Transformation of the data set to have species as a factor, with one column for dail cumulated SMR and one column for daily cumulated EWL columns 

data.Lourdais2003_daily_calculation_VB_fullday <- data.Lourdais2003_dailycalculation_fullday %>%
  select(-daily_cumulated_SMR_VS, -daily_cumulated_EWL_VS, 
         -daily_cumulated_SMR_VA, -daily_cumulated_EWL_VA, 
         -daily_cumulated_SMR_VL, -daily_cumulated_EWL_VL, 
         -daily_cumulated_SMR_VAm, -daily_cumulated_EWL_VAm)
data.Lourdais2003_daily_calculation_VB_fullday <- data.Lourdais2003_daily_calculation_VB_fullday %>%
  mutate(Species = factor("Vipera berus"))
names(data.Lourdais2003_daily_calculation_VB_fullday)[names(data.Lourdais2003_daily_calculation_VB_fullday) == "daily_cumulated_SMR_VB"] <- "daily_cumulated_SMR"
names(data.Lourdais2003_daily_calculation_VB_fullday)[names(data.Lourdais2003_daily_calculation_VB_fullday) == "daily_cumulated_EWL_VB"] <- "daily_cumulated_EWL"
str(data.Lourdais2003_daily_calculation_VB_fullday)
data.Lourdais2003_daily_calculation_VS_fullday <- data.Lourdais2003_dailycalculation_fullday %>%
  select(-daily_cumulated_SMR_VB, -daily_cumulated_EWL_VB, 
         -daily_cumulated_SMR_VA, -daily_cumulated_EWL_VA, 
         -daily_cumulated_SMR_VL, -daily_cumulated_EWL_VL, 
         -daily_cumulated_SMR_VAm, -daily_cumulated_EWL_VAm)
data.Lourdais2003_daily_calculation_VS_fullday <- data.Lourdais2003_daily_calculation_VS_fullday %>%
  mutate(Species = factor("Vipera seoanei"))
names(data.Lourdais2003_daily_calculation_VS_fullday)[names(data.Lourdais2003_daily_calculation_VS_fullday) == "daily_cumulated_SMR_VS"] <- "daily_cumulated_SMR"
names(data.Lourdais2003_daily_calculation_VS_fullday)[names(data.Lourdais2003_daily_calculation_VS_fullday) == "daily_cumulated_EWL_VS"] <- "daily_cumulated_EWL"
str(data.Lourdais2003_daily_calculation_VS_fullday)
data.Lourdais2003_daily_calculation_VA_fullday <- data.Lourdais2003_dailycalculation_fullday %>%
  select(-daily_cumulated_SMR_VB, -daily_cumulated_EWL_VB, 
         -daily_cumulated_SMR_VS, -daily_cumulated_EWL_VS, 
         -daily_cumulated_SMR_VL, -daily_cumulated_EWL_VL, 
         -daily_cumulated_SMR_VAm, -daily_cumulated_EWL_VAm)
data.Lourdais2003_daily_calculation_VA_fullday <- data.Lourdais2003_daily_calculation_VA_fullday %>%
  mutate(Species = factor("Vipera aspis"))
names(data.Lourdais2003_daily_calculation_VA_fullday)[names(data.Lourdais2003_daily_calculation_VA_fullday) == "daily_cumulated_SMR_VA"] <- "daily_cumulated_SMR"
names(data.Lourdais2003_daily_calculation_VA_fullday)[names(data.Lourdais2003_daily_calculation_VA_fullday) == "daily_cumulated_EWL_VA"] <- "daily_cumulated_EWL"
str(data.Lourdais2003_daily_calculation_VA_fullday)
data.Lourdais2003_daily_calculation_VL_fullday <- data.Lourdais2003_dailycalculation_fullday %>%
  select(-daily_cumulated_SMR_VB, -daily_cumulated_EWL_VB, 
         -daily_cumulated_SMR_VS, -daily_cumulated_EWL_VS, 
         -daily_cumulated_SMR_VA, -daily_cumulated_EWL_VA, 
         -daily_cumulated_SMR_VAm, -daily_cumulated_EWL_VAm)
data.Lourdais2003_daily_calculation_VL_fullday <- data.Lourdais2003_daily_calculation_VL_fullday %>%
  mutate(Species = factor("Vipera latastei"))
names(data.Lourdais2003_daily_calculation_VL_fullday)[names(data.Lourdais2003_daily_calculation_VL_fullday) == "daily_cumulated_SMR_VL"] <- "daily_cumulated_SMR"
names(data.Lourdais2003_daily_calculation_VL_fullday)[names(data.Lourdais2003_daily_calculation_VL_fullday) == "daily_cumulated_EWL_VL"] <- "daily_cumulated_EWL"
str(data.Lourdais2003_daily_calculation_VL_fullday)
data.Lourdais2003_daily_calculation_VAm_fullday <- data.Lourdais2003_dailycalculation_fullday %>%
  select(-daily_cumulated_SMR_VB, -daily_cumulated_EWL_VB, 
         -daily_cumulated_SMR_VS, -daily_cumulated_EWL_VS, 
         -daily_cumulated_SMR_VA, -daily_cumulated_EWL_VA, 
         -daily_cumulated_SMR_VL, -daily_cumulated_EWL_VL)
data.Lourdais2003_daily_calculation_VAm_fullday <- data.Lourdais2003_daily_calculation_VAm_fullday %>%
  mutate(Species = factor("Vipera ammodytes"))
names(data.Lourdais2003_daily_calculation_VAm_fullday)[names(data.Lourdais2003_daily_calculation_VAm_fullday) == "daily_cumulated_SMR_VAm"] <- "daily_cumulated_SMR"
names(data.Lourdais2003_daily_calculation_VAm_fullday)[names(data.Lourdais2003_daily_calculation_VAm_fullday) == "daily_cumulated_EWL_VAm"] <- "daily_cumulated_EWL"
str(data.Lourdais2003_daily_calculation_VAm_fullday)

data.Lourdais2003_daily_calculated_values_5species_factor_fullday <- bind_rows(
  data.Lourdais2003_daily_calculation_VAm_fullday,
  data.Lourdais2003_daily_calculation_VL_fullday,
  data.Lourdais2003_daily_calculation_VA_fullday,
  data.Lourdais2003_daily_calculation_VS_fullday,
  data.Lourdais2003_daily_calculation_VB_fullday)
str(data.Lourdais2003_daily_calculated_values_5species_factor_fullday)

## 3.4 Graphic representation of the mean  daily SMR and  daily EWR (full day, diurnal and nocturnal) for each species at each day type #### 
# 3.3a) Graphic representation of the mean daily SMR (full day, diurnal and nocturnal) for each species at each day type #### 

mean_se_daily_cumulated_SMR_fullday <- data.Lourdais2003_daily_calculated_values_5species_factor_fullday %>%
  group_by(Species, Heat_Day) %>%
  summarise(mean_daily_cumulated_SMR_fullday = mean(daily_cumulated_SMR, na.rm = TRUE),
            sd_daily_cumulated_SMR_fullday = sd(daily_cumulated_SMR, na.rm = TRUE),
            se_daily_cumulated_SMR_fullday = sd(daily_cumulated_SMR, na.rm = TRUE) / sqrt(n()))
mean_se_daily_cumulated_SMR_fullday

# transformation in calories
data.Lourdais2003_daily_calculated_values_5species_factor_fullday$DEE <- 4.825*data.Lourdais2003_daily_calculated_values_5species_factor_fullday$daily_cumulated_SMR

mean_se_daily_cumulated_SMR_fullday <- data.Lourdais2003_daily_calculated_values_5species_factor_fullday %>%
  group_by(Species, Heat_Day) %>%
  summarise(mean_daily_cumulated_SMR_fullday = mean(DEE, na.rm = TRUE),
            sd_daily_cumulated_SMR_fullday = sd(DEE, na.rm = TRUE),
            se_daily_cumulated_SMR_fullday = sd(DEE, na.rm = TRUE) / sqrt(n()))
mean_se_daily_cumulated_SMR_fullday$int <- interaction(mean_se_daily_cumulated_SMR_fullday$Species, mean_se_daily_cumulated_SMR_fullday$Heat_Day)
mean_se_daily_cumulated_SMR_fullday

thermal_order <- c("N", "HD", "VHD")
species_order <- c("Vipera ammodytes", "Vipera latastei", "Vipera aspis", "Vipera seoanei", "Vipera berus")

CumSMR <- ggplot(mean_se_daily_cumulated_SMR_fullday, aes(x = Species, y = mean_daily_cumulated_SMR_fullday, color = Heat_Day, group = Heat_Day)) +
  geom_point(position = position_dodge(width = 0.5), size = 7) +
  geom_errorbar(aes(ymin = mean_daily_cumulated_SMR_fullday - sd_daily_cumulated_SMR_fullday, ymax = mean_daily_cumulated_SMR_fullday + sd_daily_cumulated_SMR_fullday), 
                width = 0.3, position = position_dodge(width = 0.5), size = 1.5) +
  labs(x = "", y = "MDEE (cal.day-1)", color = "Day type") + 
  scale_color_manual(values = c("N" = "seagreen4", "HD" = "darkorange2", "VHD" = "orangered4")) +  # Set colors for each heat_day type
  theme_bw() +
  theme(axis.line = element_line(color = "black"),  # Set axis lines to black color
        axis.ticks = element_line(color = "black"),  # Set ticks to black color
        axis.text.x = element_text(size =33, face = "italic", color = "black"),
        axis.text.y = element_text(size = 23),
        axis.title.y = element_text(vjust = 1, size = 40),
        panel.grid.major = element_line(color = "gray", size = 0.5, linetype = "dotted")) +  # Add semi-transparent grid
    scale_y_continuous(limits = c(0, 1100), breaks = seq(0, 1100, by = 300))  # Set y-axis range and tick marks

print(CumSMR)

ggsave(
  "cumulated_SMR_emmeans.jpeg",
  plot = CumSMR,
  device = "jpeg",
  width = 23, #17
  height = 10,
  path = "",
  dpi = 300,
  bg = "white"
)


# 3.4b) Graphic representation of the mean daily EWL (full day, diurnal and nocturnal) for each species at each day type #### 

mean_se_daily_cumulated_EWL_fullday <- data.Lourdais2003_daily_calculated_values_5species_factor_fullday %>%
  group_by(Species, Heat_Day) %>%
  summarise(mean_daily_cumulated_EWL_fullday = mean(daily_cumulated_EWL, na.rm = TRUE),
            sd_daily_cumulated_EWL_fullday = sd(daily_cumulated_EWL, na.rm = TRUE),
            se_daily_cumulated_EWL_fullday = sd(daily_cumulated_EWL, na.rm = TRUE) / sqrt(n()))
mean_se_daily_cumulated_EWL_fullday

thermal_order <- c("N", "HD", "VHD")
species_order <- c("Vipera ammodytes", "Vipera latastei", "Vipera aspis", "Vipera seoanei", "Vipera berus")

cumEWL <- ggplot(mean_se_daily_cumulated_EWL_fullday, aes(x = Species, y = mean_daily_cumulated_EWL_fullday, color = Heat_Day, group = Heat_Day)) +
  geom_point(position = position_dodge(width = 0.5), size = 7) +
  geom_errorbar(aes(ymin = mean_daily_cumulated_EWL_fullday - sd_daily_cumulated_EWL_fullday, ymax = mean_daily_cumulated_EWL_fullday + sd_daily_cumulated_EWL_fullday), 
                width = 0.3, position = position_dodge(width = 0.5), size = 1.5) +
  labs(x = "", y = "MDEWL(mg.day-1)", color = "Day type") +
  scale_color_manual(values = c("N" = "seagreen4", "HD" = "darkorange2", "VHD" = "orangered4")) +  # Set colors for each heat_day type
  theme_bw() +
  theme(axis.line = element_line(color = "black"),  # Set axis lines to black color
        axis.ticks = element_line(color = "black"),  # Set ticks to black color
        axis.text.x = element_text(size =33, face = "italic", color = "black"),
        axis.text.y = element_text(size = 23),
        axis.title.y = element_text(vjust = 1, size = 40),
        panel.grid.major = element_line(color = "gray", size = 0.5, linetype = "dotted"))  +  # Add semi-transparent grid
  scale_y_continuous(limits = c(45, 1300), breaks = seq(0, 1300, by = 300))

print(cumEWL)

ggsave(
  "cumulated_EWL_emmeans.jpeg",
  plot = cumEWL,
  device = "jpeg",
  width = 23, #17
  height = 10,
  path = "",
  dpi = 300,
  bg = "white"
)

### 4. Models to test differences between species and day types ####

## 4.2 Models to test daily (fullday) cumulated SMR differences between species and day types ####

model_daily_cumulated_SMR_fullday <- lme(DEE ~ Heat_Day*Species, random=list(~1|DateF), na.action=na.omit, data=data.Lourdais2003_daily_calculated_values_5species_factor_fullday)

anova(model_daily_cumulated_SMR_fullday, type = "marginal")

summary(model_daily_cumulated_SMR_fullday)
emmeans(model_daily_cumulated_SMR_fullday, list(pairwise ~ Heat_Day*Species),adjust="tukey")


## 4.3 Models to test daily (fullday) cumulated EWL differences between species and day types ####

model_daily_cumulated_EWL_fullday <- lme(daily_cumulated_EWL ~ Heat_Day*Species, random=list(~1|DateF), na.action=na.omit, data=data.Lourdais2003_daily_calculated_values_5species_factor_fullday)

anova(model_daily_cumulated_EWL_fullday, type = "marginal")

summary(model_daily_cumulated_EWL_fullday)
emmeans(model_daily_cumulated_EWL_fullday, list(pairwise ~ Heat_Day*Species),adjust="tukey")





