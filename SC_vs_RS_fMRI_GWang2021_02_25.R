#control+L to clear console
#control+shift+fnF10 to clear workspace
# setwd('/Users/harissair/Desktop/Eli');
# getwd();

library(ggplot2)
library(dplyr)
library(pastecs)
library(tidyr)
library(readxl)
mydata <- read_excel("../data/SCvsRSfMRI_motor_network_2021_02_25.xlsx", sheet = "Sair_Gujar_final_ratings")
# View(SCvsRSfMRI_motor_network_2021_02_25)# summary(mydata);
subjID = sapply(1:nrow(mydata), function(r) readr::parse_number(as.character(mydata[r,1])))
# information on variable:
# newdata<-mydata[-c(1:5,15,19:20)]
# #remove single quotes from numerical data columns
# newdata$EV_dSMN<-gsub("'",'',newdata$EV_dSMN)
# newdata$TV_dSMN<-gsub("'",'',newdata$TV_dSMN)
# newdata$EV_vSMN<-gsub("'",'',newdata$EV_vSMN)
# newdata$TV_vSMN<-gsub("'",'',newdata$TV_vSMN)
# 
# #convert EV and TV data to numbers
# newdata$EV_dSMN<-as.numeric(newdata$EV_dSMN)
# newdata$TV_dSMN<-as.numeric(newdata$TV_dSMN)
# newdata$EV_vSMN<-as.numeric(newdata$EV_vSMN)
# newdata$TV_vSMN<-as.numeric(newdata$TV_vSMN)

#convert n/a values to blank
# newdata$FTbeforeSC<-gsub("n/a",'',newdata$FTbeforeSC)

# datAnalysis <- data.frame(id = newdata$Subj_ID_final,
#                           type = newdata$type,
#                           dSMV = (newdata$dorsal.motor!="[]") * 1,
#                           vSMV = (newdata$ventral_motor!="[]") * 1,
#                           CCD = newdata$FT_concordance,
#                           FM_present = newdata$FINGM_present,
#                           FTbeforeSC = newdata$FTbeforeSC
#                           )
datAnalysis0 <- data.frame(id = subjID,
                          typeSC = (mydata$type == "sc")*1,
                          dSMV = (!is.na(mydata$`consensus dorsal`)) * 1,
                          vSMV = (!is.na(mydata$`consensus ventral`))*1,
                          CCD = mydata$`FT concordance`,
                          ftBeforeSc = mydata$FTbeforeSC == "y",
                          FM_present = mydata$`FINGM present?`=="y",
                          D.rater1 = mydata$`dorsal motor`,
                          D.rater2 = as.numeric(mydata$`sachin dorsl revies`),
                          V.rater1 = as.numeric(mydata$`ventral motor`),
                          V.rater2 = mydata$`sachin ventral review`
                          )
datAnalysis0$MoterTask <- ifelse(datAnalysis$CCD=="no motor fmri", 0,1)
# convert NA to zeros
datAnalysis = datAnalysis0
datAnalysis[is.na(datAnalysis0)] = 0

# kappa's agreement ---------------------------------
library(irr)
kp.1 = kappa2(cbind(datAnalysis$D.rater1, datAnalysis$D.rater2))
kp.1
kp.2 = kappa2(cbind(datAnalysis$V.rater1, datAnalysis$V.rater2))
kp.2

# concordance VS FTbeforeSC -----------------------------------------------
library(lme4)
dat1 <- datAnalysis %>% 
  filter(CCD!="no motor fmri") %>%
  droplevels()
dat1$GoodCCD <- ifelse(dat1$CCD=="good", 1,0)
# dat1$ftBeforeSc <- ifelse(dat1$FTbeforeSC=="y",1,0)
# dat1$typeSC <- ifelse(dat1$type == "sc", 1, 0)

# fit1.0.0 <- glm(GoodCCD ~ typeSC, data = dat1, family = binomial)
# summary(fit1.0.0)

fit1.0.ri <- glmer(GoodCCD ~ (1|id) + typeSC, family = binomial, data = dat1)
summary(fit1.0.ri)
coefficients(fit1.0.ri)
confint(fit1.0.ri)

fit1.ri <- glmer(GoodCCD ~ (1|id) + typeSC + ftBeforeSc, family = binomial, data = dat1)
smy.1.ri <- summary(fit1.ri)
smy.1.ri$coefficients[,1] - 1.96*smy.1.ri$coefficients[,2]
smy.1.ri$coefficients[,1] + 1.96*smy.1.ri$coefficients[,2]
confint(fit1.ri)

fit1.1.ri <- glmer(GoodCCD ~ (1|id) + typeSC * ftBeforeSc, family = binomial, data = dat1)
summary(fit1.1.ri)

smy.1.ri <- summary(fit1.1.ri)
smy.1.ri$coefficients[,1]
smy.1.ri$coefficients[,1] - 1.96*smy.1.ri$coefficients[,2]
smy.1.ri$coefficients[,1] + 1.96*smy.1.ri$coefficients[,2]


fit1.2.ri <- glmer(GoodCCD ~ (1|id) + typeSC + typeSC : ftBeforeSc, family = binomial, data = dat1)
summary(fit1.2.ri)

smy.1.ri <- summary(fit1.2.ri)
smy.1.ri$coefficients[,1]
smy.1.ri$coefficients[,1] - 1.96*smy.1.ri$coefficients[,2]
smy.1.ri$coefficients[,1] + 1.96*smy.1.ri$coefficients[,2]


### Check the random intercept variable by bootstrapping 
pbgmm <- pbnm::pbnm(fit1.0.ri,fit1.0.0, cores = 4, tasks = 10, seed = 1)
summary(pbgmm)
# 
# confs = confint(ri.fit)
# exp(confs)


# motor detected VS types(RS and SC) --------------------------------------

fit2.ri <- glmer(data = datAnalysis, formula = dSMV ~ (1|id) + typeSC * MoterTask, family = "binomial")
summary(fit2.ri)
smy.1.ri <- summary(fit2.ri)
smy.1.ri$coefficients[,1] %>% round(3)
(smy.1.ri$coefficients[,1] - 1.96*smy.1.ri$coefficients[,2]) %>% round(3)
(smy.1.ri$coefficients[,1] + 1.96*smy.1.ri$coefficients[,2]) %>% round(3)


fit2.ri.1 <- glmer(data = datAnalysis, formula = dSMV ~ (1|id) + typeSC + typeSC : MoterTask, family = "binomial")
summary(fit2.ri.1)
smy.1.ri <- summary(fit2.ri.1)
smy.1.ri$coefficients[,1] %>% round(3)
(smy.1.ri$coefficients[,1] - 1.96*smy.1.ri$coefficients[,2]) %>% round(3)
(smy.1.ri$coefficients[,1] + 1.96*smy.1.ri$coefficients[,2]) %>% round(3)



fit2.ri.2 <- glmer(data = datAnalysis, formula = dSMV ~ (1|id) + typeSC + MoterTask, family = "binomial")
summary(fit2.ri.2)
smy.1.ri <- summary(fit2.ri.2)
smy.1.ri$coefficients[,1] %>% round(3)
(smy.1.ri$coefficients[,1] - 1.96*smy.1.ri$coefficients[,2]) %>% round(3)
(smy.1.ri$coefficients[,1] + 1.96*smy.1.ri$coefficients[,2]) %>% round(3)


fit2.ri.3 <- glmer(data = datAnalysis, formula = dSMV ~ (1|id) + typeSC, family = "binomial")
summary(fit2.ri.3)

smy.1.ri <- summary(fit2.ri.3)
smy.1.ri$coefficients[,1] %>% round(3)
(smy.1.ri$coefficients[,1] - 1.96*smy.1.ri$coefficients[,2]) %>% round(3)
(smy.1.ri$coefficients[,1] + 1.96*smy.1.ri$coefficients[,2]) %>% round(3)


anova(fit2.ri, fit2.ri.3)

fit3.ri <- glmer(data = datAnalysis, formula = vSMV ~ (1|id) + typeSC * MoterTask, family = "binomial", nAGQ=17)
summary(fit3.ri)
smy.1.ri <- summary(fit3.ri)
smy.1.ri$coefficients[,1] %>% round(3)
(smy.1.ri$coefficients[,1] - 1.96*smy.1.ri$coefficients[,2]) %>% round(3)
(smy.1.ri$coefficients[,1] + 1.96*smy.1.ri$coefficients[,2]) %>% round(3)


fit3.ri.1 <- glmer(data = datAnalysis, formula = vSMV ~ (1|id) + typeSC + typeSC : MoterTask, family = "binomial", nAGQ=17)
summary(fit3.ri.1)
smy.1.ri <- summary(fit3.ri.1)
smy.1.ri$coefficients[,1] %>% round(3)
(smy.1.ri$coefficients[,1] - 1.96*smy.1.ri$coefficients[,2]) %>% round(3)
(smy.1.ri$coefficients[,1] + 1.96*smy.1.ri$coefficients[,2]) %>% round(3)

fit3.ri.2 <- glmer(data = datAnalysis, formula = vSMV ~ (1|id) + typeSC + MoterTask, family = "binomial", nAGQ=17)
summary(fit3.ri.2)
smy.1.ri <- summary(fit3.ri.2)
smy.1.ri$coefficients[,1] %>% round(3)
(smy.1.ri$coefficients[,1] - 1.96*smy.1.ri$coefficients[,2]) %>% round(3)
(smy.1.ri$coefficients[,1] + 1.96*smy.1.ri$coefficients[,2]) %>% round(3)

fit3.ri.3 <- glmer(data = datAnalysis, formula = vSMV ~ (1|id) + typeSC, family = "binomial", nAGQ=17)
summary(fit3.ri.3)
smy.1.ri <- summary(fit3.ri.3)
smy.1.ri$coefficients[,1] %>% round(3)
(smy.1.ri$coefficients[,1] - 1.96*smy.1.ri$coefficients[,2]) %>% round(3)
(smy.1.ri$coefficients[,1] + 1.96*smy.1.ri$coefficients[,2]) %>% round(3)

anova(fit3.ri, fit3.ri.3)

# fit2.c <- clogit(data = datAnalysis, formula = dSMV ~ type+strata(id))
# summary(fit2.c)
# 
# fit2.c1 <- clogit(data = datAnalysis, formula = vSMV ~ type+strata(id))
# summary(fit2.c1)

# bivariate logistic  -----------------------------------------------------
# MCGLM -------------------------------------------------------------------

#devtools::install_github("wbonat/mcglm")
library(mcglm)
# data("dietox", package = "geepack")
# 
# Z0_ex2 <- mc_id(dietox)
# Z1_ex2 <- mc_mixed(~ 0 + Pig, data = dietox)
# fit0_ex2 <- mcglm(linear_pred = c(Weight ~ 1), matrix_pred = list(resp = c(Z0_ex2, Z1_ex2)), data = dietox)
cor(datAnalysis$dSMV, datAnalysis$vSMV)

Z0 = mc_id(datAnalysis)
Z1 = mc_mixed(~ 0 + id, data = datAnalysis) 
form.dSMV = dSMV ~ typeSC * MoterTask
form.vSMV = vSMV ~ typeSC * MoterTask

blfit0 = mcglm(linear_pred = c(form.dSMV, form.vSMV), 
               matrix_pred = list(c(Z0, Z1), c(Z0, Z1)), 
               data = datAnalysis,
               link = c("logit", "logit"), 
               variance = c("binomialP", "binomialP"))
summary(blfit0)
anova(blfit0)

form.dSMV1 = dSMV ~ 1
form.vSMV1 = vSMV ~ 1

blfit1 = mcglm(linear_pred = c(form.dSMV, form.vSMV), 
               matrix_pred = list(c(Z0, Z1), c(Z0, Z1)), 
               data = datAnalysis,
               link = c("logit", "logit"), 
               variance = c("binomialP", "binomialP"))
summary(blfit1)

anova(blfit0, blfit1)
summary(blfit0, print = "power")




