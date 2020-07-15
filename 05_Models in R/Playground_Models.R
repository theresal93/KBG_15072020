################################### Models ###################################

# load packages
library(mfx)
library(nnet)
library(foreign)
library(haven)
library(dplyr)  
library(descr)
require(readstata13)
library(AER)
library(MASS)
library(stargazer)
library(DescTools)
library(stringr)

# load data
# KBG_work_4 <- read_sav("Y:/common/Projekte/2 Laufende Projekte/Kinderbetreuungsgeldkonto - Evaluierung ab 2018/Quant_BezieherInnen_2019/Daten/KBG_work_4.sav")

source("Z:/KBG/02_Prepare Data in R/New_Variables_KBG.R")
KBG_work_4 <- KBG_work_3

#### 1. Logit binary model ####

#### 1.1 Base model with factor scores #### 

## zwei Syteme : Einkommensabhängig= 1
##               Konto=0

# factorscores <- c(KBG_work_4$FAC1_2, KBG_work_4$FAC2_2, KBG_work_4$FAC3_2, KBG_work_4$FAC4_2, KBG_work_4$FAC5_2)

KBG_work_4$logiteinkommen <- ifelse(KBG_work_4$F2_2==1, 0, # Konto
                                    ifelse(KBG_work_4$F2_2==2, 1, NA)) # Einkommensabhängig
KBG_work_4$logiteinkommen <- factor(KBG_work_4$logiteinkommen,labels = c("Konto", "Einkommensabh?ngig"))
KBG_work_4$logiteinkommen  <- relevel(KBG_work_4$logiteinkommen , "Konto") # base = Konto


logit1<- glm(logiteinkommen ~ FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2, family = binomial(link = "logit"), data = KBG_work_4)
summary(logit1)

# average marginal effect
logit1mfx_avg<- logitmfx(logiteinkommen ~ FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2,  data = KBG_work_4, atmean = TRUE)
# average partial effects
logit1mfx_part<- logitmfx(logiteinkommen ~ FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2,  data = KBG_work_4, atmean = FALSE)

#### 1.2 model with additional explanatory varaibles #### 

# S01: Geschlecht 1 weiblich 2 m?nnlich
# F1_1: Erwerbsstatus vor Geburt
# value                                                                  label
# 1                                           Unselbstst?ndig erwerbst?tig
# 2                                             Selbstst?ndig erwerbst?tig
# 3 Studierend bzw. in Ausbildung (Pr?senzdienst, freiwilliges Sozialjahr)
# 4                                                                 Karenz
# 5                                                      im Haushalt t?tig
# 6                                                             Arbeitslos
# S02: Alter
# S07: Bildungslevel
# S05: Anzahl Kinder
# S11: Einstellung Familie # kein Einfluss
# S03: lebt mit Partner_in zusammen

# beziehtPB # kein Einfluss
# F2.10: wann Entscheidung 
# F2.11: FZB # kein Einfluss
# F3.3: Relevanz tageweise
# S09: Bundesland # kein Einfluss
# S10: Einwohner Gemeinde # kein Einlfuss
# KBGm: KBG-Bezug des Mannes - kein Einfluss
# KBGw: KBG-Bezug der Frau - Einfluss (macht aber eher wenig sinn da reverse causality)


colnames(KBG_work_4[, (grepl("F1_1_.", names(KBG_work_4)))])
colnames(KBG_work_4[, (grepl("S07_.", names(KBG_work_4)))])

KBG_work_4$S01  <- relevel(KBG_work_4$S01 , "weiblich") # base = weiblich
KBG_work_4$F1_1  <- relevel(KBG_work_4$F1_1 , "unselbststaending") # base = unselbststaending
KBG_work_4$S07  <- relevel(KBG_work_4$S07 , "studium") # base = studium 

KBG_work_4$F3_3 <- relevel(as_factor(KBG_work_4$F3_3), "gar nicht relevant") # base = gar nicht relevant

# Wie viele Beobachtungen fallen durch NA in Regression heraus
# logit1: 41
summary(complete.cases(cbind(KBG_work_4$logiteinkommen)))
# logit2: 46
summary(complete.cases(cbind(KBG_work_4$logiteinkommen, KBG_work_4$FAC1_2, KBG_work_4$FAC2_2, KBG_work_4$FAC3_2, KBG_work_4$FAC4_2, KBG_work_4$FAC5_2, KBG_work_4$S01, KBG_work_4$F1_1, KBG_work_4$S07, KBG_work_4$S05, KBG_work_4$S03)))

cbind(KBG_work_4$FAC1_2, KBG_work_4$FAC2_2, KBG_work_4$FAC3_2, KBG_work_4$FAC4_2, KBG_work_4$FAC5_2,
  KBG_work_4$S01, KBG_work_4$F1_1, KBG_work_4$S07, KBG_work_4$S05, KBG_work_4$S03)

logit2mfx_part<- logitmfx(logiteinkommen ~ 
                            FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2 +
                            # S01 + # nicht signfifikant
                            S02 + 
                            F1_1 +
                            S07 +
                            S05 +
                            S03 + F3_3 + F2_10 + S10 + F2_11
                          , data = KBG_work_4, atmean = FALSE)
logit2mfx_part

logit2<- glm(logiteinkommen ~ 
                            FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2 +
                            # S01 + # nicht signfifikant
                            S02 + 
                            F1_1 +
                            S07 +
                            S05 +
                            S03
                          , data = KBG_work_4, family = binomial(link = "logit"))


# Notizen
# summary(KBG_work_4$F1_1)
# unselbststaending   selbststaending        studierend            karenz          haushalt        arbeitslos 
#               690                58                22               114                43                73 
# summary(KBG_work_4$S07)
#               studium pflichtschule         lehre    fachschule        matura          NA's 
#                   406            47           202           116           223             6
