################################### Models Logit - systeme binary model ###################################
# the variables are based on the respondent and  his/her* partner
# in this model we also take into account that the household decision making for the system might also depend on the characteristics of the partner.
# Due to the building of the variables in this analysis only individuals who are living together with its partner can be included.
# More details on the building of the varialbes see New_Variables_KBG.R

# Several logit models are estimated to check which characteristics define the likelihood to choose the income depending KBG version:

# The selection of the variables is based on the models in Model_Logit_respontent.R. This means that for example in 1. logit1 the same variables as in
# the script Model_Logit_respondent.R are included. For more details see (Model_Logit_respondent.R)
# 1. logit1: Hard Facts model
#            A model that includes the characteristics that seem to be the most reasonable onees to impact this decision. That is:
#            - F1_1: Erwerbstatus vor der Geburt
#            - S01: Geschlecht
#            - S02: Alter
#            - S07: Bildung
#            - S05: Anzahl Kinder
#            Not included: S03: Partner*in (ja/nein) because we only have individuals with partner
#            income (F1_1) is not included because we would loose too many observations
# 2. logit2: Hard Facts model plus factor scores (excluding non sign. ones)
#            Now in additon factor scores (of PCA see: ) are included, the insiginificant variable S01 (Geschlecht) is excluded
# 3. logit3: Hard Facts model plus factor scores plus additional variables
#            Now in addition the follwoing variables are included:
#            Ob Entscheidung von oder nach der Geburt fiel (F2_10), wie relevant die tageweise Inaspruchnahme des Kontos war, Wochenarbeitsstunden vor Geburt,
#            ob sich der Vater am KBG beteiligt
#            Also, the impact of further variables that showed, however, no significant effect. That is:
#            S11: Einstellung Familie 
#            - F2_11: FZB
#            - beziehtPB 
#            - S09: Bundesland 
#            - S10: Einwohner Gemeinde 
# 4. logit4: Erwerbstätigen Model: Subset model to include income
#            Now in addition also income (F1_3: monatliches Nettoeinkommen vor der Geburt) was included
#            Note regarding model 4.1 : And also an additional model excluding all the irrelevant variables was included.
#                                 4.2 : And also an additional model exuding income with the comple.case dataset of logit4.1 was estimated to check the effect 
#            of income
# 5. logit5: model with men who take KBG
#            This model is based on a subset which means that only men who participate in KBG are included. The theory behind this is that especially if men 
#            participate the choice between Einkommensabhängig and KBG is likely to depend also on the characteristics of the man. However, the problem of this
#            model is that the remaining subset dataset is small which does not really allow any conclusion.
# 6. Stargazer Output
#            Summarized all the models in this script. The coefficients are average partial effects. The constant is not shown because of marginal effects
--------------------------------------------------------------------------------------------------------------------------------------------------------------------

# import the needed variables
# note: also dependency to R_dummies
source("Z:/KBG/02_Prepare Data in R/New_Variables_KBG.R")

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



#### 1. logit1: Hard Facts model for men and women (only those who have partner) ####

## summary of considered variables

# DEPENDENT BINARY VARIABLE
# logiteinkommen: KBG system (base Konto)
# Konto Einkommensabh?ngig               NA's 
#   586                373                 41 

# INDEPENDENT VARIABLES
# Erwerbsstatusw: Erwerbsstatus vor Geburt Frau (unselbsttaendig base)
# unselbststaending   selbststaending        studierend            karenz          haushalt        arbeitslos              NA's 
#               682                52                22               120                46                75                 3 
# # Erwerbstatusm:  Erwerbsstatus vor Geburt Mann (unselbsttaendig base)
# unselbststaending   selbststaending        studierend            karenz          haushalt        arbeitslos              NA's 
#               757               106                 7                 6                 5                30                89 
# Erwerbsstausm NA's + Erwerbsstatusw NA's = 92 (= S03 NA's)

# S01: Geschlecht 1 weiblich (base) 2 m?nnlich
# weiblich männlich NA's
#      905       95    0

# Alterw: Alter Frau
# Alterm: Alter Mann
# Alterm NA's + Alterw NA's = 92 (= S03 NA's)


# Bildungw: Bildungslevel Frau (studium base)
# studium pflichtschule         lehre    fachschule        matura          NA's 
#     365            58           196           127           245             9 

# Bildungm: Bildungslevel Mann
# studium pflichtschule         lehre    fachschule        matura          NA's 
#     87           273            96           185           262            97 

# S05: Anzahl Kinder
#   1   2   3   4   5   6   7   9  NA's
# 429 394 121  43   8   2   1   2     0

# S03: lebt mit Partner_in zusammen (ja base) - fällt hier überall raus
# ja  nein NA's
# 908   92 0

## summary of considered observations
# 869
logit1_mw_completecases <- summary(complete.cases(KBG_work_4$logiteinkommen,
                                               KBG_work_4$Erwerbsstatusm,
                                               KBG_work_4$Erwerbsstatusw,
                                               KBG_work_4$Alterw,
                                               KBG_work_4$Alterm,
                                               KBG_work_4$Bildungm,
                                               KBG_work_4$Bildungw,
                                               KBG_work_4$S01,
                                               KBG_work_4$S05))

## Model logit1_mw : logit with marginal partial effects
logit1_mw <- glm(logiteinkommen ~ 
                   Erwerbsstatusw + Erwerbsstatusm  + Alterw + Alterm + Bildungm + Bildungw + S01 + S05
                 , data = KBG_work_4, family = binomial(link = "logit"))
logit1_mw_mfx_part <- logitmfx(logiteinkommen ~ 
                             Erwerbsstatusw + Erwerbsstatusm  + Alterw + Alterm + Bildungm + Bildungw + S01 + S05
                              , data = KBG_work_4, atmean = FALSE)
logit1_mw_mfx_part


#### 2. logit2: Hard Facts model plus factor scores (excluding non sign. ones) ####
logit2_mw <- glm(logiteinkommen ~ 
                   Erwerbsstatusw + Erwerbsstatusm  + Alterw + Alterm + Bildungm + Bildungw + S01 + S05 +
                   FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2
                 , data = KBG_work_4, family = binomial(link = "logit"))
logit2_mw_mfx_part <- logitmfx(logiteinkommen ~ 
                                 Erwerbsstatusw + Erwerbsstatusm  + Alterw + Alterm + Bildungm + Bildungw + S01 + S05 +
                                 FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2
                               , data = KBG_work_4, atmean = FALSE)
logit2_mw_mfx_part

#### 3. logit3: Hard Facts model plus factor scores plus additional variables ####


# F2_10: wann Entscheidung 
# waehrendschwangerschaft              nachgeburt                    NA's 
#                     767                     219                      14 

# F3_3: Relevanz tageweise
# sehr relevant       eher relevant eher nicht relevant  gar nicht relevant 
#           131                 237                 213                 419 

# Stundenw: Wochenarbeitsstunden Frau (viele NA's daher erst hier)
# 268 NA's
# Stundenm: Wochenarbeitsstunden Frau (viele NA's daher erst hier)
# 137 NA's

# KBGm_beteiligt: Männer, die am KBG beteiligt sind (0 - nicht beteiligt, 1 - beteiligt)
# 0    1 NA's 
# 626  248  126 

## summary of considered observations
# 622
logit3_mw_completecases <- summary(complete.cases(KBG_work_4$logiteinkommen,
                                                  KBG_work_4$Erwerbsstatusm,
                                                  KBG_work_4$Erwerbsstatusw,
                                                  KBG_work_4$Alterw,
                                                  KBG_work_4$Alterm,
                                                  KBG_work_4$Bildungm,
                                                  KBG_work_4$Bildungw,
                                                  KBG_work_4$S01,
                                                  KBG_work_4$S05,
                                                  
                                                  KBG_work_4$FAC1_2,
                                                  KBG_work_4$FAC2_2, 
                                                  KBG_work_4$FAC3_2,  
                                                  KBG_work_4$FAC4_2, 
                                                  KBG_work_4$FAC5_2,
                                                  
                                                  KBG_work_4$F2_10,
                                                  KBG_work_4$F3_3,
                                                  KBG_work_4$Stundenw,
                                                  KBG_work_4$Stundenm,
                                                  KBG_work_4$KBGm_beteiligt))


## Model logit3: logit with marginal partial effects
logit3_mw <- glm(logiteinkommen ~ 
                   Erwerbsstatusw + Erwerbsstatusm  + Alterw + Alterm + Bildungm + Bildungw + S01 + S05 +
                   FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2 +
                   F2_10 + F3_3 + Stundenw + Stundenm + KBGm_beteiligt
                 , data = KBG_work_4, family = binomial(link = "logit"))
logit3_mw_mfx_part <- logitmfx(logiteinkommen ~ 
                                 Erwerbsstatusw + Erwerbsstatusm  + Alterw + Alterm + Bildungm + Bildungw + S01 + S05 +
                                 FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2 +
                                 F2_10 + F3_3 + Stundenw + Stundenm + KBGm_beteiligt
                               , data = KBG_work_4, atmean = FALSE)
logit3_mw_mfx_part



#### 4. logit4: Erwerbstätigen Model: Subset model to include income  ####

## summary of additionally considered variables
# Einkommenw: monatliches Nettoeinkommen vor der Geburt Frau
          # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
          # 250    1100    1500    1611    2000    5800     459 
# Einkommenm: monatliches Nettoeinkommen vor der Geburt Mann
          #    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
          #    120    1600    2000    2073    2400    8500     464

## summary of considered observations
# 381
logit4_mw_completecases <- summary(complete.cases(KBG_work_4$logiteinkommen,
                                                  KBG_work_4$Erwerbsstatusm,
                                                  KBG_work_4$Erwerbsstatusw,
                                                  KBG_work_4$Alterw,
                                                  KBG_work_4$Alterm,
                                                  KBG_work_4$Bildungm,
                                                  KBG_work_4$Bildungw,
                                                  KBG_work_4$S01,
                                                  KBG_work_4$S05,
                                                  
                                                  KBG_work_4$FAC1_2,
                                                  KBG_work_4$FAC2_2, 
                                                  KBG_work_4$FAC3_2,  
                                                  KBG_work_4$FAC4_2, 
                                                  KBG_work_4$FAC5_2,
                                                  
                                                  KBG_work_4$F2_10,
                                                  KBG_work_4$F3_3,
                                                  KBG_work_4$Stundenw,
                                                  KBG_work_4$Stundenm,
                                                  KBG_work_4$KBGm_beteiligt,
                                                  
                                                  KBG_work_4$Einkommenw,
                                                  KBG_work_4$Einkommenm))

## Model logit4: logit with marginal partial effects
logit4_mw <- glm(logiteinkommen ~ 
                                 Erwerbsstatusw + Erwerbsstatusm  + Alterw + Alterm + Bildungm + Bildungw + S01 + S05 +
                                 FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2 +
                                 F2_10 + F3_3 + Stundenw + Stundenm + KBGm_beteiligt +
                                 Einkommenw + Einkommenm
                               , data = KBG_work_4, binomial(link = "logit"))

logit4_mw_mfx_part <- logitmfx(logiteinkommen ~ 
                                 Erwerbsstatusw + Erwerbsstatusm  + Alterw + Alterm + Bildungm + Bildungw + S01 + S05 +
                                 FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2 +
                                 F2_10 + F3_3 + Stundenw + Stundenm + KBGm_beteiligt +
                                 Einkommenw + Einkommenm
                               , data = KBG_work_4, atmean = FALSE)
logit4_mw_mfx_part

# only with relavant Variables
# 383
KBG_work_4$competecases_income_mw <- (complete.cases(KBG_work_4$logiteinkommen,
                                                  #KBG_work_4$Erwerbsstatusm,
                                                  KBG_work_4$Erwerbsstatusw,
                                                  #KBG_work_4$Alterw,
                                                  #KBG_work_4$Alterm,
                                                  #KBG_work_4$Bildungm,
                                                  KBG_work_4$Bildungw,
                                                  #KBG_work_4$S01,
                                                  KBG_work_4$S05,
                                                  
                                                  #KBG_work_4$FAC1_2,
                                                  #KBG_work_4$FAC2_2, 
                                                  #KBG_work_4$FAC3_2,  
                                                  #KBG_work_4$FAC4_2, 
                                                  #KBG_work_4$FAC5_2,
                                                  
                                                  #KBG_work_4$F2_10,
                                                  KBG_work_4$F3_3,
                                                  #KBG_work_4$Stundenw,
                                                  #KBG_work_4$Stundenm,
                                                  KBG_work_4$KBGm_beteiligt,
                                                  
                                                  KBG_work_4$Einkommenw,
                                                  KBG_work_4$Einkommenm))


logit4.1_mw <- glm(logiteinkommen ~ 
                                   Erwerbsstatusw    + Bildungw  + S05 +
                                   FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2 +
                                   F3_3   + KBGm_beteiligt +
                                   Einkommenw + Einkommenm
                                 , data = KBG_work_4, binomial(link = "logit"))
logit4.1_mw_mfx_part <- logitmfx(logiteinkommen ~ 
                                 Erwerbsstatusw    + Bildungw  + S05 +
                                 FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2 +
                                  F3_3   + KBGm_beteiligt +
                                 Einkommenw + Einkommenm
                               , data = KBG_work_4, atmean = FALSE)
logit4.1_mw_mfx_part

# same model but without Einkommenm and Einkommenw i.e. use only 533 observations of logit4_completecases to check whether Einkommen makes a difference
logit4.2 <- glm(logiteinkommen ~ 
                                   Erwerbsstatusw    + Bildungw  + S05 +
                                   FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2 +
                                   F3_3   + KBGm_beteiligt 
                                 , data =  KBG_work_4[which(KBG_work_4$competecases_income_mw==TRUE),], binomial(link = "logit"))

logit4.2_mw_mfx_part <- logitmfx(logiteinkommen ~ 
                                   Erwerbsstatusw    + Bildungw  + S05 +
                                   FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2 +
                                   F3_3   + KBGm_beteiligt 
                                 , data =  KBG_work_4[which(KBG_work_4$competecases_income_mw==TRUE),], atmean = FALSE)
logit4.2_mw_mfx_part


#### 5. logit5: model with men who take KBG  ####
# Check whether variables of men have effect when they also take part in KBG


# subset nur wo sich Männer beteiligen # rest kein Einfluss
beteiligt <- KBG_work_4[which(KBG_work_4$KBGm_beteiligt==1),]

# 119
logit5_mw_completecases <- summary(complete.cases(beteiligt$logiteinkommen,
                                               beteiligt$Erwerbsstatusw,
                                               beteiligt$Erwerbsstatusm,
                                               beteiligt$Bildungw,
                                               beteiligt$Bildungm,
                                               # beteiligt$Alterw,
                                               # beteiligt$Alterm,
                                               beteiligt$Einkommenm,
                                               beteiligt$Einkommenw))
                                               # beteiligt$Stundenw,
                                               # beteiligt$Stundenm))

## Since the remaining dataset is very small, number of factor levels need to be reduced
beteiligt$Erwerbsstatusm <- factor(ifelse(beteiligt$Erwerbsstatusm %in% c("unselbststaending"), "unselbststaending",
                                           ifelse(is.na(beteiligt$Erwerbsstatusm)==TRUE, NA, "anderes")))
beteiligt$Erwerbsstatusw <- factor(ifelse(beteiligt$Erwerbsstatusw %in% c("unselbststaending"), "unselbststaending",
                                           ifelse(is.na(beteiligt$Erwerbsstatusw)==TRUE, NA, "anderes")))

beteiligt$Bildungw <- factor(ifelse(beteiligt$Bildungw %in% c("studium", "matura"), "hoch",
                                     ifelse(is.na(beteiligt$Bildungw)==TRUE, NA, "niedrig")))
beteiligt$Bildungm <- factor(ifelse(beteiligt$Bildungm %in% c("studium", "matura"), "hoch",
                                     ifelse(is.na(beteiligt$Bildungm)==TRUE, NA, "niedrig")))


## Model logit5: logit with marginal partial effects
logit5_mw <- glm(logiteinkommen ~ 
                             Erwerbsstatusw + Erwerbsstatusm    +
                             Einkommenw + Einkommenm 
                           #Bildungw + Bildungm (education does not matter)
                           , data = beteiligt, binomial(link = "logit"))

logit5_mw_mfx_part <- logitmfx(logiteinkommen ~ 
                             Erwerbsstatusw + Erwerbsstatusm    +
                             Einkommenw + Einkommenm 
                             #Bildungw + Bildungm (education does not matter)
                           , data = beteiligt, atmean = FALSE)
logit5_mw_mfx_part

#### 6. Stargazer Output ####

stargazer(logit1_mw, logit2_mw, logit3_mw, logit4_mw, logit4.1_mw, logit4.1_mw, logit5_mw,
          coef = list(c(logit1_mw_mfx_part$mfxest[,1]), c(logit2_mw_mfx_part$mfxest[,1]), c(logit3_mw_mfx_part$mfxest[,1]), 
                      c(logit4_mw_mfx_part$mfxest[,1]), c(logit4.1_mw_mfx_part$mfxest[,1]), c(logit4.2_mw_mfx_part$mfxest[,1]),
                      c(logit5_mw_mfx_part$mfxest[,1])),
          se = list(c(logit1_mw_mfx_part$mfxest[,2]), c(logit2_mw_mfx_part$mfxest[,2]), c(logit3_mw_mfx_part$mfxest[,2]),
                    c(logit4_mw_mfx_part$mfxest[,2]), c(logit4.1_mw_mfx_part$mfxest[,2]), c(logit4.2_mw_mfx_part$mfxest[,2]),
                    c(logit5_mw_mfx_part$mfxest[,2])),
          type="text", single.row = TRUE, dep.var.caption="Logit Systemwahl männlich und weiblich", intercept.bottom = FALSE, intercept.top = FALSE,
          dep.var.labels = "", column.labels = c("Hard Facts", "(1) + Factor Scores", "(2) + additonal explanatory", "(3) + income", "(4) - insignificant variables", "(5) - income", "KBGm > 0"))