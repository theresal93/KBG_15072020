########################################################## Models Logit - systeme binary model respondent ##########################################################
# now with polychoric matric as input


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

# import the needed variables
# source polychoric PCA
source("Z:/KBG/03_Factor Analysis (PCA)/PCA_Polychor.R")
# note: also dependency to R_dummies
source("Z:/KBG/02_Prepare Data in R/New_Variables_KBG.R")
rm(F2.7new, F3.2a2new, F3.2b2new, KBG_work_3, test)

# merge pcascores with KBG_work_4
KBG_work_4 <- cbind(KBG_work_4, pca_ord_full_scores, pca_ord_highcor_scores)

#This is the new data we are working with KBG_work_2020
# setwd("Y:/common/Projekte/2 Laufende Projekte/Kinderbetreuungsgeldkonto - Evaluierung ab 2018/Quant_BezieherInnen_2019/2020/Daten")
# write_sav(apply_labels(KBG_work_4), "KBG_work_2020.sav")
# # save.dta13(as.data.frame(KBG_work_4), "KBG_work_2020.dta", data.label = NULL, time.stamp = TRUE,
# #            convert.factors = TRUE, convert.dates = TRUE, tz = "GMT",
# #            add.rownames = FALSE, compress = FALSE, version = 117,
# #            convert.underscore = FALSE)
# # foreign::write.dta(KBG_work_4, "KBG_work_2020.dta",  convert.factors = c("labels", "string", "numeric", "codes"))
# write.table(KBG_work_4, file = "KBG_work_2020.csv",
#              sep = "\t", row.names = F)
# write_dta(KBG_work_4, "KBG_work_2020_1.dta", label = NULL)

#### 1. logit1: Hard Facts model ####

## summary of considered variables

# DEPENDENT BINARY VARIABLE
# logiteinkommen: KBG system (base Konto)
# Konto Einkommensabh?ngig               NA's
#   586                373                 41

# INDEPENDENT VARIABLES
# F1_1: Erwerbsstatus vor Geburt (unselbsttaendig base)
# unselbststaending   selbststaending        studierend            karenz          haushalt        arbeitslos
#               690                58                22               114                43                73
# S01: Geschlecht 1 weiblich (base) 2 m?nnlich
# weiblich männlich NA's
#      905       95    0
# S02: Alter
# 17 18 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 51 55 NA's
#  1  1  3  7 10 19 15 17 31 41 45 52 79 58 76 68 71 70 69 49 46 43 51 19 15 12 10 11  1  3  2  3  1  1  0
# S07: Bildungslevel (studium base)
# studium  pflichtschule         lehre    fachschule        matura          NA's
#     406             47           202           116           223             6
# S05: Anzahl Kinder
#   1   2   3   4   5   6   7   9  NA's
# 429 394 121  43   8   2   1   2     0
# S03: lebt mit Partner_in zusammen (ja base)
# ja  nein NA's
# 908   92 0

# Note that: income (F1_1) is not included because we would loose too many observations

## summary of considered observations
# 954 observatations are considered in logit1
logit1_completecases <-
  summary(
    complete.cases(
      KBG_work_4$logiteinkommen,
      KBG_work_4$F1_1,
      KBG_work_4$S01,
      KBG_work_4$S02,
      KBG_work_4$S07,
      KBG_work_4$S05,
      KBG_work_4$S03
    )
  )

## Model logit1: logit with marginal partial effects
logit1 <- glm(
  logiteinkommen ~
    F1_1 + S01 + S02 + S07 + S05 + S03 ,
  data = KBG_work_4,
  family = binomial(link = "logit")
)
logit1mfx_part <- logitmfx(logiteinkommen ~
                             F1_1 + S01 + S02 + S07 + S05 + S03 
                           ,
                           data = KBG_work_4,
                           atmean = TRUE)

## summary of additionally considered variables
# INDEPENDENT VARIABLES
#   All factor scores of PCA (see ...)
#   Umso höher der score umso stärker, strebt diese Person in Richtung diesem Faktor
#   (Unterschied zu FAC1_1: umso niedriger der score, umso stärker strebt diese Person in Richtung diesem Faktor)

# FAC1: "Traditionell"
# FAC2: "Modern"
# FAC3: "Übermutter"
# FAC4: "Powercouple"
# FAC5: "versuchte partnerschaftliche Familienführung"

## variables excluded (compared to logit1) because becomes insignificant
# S01: Geschlecht

## summary of considered observations
# 954 observatations are considered in logit1
logit2_completecases <-
  summary(
    complete.cases(
      KBG_work_4$logiteinkommen,
      KBG_work_4$F1_1,
      #KBG_work_4$S01, excluded
      KBG_work_4$S02,
      KBG_work_4$S07,
      KBG_work_4$S05,
      KBG_work_4$S03,
      
      KBG_work_4$FAC1_2,
      KBG_work_4$FAC2_2,
      KBG_work_4$FAC3_2,
      KBG_work_4$FAC4_2,
      KBG_work_4$FAC5_2
    )
  )


#### 2. logit2: Hard Facts model plus factor scores (excluding non sign. ones) ####
## Model logit2: logit with marginal partial effects
# full poly model
logit2_poly_full <- glm(
  logiteinkommen ~
    F1_1  + S02 + S07 + S05 + S03 +
    FAC1_poly_full + FAC2_poly_full + FAC3_poly_full + FAC4_poly_full + FAC5_poly_full , # use full poly PCA
  data = KBG_work_4,
  family = binomial(link = "logit")
)
logit2mfx_part_poly_full <- logitmfx(
  logiteinkommen ~
    F1_1  +S02 + S07 + S05 + S03 +
    FAC1_poly_full + FAC2_poly_full + FAC3_poly_full + FAC4_poly_full + FAC5_poly_full 
  ,
  data = KBG_work_4,
  atmean = FALSE
)
logit2mfx_part_poly_full

# highcor poly model
logit2_poly_highcor <- glm(
  logiteinkommen ~
    F1_1  + S02 + S07 + S05 + S03 +
    FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor , # use highcor poly PCA
  data = KBG_work_4,
  family = binomial(link = "logit")
)
logit2mfx_part_poly_highcor <- logitmfx(
  logiteinkommen ~
    F1_1   +S02 + S07 + S05 + S03 +
    FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor
  ,
  data = KBG_work_4,
  atmean = FALSE
)
logit2mfx_part_poly_highcor



#### 3. logit3: Hard Facts model plus factor scores plus additional variables ####

# F2_10: wann Entscheidung
# waehrendschwangerschaft              nachgeburt                    NA's
#                     767                     219                      14
# F3_3: Relevanz tageweise
# sehr relevant       eher relevant eher nicht relevant  gar nicht relevant
#           131                 237                 213                 419
# KBGm_beteiligt: ob sich Mann an KBG beteiligt oder nicht


# NO EFFECT
# S11: Einstellung Familie
# F2_11: FZB (auch wenn auf 2 levels aufgeteilt, kein Effekt)
# beziehtPB
# S09: Bundesland (außer vbg, wenn wien als base)
# S10: Einwohner Gemeinde
# KBGm: KBG-Bezug des Mannes

# rewrite F2_11 benntFZBnicht und anderer Grund werden zu nein
# summary(KBG_work_4$F2_11)
# before
# beziehtFZB kenntFZBnicht  andererGrund          NA's 
#           169           304           461            66 
# after
KBG_work_4$F2_11 <- factor(ifelse(KBG_work_4$F2_11=='beziehtFZB', "ja",
                                  ifelse(KBG_work_4$F2_11 %in% c("kenntFZBnicht", "andererGrund"), "nein", NA)))
# ja nein NA's 
#  169  765   66

# 862 
KBG_work_4$logit3_completecases <-
  (
    complete.cases(
      KBG_work_4$logiteinkommen,
      KBG_work_4$F1_1,
      #KBG_work_4$S01, excluded
      KBG_work_4$S02,
      KBG_work_4$S07,
      KBG_work_4$S05,
      KBG_work_4$S03,
      
      KBG_work_4$FAC1_2,
      KBG_work_4$FAC2_2,
      KBG_work_4$FAC3_2,
      KBG_work_4$FAC4_2,
      KBG_work_4$FAC5_2,
      
      KBG_work_4$F2_10,
      KBG_work_4$F3_3,
      KBG_work_4$KBGm_beteiligt
    )
  )


## Model logit3: logit with marginal partial effects
# full poly model
logit3_poly_full <- glm(
  logiteinkommen ~
    F1_1  + S02 + S07 + S05 + S03 +
    FAC1_poly_full + FAC2_poly_full + FAC3_poly_full + FAC4_poly_full + FAC5_poly_full +
    F2_10 + F3_3 + KBGm_beteiligt ,# use full poly PCA
  data = KBG_work_4,
  family = binomial(link = "logit")
)
logit3mfx_part_poly_full <- logitmfx(
  logiteinkommen ~
    F1_1  + S02 + S07 + S05 + S03 +
    FAC1_poly_full + FAC2_poly_full + FAC3_poly_full + FAC4_poly_full + FAC5_poly_full +
    F2_10 + F3_3 + KBGm_beteiligt ,
  data = KBG_work_4,
  atmean = FALSE
)
logit3mfx_part_poly_full

# highcor poly model
logit3_poly_highcor <- glm(
  logiteinkommen ~
    F1_1  + S02 + S07 + S05 + S03 +
    FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
    F2_10 + F3_3 + KBGm_beteiligt  ,# use highcor poly PCA
  data = KBG_work_4,
  family = binomial(link = "logit")
)
logit3mfx_part_poly_highcor <- logitmfx(
  logiteinkommen ~
    F1_1   +S02 + S07 + S05 + S03 +
    FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
    F2_10 + F3_3 + KBGm_beteiligt ,
  data = KBG_work_4,
  atmean = FALSE
)
logit3mfx_part_poly_highcor


#### 4. logit4: Hard Facts model plus factor scores plus additional variables plus hourly net wage  ####

# use nettoeinkommen (including imputations) instead of F1_2 and F1_3
# full poly model
logit4.2_poly_full <- glm(
  logiteinkommen ~
    F1_1  + + S07 + S05   + S03 +
    FAC1_poly_full + FAC2_poly_full + FAC3_poly_full + FAC4_poly_full + FAC5_poly_full +
    F2_10 + F3_3 + KBGm_beteiligt +
    Nettostundeneinkommen
  ,
  data = KBG_work_4,
  family = binomial(link = "logit")
)
logit4.2mfx_part_poly_full <- logitmfx(
  logiteinkommen ~
    F1_1   + S07 + S05  + S03 +
    FAC1_poly_full + FAC2_poly_full + FAC3_poly_full + FAC4_poly_full + FAC5_poly_full +
    F2_10 + F3_3 + KBGm_beteiligt +
    Nettostundeneinkommen
  ,
  data = KBG_work_4,
  atmean = FALSE
)
logit4.2mfx_part_poly_full

# highcor poly model
logit4.2_poly_highcor <- glm(
  logiteinkommen ~
    F1_1  + + S07 + S05   + S03 +
    FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
    F2_10 + F3_3 + KBGm_beteiligt +
    Nettostundeneinkommen
  ,
  data = KBG_work_4,
  family = binomial(link = "logit")
)
logit4.2mfx_part_poly_highcor <- logitmfx(
  logiteinkommen ~
    F1_1   + S07 + S05  + S03 +
    FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
    F2_10 + F3_3 + KBGm_beteiligt +
    Nettostundeneinkommen
  ,
  data = KBG_work_4,
  atmean = FALSE
)
logit4.2mfx_part_poly_highcor


# same model but without Nettoeinkommen use only 703 observations of logit4_completecases to check whether F1_3 makes a difference
KBG_work_4$competecases_income_new <-
  complete.cases(
    KBG_work_4$logiteinkommen,
    KBG_work_4$F1_1,
    #KBG_work_4$S01, excluded
    KBG_work_4$S02,
    KBG_work_4$S07,
    KBG_work_4$S05,
    KBG_work_4$S03,
    
    KBG_work_4$FAC1_2,
    KBG_work_4$FAC2_2,
    KBG_work_4$FAC3_2,
    KBG_work_4$FAC4_2,
    KBG_work_4$FAC5_2,
    
    KBG_work_4$F2_10,
    KBG_work_4$F3_3,
    KBG_work_4$KBGm_beteiligt,
    
    KBG_work_4$Nettostundeneinkommen
  )

nrow(KBG_work_4[which(KBG_work_4$competecases_income_new == TRUE), ])


# full poly model
logit4.3_poly_full <- glm(
  logiteinkommen ~
    F1_1  + + S07 + S05   + S03 +
    FAC1_poly_full + FAC2_poly_full + FAC3_poly_full + FAC4_poly_full + FAC5_poly_full +
    F2_10 + F3_3 + KBGm_beteiligt 
  ,
  data = KBG_work_4[which(KBG_work_4$competecases_income_new == TRUE), ],
  family = binomial(link = "logit")
)
logit4.3mfx_part_poly_full <- logitmfx(
  logiteinkommen ~
    F1_1   + S07 + S05  + S03 +
    FAC1_poly_full + FAC2_poly_full + FAC3_poly_full + FAC4_poly_full + FAC5_poly_full +
    F2_10 + F3_3 + KBGm_beteiligt 
  ,
  data = KBG_work_4[which(KBG_work_4$competecases_income_new == TRUE), ],
  atmean = FALSE
)
logit4.3mfx_part_poly_full

# highcor poly model
logit4.3_poly_highcor<- glm(
  logiteinkommen ~
    F1_1  + + S07 + S05   + S03 +
    FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
    F2_10 + F3_3 + KBGm_beteiligt 
  ,
  data = KBG_work_4[which(KBG_work_4$competecases_income_new == TRUE), ],
  family = binomial(link = "logit")
)
logit4.3mfx_part_poly_highcor <- logitmfx(
  logiteinkommen ~
    F1_1   + S07 + S05  + S03 +
    FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
    F2_10 + F3_3 + KBGm_beteiligt
  ,
  data = KBG_work_4[which(KBG_work_4$competecases_income_new == TRUE), ],
  atmean = FALSE
)
logit4.3mfx_part_poly_highcor



#### 5. Stargazer Output ####
## 5.1 poly full
stargazer(
  logit1, # no factors included --> no difference to Model_Logit_respondent
  logit2_poly_full,
  logit3_poly_full,
  logit4.2_poly_full,
  logit4.3_poly_full,
  coef = list(
    c(logit1mfx_part$mfxest[, 1]),
    c(logit2mfx_part_poly_full$mfxest[, 1]),
    c(logit3mfx_part_poly_full$mfxest[, 1]),
    c(logit4.2mfx_part_poly_full$mfxest[, 1]),
    c(logit4.3mfx_part_poly_full$mfxest[, 1])
  ),
  se = list(
    c(logit1mfx_part$mfxest[, 2]),
    c(logit2mfx_part_poly_full$mfxest[, 2]),
    c(logit3mfx_part_poly_full$mfxest[, 2]),
    c(logit4.2mfx_part_poly_full$mfxest[, 2]),
    c(logit4.3mfx_part_poly_full$mfxest[, 2])
  ),
  type = "text",
  single.row = TRUE,
  dep.var.caption = "Logit Systemwahl Respondent mit Polychoric Korrelationsmatrix und allen Einstellungsvariablen",
  intercept.bottom = FALSE,
  dep.var.labels = "",
  column.labels = c(
    "Hard Facts",
    "(1) + Factor Scores",
    "(2) + additonal explanatory",
    "(3) + income",
    "(4) - income"
  )
)

## 5.1 poly highcor
stargazer(
  logit1, # no factors included --> no difference to Model_Logit_respondent
  logit2_poly_highcor,
  logit3_poly_highcor,
  logit4.2_poly_highcor,
  logit4.3_poly_highcor,
  coef = list(
    c(logit1mfx_part$mfxest[, 1]),
    c(logit2mfx_part_poly_highcor$mfxest[, 1]),
    c(logit3mfx_part_poly_highcor$mfxest[, 1]),
    c(logit4.2mfx_part_poly_highcor$mfxest[, 1]),
    c(logit4.3mfx_part_poly_highcor$mfxest[, 1])
  ),
  se = list(
    c(logit1mfx_part$mfxest[, 2]),
    c(logit2mfx_part_poly_highcor$mfxest[, 2]),
    c(logit3mfx_part_poly_highcor$mfxest[, 2]),
    c(logit4.2mfx_part_poly_highcor$mfxest[, 2]),
    c(logit4.3mfx_part_poly_highcor$mfxest[, 2])
  ),
  type = "text",
  single.row = TRUE,
  dep.var.caption = "Logit Systemwahl Respondent mit Polychoric Korrelationsmatrix und nur > 0.3 korrelierten Einstellungsvariablen",
  intercept.bottom = FALSE,
  dep.var.labels = "",
  column.labels = c(
    "Hard Facts",
    "(1) + Factor Scores",
    "(2) + additonal explanatory",
    "(3) + income",
    "(4) - income"
  )
)

## Coefficient model

stargazer(logit1, logit2_poly_highcor, logit3_poly_highcor, logit4.2_poly_highcor, logit4.3_poly_highcor, type = "text",  single.row = TRUE,
          dep.var.caption = "Logit Systemwahl Respondent mit Polychoric Korrelationsmatrix und nur > 0.3 korrelierten Einstellungsvariablen",
          intercept.bottom = FALSE,
          dep.var.labels = "noch keine marginal effects berechnet",
          column.labels = c(
            "Hard Facts",
            "(1) + Factor Scores",
            "(2) + additonal explanatory",
            "(3) + income",
            "(4) - income"))


#### Diagnostics ####
# source: https://onlinelibrary.wiley.com/doi/epdf/10.1111/j.1540-5907.2011.00525.x

# ROC curve
prob=predict(logit4.3_poly_highcor,type=c("response"))
ROC <- cbind(KBG_work_4[which(KBG_work_4$competecases_income_new == TRUE), ], prob)
library(pROC)
g <- roc(logiteinkommen ~ prob, data = ROC) # not bad
par(pty = "s") 
str(g)

roc.df <- data.frame(
  tpp=g$sensitivities*100, ## tpp = true positive percentage
  fpp=(1 - g$specificities)*100, ## fpp = false positive precentage
  thresholds=g$thresholds)

roc.df[roc.df$tpp > 60 & roc.df$tpp < 80,]
plot(g, legacy.axes=TRUE,  percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage",
     col="#377eb8", lwd=4, print.auc=TRUE)  

# Brier Score to closer to zero the better -- not bad
BrierScore(logit4.3_poly_highcor)

library(DAMisc)
pre(logit4.3_poly_highcor)
# ePCP ur hypothetical
# model  assigns 72.3% of the probability density to the correct outcome category
library(separationplot)
# great way of check also to compare the models

a <- separationplot(logit1$fitted.values, logit1$y,
               type="rect",line=TRUE, show.expected=TRUE)
b <- separationplot(logit4.3_poly_highcor$fitted.values, logit4.3_poly_highcor$y,
               type="rect",line=TRUE, show.expected=TRUE)
