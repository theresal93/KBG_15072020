######################### ordered logit ########################

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
KBG_work_2020 <- KBG_work_4

# subset for people who chose Konto
KBG_work_2020.konto <-
  KBG_work_2020[which(KBG_work_2020$logiteinkommen == 'Konto'),]
nrow(KBG_work_2020.konto) # 586 Personen

# plot(KBG_work_2020.konto$f2_4kor, ylim=c(0,30), yaxt="n", xlab="Häufikeit")
# hist(factor(KBG_work_2020.konto$f2_4kor), ylim=c(0,200), col = "#6975A6", xlab= "Monate des Bezugs", ylab="Häufigkeit",
#      main="Verteilung der Bezugsmonate", xaxt="n")
# xtick<-c(0, 12, 18, 24, 28)
# axis(side=1, at=hist$mids, labels = seq(1,27,2))
# text(x=xtick, par("usr")[3], 
#      labels = xtick, srt = 0, pos = 2, xpd = TRUE)

#### 1. ord1: Hard facts model ####
ord1 <- polr(variante ~ 
                 F1_1 + S01 + S02 + S07 + S05 + S03 ,
                 data=KBG_work_2020.konto, Hess=TRUE)

coeftest(ord1)

# marginal effects
library("oglmx")
ord1mfx<-oglmx(variante ~ 
                       F1_1 + S01 + S02 + S07 + S05 + S03 ,
                       data=KBG_work_2020.konto, link="logit",    
                       robust=FALSE,
                       constantMEAN = FALSE, constantSD = FALSE,
                       delta=0,threshparam = NULL)

margins.oglmx(ord1mfx,ascontinuous = FALSE, atmeans=TRUE, AME=FALSE)

#### 2. ord2: Hard Facts model plus factor scores (excluding non sign. ones) ####
ord2 <- polr(variante ~ 
               F1_1 + S01  + S07 +
               FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor ,
             data=KBG_work_2020.konto, Hess=TRUE)

coeftest(ord2)

#### 3. ord3:  Hard Facts model plus factor scores plus additional variables ####

# KBGm_beteiligt: ob sich Mann an KBG beteiligt oder nicht
#    0    1 NA's 
#  418   98   70 

# NO EFFECT
# F2_10: wann Entscheidung
# F3_3: Relevanz tageweise

# S11: Einstellung Familie
# F2_11: FZB (auch wenn auf 2 levels aufgeteilt, kein Effekt)
# beziehtPB
# S10: Einwohner Gemeinde
# F3_1_8: Wenn wir die passende Kinderbetreuung gehabt hätte, hätten wir eine kürzere KBG-Variante gewählt

KBG_work_2020.konto$F2_11 <- factor(ifelse(KBG_work_2020.konto$F2_11=='beziehtFZB', "ja",
                                           ifelse(KBG_work_2020.konto$F2_11 %in% c("kenntFZBnicht", "andererGrund"), "nein", NA)))

ord3 <- polr(
  variante ~ F1_1 + S01  + S07 +
    FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
    KBGm_beteiligt,
  data = KBG_work_2020.konto, na.action = na.omit
)

coeftest(ord3)


#### 4. lm4:  Hard Facts model plus factor scores plus additional variables plus F3_4 ####

# F3_4: Relevanz folgender Punkte für Entscheidung Konto
#  F3_4_1 : dazuverdienen
#  F3_4_2 : Länge
#  F3_4_3 : Flexibilität
#  F3_4_4 : unsichere Lebenssituation
#  F3_4_5 : kein Anspruch eaKBG


## Eventually only F3_4_2 and F3_4_5 were added because 3 and 4 are not significant

# F3_4_2 : Länge
#     nicht relevant       relevant   NA's
#                213            373      0
# F3_4_5 : kein Anspruch eaKBG
#     nicht relevant       relevant   NA's
#                343            243      0


## 4.1 recode only two factors for F3_4
KBG_work_2020.konto$F3_4_1 <-
  factor(ifelse(
    as_factor(KBG_work_2020.konto$F3_4_1) %in% c("Sehr relevant", "eher relevant"),
    "relevant",
    ifelse(
      as_factor(KBG_work_2020.konto$F3_4_1) %in% c("eher nicht relevant", "gar nicht relevant"),
      "nicht relevant",
      NA
    )
  ))

KBG_work_2020.konto$F3_4_2 <-
  factor(ifelse(
    as_factor(KBG_work_2020.konto$F3_4_2) %in% c("Sehr relevant", "eher relevant"),
    "relevant",
    ifelse(
      as_factor(KBG_work_2020.konto$F3_4_2) %in% c("eher nicht relevant", "gar nicht relevant"),
      "nicht relevant",
      NA
    )
  ))


KBG_work_2020.konto$F3_4_3 <-
  factor(ifelse(
    as_factor(KBG_work_2020.konto$F3_4_3) %in% c("Sehr relevant", "eher relevant"),
    "relevant",
    ifelse(
      as_factor(KBG_work_2020.konto$F3_4_3) %in% c("eher nicht relevant", "gar nicht relevant"),
      "nicht relevant",
      NA
    )
  ))


KBG_work_2020.konto$F3_4_4 <-
  factor(ifelse(
    as_factor(KBG_work_2020.konto$F3_4_4) %in% c("Sehr relevant", "eher relevant"),
    "relevant",
    ifelse(
      as_factor(KBG_work_2020.konto$F3_4_4) %in% c("eher nicht relevant", "gar nicht relevant"),
      "nicht relevant",
      NA
    )
  ))


KBG_work_2020.konto$F3_4_5 <-
  factor(ifelse(
    as_factor(KBG_work_2020.konto$F3_4_5) %in% c("Sehr relevant", "eher relevant"),
    "relevant",
    ifelse(
      as_factor(KBG_work_2020.konto$F3_4_5) %in% c("eher nicht relevant", "gar nicht relevant"),
      "nicht relevant",
      NA
    )
  ))


## 4.2 model
ord4 <- polr(
  variante ~ F1_1 + S01  + S07 +
    FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
    KBGm_beteiligt+ F3_4_2  + F3_4_5 , # F3_4_1 , 3 and 4 are not signi
  data = KBG_work_2020.konto
)
coeftest(ord4)


#### 5. lm5: Hard Facts model plus factor scores plus additional variables plus F3_4 plus hourly net wage ####

ord5 <- polr(
  variante ~ F1_1 + S01  + S07 +
    FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
    KBGm_beteiligt+ F3_4_2  + F3_4_5 +
    Nettostundeneinkommen, # Nettostundeneinkommen not significant
  data = KBG_work_2020.konto
)
coeftest(ord5)


#### 6. lm6: Hard Facts model plus factor scores plus additional variables plus F3_4 plus Bundesland ####
KBG_work_2020.konto$ord6_completecases <- complete.cases(KBG_work_2020.konto$variante,
               KBG_work_2020.konto$F1_1,
               KBG_work_2020.konto$S01,
               KBG_work_2020.konto$S07,
               KBG_work_2020.konto$KBGm_beteiligt,
               KBG_work_2020.konto$F3_4_2,
               KBG_work_2020.konto$F3_4_5,
               KBG_work_2020.konto$S09)


ord6 <- polr(
  variante ~ F1_1 + S01  + S07 +
    FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
    KBGm_beteiligt+ F3_4_2 + F3_4_5 +
    S09 , 
  data = KBG_work_2020.konto
)
coeftest(ord6)

ord6mfx<-oglmx(variante ~ 
                 F1_1 + S01  + S07 +
                 FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
                 KBGm_beteiligt+ F3_4_2 + F3_4_5 +
                 S09 , link="logit",    
               robust=FALSE,
               constantMEAN = FALSE, constantSD = FALSE,
               delta=0,threshparam = NULL, data = KBG_work_2020.konto)

margins.oglmx(ord6mfx,ascontinuous = FALSE, atmeans=TRUE, AME=FALSE)


#### 7. Informationskriterien ####

PseudoR2(ord6, which = "all")

#### 8. Stargazer Output ####

setwd("Z:/KBG/05_Models in R")
stargazer(ord1, ord2, ord3, ord4, ord5, ord6, type = "text",  single.row = TRUE,
          dep.var.caption = "ordered logit Länge KBG Respondent mit Polychoric Korrelationsmatrix und nur > 0.3 korrelierten Einstellungsvariablen",
          intercept.bottom = FALSE,
          dep.var.labels = "noch keine marginal effects berechnet",
          column.labels = c(
            "Hard Facts",
            "(1) + Factor Scores",
            "(2) + add. explanatory",
            "(3) + F3.4",
            "(4) + income",
            "(4) + Bundesland"),
          out ="Z:/KBG/05_Models in R/Ordered_Results_Polychoric.htm")
          
#### 9. Diagnostics ####
# source:
# https://stats.idre.ucla.edu/stata/dae/ordered-logistic-regression/
# Note that: Doing diagnostics for non-linear models is difficult, and ordered logit/probit models are even more difficult than binary models.

# Obtain the SBS/probability-scale residuals
library(sure)
library(PResiduals)
pres <- presid(ord6)
# ggplot for residuals
p2 <- ggplot(data.frame(y = pres), aes(sample = y)) +stat_qq(distribution = qunif, dparams = list(min = -1, max = 1), alpha = 0.5) +xlab("Sample quantile") +ylab("Theoretical quantile")
