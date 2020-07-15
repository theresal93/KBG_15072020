########################################################## Models Logit - systeme binary model respondent ##########################################################
# the variables are based on the respondent only and not his/her* partner
# in this model we do not take into account that the household decision making for the system might also depent on the characteristics of the partner (this is however considered in ModelModel_Logit_respondent_effect_partner.R)

# Several logit models are estimated to check which characteristics define the likelihood to choose the income depending KBG version:
# 1. logit1: Hard Facts model
#            A model that includes the characteristics that seem to be the most reasonable onees to impact this decision. That is:
#            - F1_1: Erwerbstatus vor der Geburt
#            - S01: Geschlecht
#            - S02: Alter
#            - S07: Bildung
#            - S05: Anzahl Kinder
#            - S03: Partner*in (ja/nein)
#            income (F1_1) is not included because we would loose too many observations
# 2. logit2: Hard Facts model plus factor scores (excluding non sign. ones)
#            Now in additon factor scores (of PCA see: ) are included, the insiginificant variable S01 (Geschlecht) is excluded
# 3. logit3: Hard Facts model plus factor scores plus additional variables
#            Now in addition the follwoing variables are included:
#            Ob Entscheidung von oder nach der Gebur fiel (F2_10), wie relevant die tageweise Inaspruchnahme des Kontos war, Wochenarbeitsstunden vor Geburt,
#            ob sich der Vater am KBG beteiligt
#            Also, the impact of further variables that showed, however, no significant effect. That is:
#            S11: Einstellung Familie 
#            - F2_11: FZB
#            - beziehtPB 
#            - S09: Bundesland 
#            - S10: Einwohner Gemeinde 
# 4. logit4: Erwerbstätigen Model: Subset model to include income
#            Now in addition also income (F1_3: monatliches Nettoeinkommen vor der Geburt) was included
#            Note regarding model 4.1 : And also an additional model exuding income with the complte.case dataset of logit4 was estimated to check the effect 
#            of income
# 5. Stargazer Output
#            Summarized all the models in this script. The coefficients are average partial effects. The constant is not shown because of marginal effects
# ----------------------------------------------------------------------------------------------------------------------------------------# ----------------------------



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
# note: also dependency to R_dummies
source("Z:/KBG/02_Prepare Data in R/New_Variables_KBG.R")


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
                             F1_1 + S01 + S02 + S07 + S05 + S03 + F2_11
                           ,
                           data = KBG_work_4,
                           atmean = FALSE)


#### 2. logit2: Hard Facts model plus factor scores (excluding non sign. ones) ####

## summary of additionally considered variables
# INDEPENDENT VARIABLES
#   All factor scores of PCA (see ...)
#   Umso höher der score umso stärker, strebt diese Person in Richtung diesem Faktor
#   (Unterschied zu FAC1_1: umso niedriger der score, umso stärker strebt diese Person in Richtung diesem Faktor)

# FAC1_2: "Traditionell"
# FAC2_2: "Modern"
# FAC3_2: "Übermutter"
# FAC4_2: "Powercouple"
# FAC5_2: "versuchte partnerschaftliche Familienführung"

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

## Model logit2: logit with marginal partial effects
logit2 <- glm(
  logiteinkommen ~
    F1_1 + S01 + S02 + S07 + S05 + S03 +
    FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2 ,
  data = KBG_work_4,
  family = binomial(link = "logit")
)
logit2mfx_part <- logitmfx(
  logiteinkommen ~
    F1_1 + S02 + S07 + S05 + S03 +
    FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2
  ,
  data = KBG_work_4,
  atmean = FALSE
)
logit2mfx_part



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
# S09: Bundesland
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
logit3 <- glm(
  logiteinkommen ~
    F1_1  + S02 + S07 + S05 + S03 +
    FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2 +
    F2_10 + F3_3 + KBGm_beteiligt + S09 ,
  data = KBG_work_4,
  family = binomial(link = "logit")
)
logit3mfx_part <- logitmfx(
  logiteinkommen ~
    F1_1 + S02 + S07 + S05 + S03 +
    FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2 +
    F2_10 + F3_3 + KBGm_beteiligt + S09
  ,
  data = KBG_work_4,
  atmean = FALSE
)
logit3mfx_part


## Test use factor scores from policoric model
# Factor scores from policoric model
# source PCA and hierachical_cat_highcor and then source R_New_Variables again.

# source only parts of the script
# source2 <- function(file, start, end, ...) {
#   file.lines <-
#     scan(
#       file,
#       what = character(),
#       skip = start - 1,
#       nlines = end - start + 1,
#       sep = '\n'
#     )
#   file.lines.collapsed <- paste(file.lines, collapse = '\n')
#   source(textConnection(file.lines.collapsed), ...)
# }
# 
# source2(
#   "Z:/KBG/04_Cluster Analysis_TL/Results/PCA and hierachical_cat_and highcor.R",
#   1,
#   145
# )
# source("Z:/KBG/02_Prepare Data in R/New_Variables_KBG.R")
# 
# KBG_work_4$pca_ord_scores <- pca_ord_scores
# 
# logit3_poly <- glm(
#   logiteinkommen ~
#     F1_1 + S01 + S02 + S07 + S05 + S03 +
#     pca_ord_scores +
#     F1_2 + F2_10 + F3_3 + KBGm_beteiligt ,
#   data = KBG_work_4,
#   family = binomial(link = "logit")
# )
# logit3mfx_part_poly <- logitmfx(
#   logiteinkommen ~
#     F1_1 + S02 + S07 + S05 + S03 +
#     pca_ord_scores +
#     F1_2 + F2_10 + F3_3 + KBGm_beteiligt
#   ,
#   data = KBG_work_4,
#   atmean = FALSE
# )
# logit3mfx_part_poly

# changed results compared to logit3mfx_part
# FAC4_2 and FAC5_2 are still siginificant of 10 % significance level
logit3.1mfx_part <- logitmfx(
  logiteinkommen ~
    F1_1 + S02 + S07 + S05 + S03 +
    FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2 +
    F1_2 + F2_10 # without F3_3 because of missings
  ,
  data = KBG_work_4,
  atmean = FALSE
)
logit3.1mfx_part


#### 4. logit4: Erwerbstätigen Model: Subset model to include income  ####

## summary of additionally considered variables
# F1_3: monatliches Nettoeinkommen vor der Geburt
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#   250    1200    1600    1667    2000    8500     439
# F1_2: Wochenarbeitsstunden (viele NA's daher erst hier)
# 6    8    9   10   12   14   15   16   17   18   19   20   21   22 22.5   24   25   26   27   28 28.5   30 30.5   32 32.5   33   34   35
# 4    2    1   10    8    1   13    6    2    5    2   79    5    5    1   10   38    1    3    3    1   72    1   11    1    2    2   20
#   36   37   38 38.5   39   40   41   42 42.5 44.5   45   50   60   70 NA's
#    1    2   19   77    2  309    1    3    1    1    2   12    6    1  254

# 495
logit4_completecases <-
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
      KBG_work_4$FAC5_2,
      
      KBG_work_4$F2_10,
      KBG_work_4$F3_3,
      KBG_work_4$KBGm_beteiligt,
      
      KBG_work_4$F1_2,
      KBG_work_4$F1_3
    )
  )

## Model logit4: logit with marginal partial effects
logit4 <- glm(
  logiteinkommen ~
    F1_1  + S02 + S07 + S05 + S03 +
    FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2 +
    F1_2 + F2_10 + F3_3 + KBGm_beteiligt +
    F1_3,
  data = KBG_work_4,
  family = binomial(link = "logit")
)
logit4mfx_part <- logitmfx(
  logiteinkommen ~
    F1_1 + S02 + S07 + S05 + S03 +
    FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2 +
    F2_10 + F3_3 + KBGm_beteiligt +
    F1_2 + F1_3
  ,
  data = KBG_work_4,
  atmean = FALSE
)
logit4mfx_part

# same model but without F1_3 i.e. use only 533 observations of logit4_completecases to check whether F1_3 makes a difference
KBG_work_4$competecases_income <-
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
    KBG_work_4$F1_2,
    KBG_work_4$KBGm_beteiligt,
    
    KBG_work_4$F1_3
  )

nrow(KBG_work_4[which(KBG_work_4$competecases_income == TRUE), ])

# Income makes a differences: many other variables become less siginificant as soon as income is considered in the model
logit4.1 <- glm(
  logiteinkommen ~
    F1_1  + S02 + S07 + S05 + S03 +
    FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2 +
    F1_2 + F2_10 + F3_3 + KBGm_beteiligt 
    ,
  data = KBG_work_4[which(KBG_work_4$competecases_income == TRUE), ],
  family = binomial(link = "logit")
)
logit4.1mfx_part <- logitmfx(
  logiteinkommen ~
    F1_1 + S02 + S07 + S05 + S03 +
    FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2 +
    F1_2 + F2_10 + F3_3 +  KBGm_beteiligt
  ,
  data = KBG_work_4[which(KBG_work_4$competecases_income == TRUE), ],
  atmean = FALSE
)
logit4.1mfx_part


# use nettoeinkommen (including imputations) instead of F1_2 and F1_3
logit4.2 <- glm(
  logiteinkommen ~
    F1_1  + S02 + S07 + S05   + S03 +
    FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2 +
    F2_10 + F3_3 + KBGm_beteiligt + S09 +
    Nettostundeneinkommen
  ,
  data = KBG_work_4,
  family = binomial(link = "logit")
)
logit4.2mfx_part <- logitmfx(
  logiteinkommen ~
    F1_1 + S02  + S07 + S05  + S03 +
    FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2 +
    F2_10 + F3_3 + KBGm_beteiligt + S09 +
    Nettostundeneinkommen
  ,
  data = KBG_work_4,
  atmean = FALSE
)
logit4.2mfx_part


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


logit4.3 <- glm(
  logiteinkommen ~
    F1_1  + S02 + S07 + S05   + S03 +
    FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2 +
    F2_10 + F3_3 + KBGm_beteiligt + S09,
  data = KBG_work_4[which(KBG_work_4$competecases_income_new == TRUE),],
  family = binomial(link = "logit")
)
logit4.3mfx_part <- logitmfx(
  logiteinkommen ~
    F1_1 + S02   + S07 + S05  + S03 +
    FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2 +
    F2_10 + F3_3 + KBGm_beteiligt + S09,
  data = KBG_work_4[which(KBG_work_4$competecases_income_new == TRUE),],
  atmean = FALSE
)
logit4.3mfx_part



#### 5. Stargazer Output ####
stargazer(
  logit1,
  logit2,
  logit3,
  logit4.2,
  logit4.3,
  coef = list(
    c(logit1mfx_part$mfxest[, 1]),
    c(logit2mfx_part$mfxest[, 1]),
    c(logit3mfx_part$mfxest[, 1]),
    c(logit4.2mfx_part$mfxest[, 1]),
    c(logit4.3mfx_part$mfxest[, 1])
  ),
  se = list(
    c(logit1mfx_part$mfxest[, 2]),
    c(logit2mfx_part$mfxest[, 2]),
    c(logit3mfx_part$mfxest[, 2]),
    c(logit4.2mfx_part$mfxest[, 2]),
    c(logit4.3mfx_part$mfxest[, 2])
  ),
  type = "text",
  single.row = TRUE,
  dep.var.caption = "Logit Systemwahl Respondent",
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
