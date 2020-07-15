############################### Model Variante linear ############################



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


### In addition to Logit model the following variables might be of interest:
# F3_4: Relevanz folgender Punkte für Entscheidung Konto
#  F3_4_1 : dazuverdienen
#  F3_4_2 : Länge
#  F3_4_3 : Flexibilität
#  F3_4_4 : unsichere Lebenssituation
#  F3_4_5 : kein Anspruch

# F3_1_8 : Wenn wir die passende Kinderbetreuung gehabt hätte, hätten wir eine kürzere KBG-Variante gewählt


#### 1. lm1: Hard facts model ####

## summary of considered variables

# DEPENDENT continous VARIABLE
# f2_4kor: persönlicher KBG-Bezug in Monaten
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#    2,00   16,00   24,00   20,84   24,00   28,00      20

# INDEPENDENT VARIABLES
# F1_1: Erwerbsstatus vor Geburt (unselbsttaendig base)
# unselbststaending   selbststaending        studierend            karenz          haushalt        arbeitslos
#               332                39                16               100                35                64
# S01: Geschlecht 1 weiblich (base) 2 m?nnlich
# weiblich männlich NA's
#      550       36    0
# S02: Alter
#  NA's
#    0
# S07: Bildungslevel (studium base)
# studium  pflichtschule         lehre    fachschule        matura          NA's
#     185            37           152            81           126             5
# S05: Anzahl Kinder
#  NA's
#   0
# S03: lebt mit Partner_in zusammen (ja base)
# ja  nein NA's
# 516   70 0

## 1.1 model
lm1 <- lm(
  f2_4kor ~ F1_1 + S01 + S02 + S07 + S05 + S03,
  data = KBG_work_2020.konto
)
summary(lm1)

#### 2. lm2: Hard Facts model plus factor scores (excluding non sign. ones) ####

# exclude insignificant ones (i.e. S02, S05, S03) and add factors
## 2.1 model
lm2 <- lm(
  f2_4kor ~ F1_1 + S01  + S07 +
    FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor,
  data = KBG_work_2020.konto
)
summary(lm2)

#### 3. lm3:  Hard Facts model plus factor scores plus additional variables ####

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


## 3.1 model
lm3 <- lm(
  f2_4kor ~ F1_1 + S01  + S07 +
    FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
    KBGm_beteiligt,
  data = KBG_work_2020.konto, na.action = na.omit
)
summary(lm3) ### sobald man KBGm_beteiligt hinzufügt, wird Faktor 4 signifikant, F2_10 + F3_3 sind nicht signi, as_factor(F3_1_8) nicht signi

# KBGm_beteiligt and FAC4_poly_highcor are not correlated
cor(KBG_work_2020.konto[which(!is.na(KBG_work_2020.konto$KBGm_beteiligt)), "KBGm_beteiligt"],
    KBG_work_2020.konto[which(!is.na(KBG_work_2020.konto$KBGm_beteiligt)), "FAC4_poly_highcor"])


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
lm4 <- lm(
  f2_4kor ~ F1_1 + S01  + S07 +
    FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
    KBGm_beteiligt+ F3_4_2  + F3_4_5 , # F3_4_1 , 3 and 4 are not signi
  data = KBG_work_2020.konto
)
summary(lm4)


#### 5. lm5: Hard Facts model plus factor scores plus additional variables plus F3_4 plus hourly net wage ####

lm5 <- lm(
  f2_4kor ~ F1_1 + S01  + S07 +
    FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
    KBGm_beteiligt+ F3_4_2  + F3_4_5 +
    Nettostundeneinkommen, # Nettostundeneinkommen not significant
  data = KBG_work_2020.konto
)
summary(lm5)

#### 6. lm6: Hard Facts model plus factor scores plus additional variables plus F3_4 plus Bundesland ####
lm6 <- lm(
  f2_4kor ~ F1_1 + S01  + S07 +
    FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
    KBGm_beteiligt+ F3_4_2  + F3_4_5 +
    S09 , 
  data = KBG_work_2020.konto
)
summary(lm6)


#### 7. Stargazer output ####
setwd("Z:/KBG/05_Models in R")
stargazer(lm1, lm2, lm3, lm4, lm5, lm6, type = "html",  single.row = TRUE,
          dep.var.caption = "lm Länge KBG (in Monaten) Respondent mit Polychoric Korrelationsmatrix und nur > 0.3 korrelierten Einstellungsvariablen",
          intercept.bottom = FALSE,
          dep.var.labels = "",
          column.labels = c(
            "Hard Facts",
            "(1) + Factor Scores",
            "(2) + add. explanatory",
            "(3) + F3.4",
            "(4) + income",
            "(4) + Bundesland"),
          out ="Z:/KBG/05_Models in R/Lm_Results_Polychoric.htm")
          
#### 8. Visualize ####

# sources
# https://cran.r-project.org/web/packages/ggiraphExtra/vignettes/ggPredict.html
# https://rpubs.com/aaronsc32/regression-confidence-prediction-intervals

# FAC2_range <- seq(from=min(KBG_work_4$FAC2_poly_highcor), to=max(KBG_work_4$FAC2_poly_highcor), by=.01)
FAC1_range <- seq(from=min(KBG_work_2020.konto$FAC1_poly_highcor), to=max(KBG_work_2020.konto$FAC1_poly_highcor), by=.01)

status <- c("unselbststaending", "selbststaending", "arbeitslos")

for( i in 1:length(status)){
setup_interval <- data.frame(F1_1 = rep(status[i], length(FAC1_range)),
                               S01 = "weiblich",
                               S02 = rep(mean(KBG_work_4$S02), 1),
                               S07 = rep("studium", 1),
                               S05= rep(mean(KBG_work_4$S05), 1),
                               FAC1_poly_highcor = FAC1_range ,
                               FAC2_poly_highcor = rep(mean(KBG_work_4$FAC1_poly_highcor), length(FAC1_range)),
                               FAC3_poly_highcor = rep(mean(KBG_work_4$FAC3_poly_highcor), length(FAC1_range)),
                               FAC4_poly_highcor = rep(mean(KBG_work_4$FAC4_poly_highcor), length(FAC1_range)),
                               KBGm_beteiligt = rep(0, length(FAC1_range), 1))

  pp <- predict(lm2, newdata = setup_interval,  interval="confidence") # passt, koef mit händischer Berechnung verglichen
  colnames(pp) <- c("pred", "lwr", "upr")
  pp <- as.data.frame(pp)
  assign(paste0('pframe_', status[i]), pp)
  }



plot.data <- data.frame(a=pframe_unselbststaending$pred, b=pframe_selbststaending$pred, c=pframe_arbeitslos$pred, FAC1=FAC1_range)
plot.data <- gather(plot.data, key=group, value=value, a:c)

nrow(pframe_unselbststaending) + nrow(pframe_selbststaending) + nrow(pframe_arbeitslos)
nrow(plot.data)
plot.data <- rbind(
  cbind(plot.data[which(plot.data$group=="a"),], pframe_unselbststaending[, 2:ncol(pframe_unselbststaending)]),
  cbind(plot.data[which(plot.data$group=="b"),], pframe_selbststaending[, 2:ncol(pframe_selbststaending)]),
  cbind(plot.data[which(plot.data$group=="c"),], pframe_arbeitslos[, 2:ncol(pframe_arbeitslos)]))

#test
plot.data[which(plot.data$prob!=plot.data$pred),]
plot.data$prob <- NULL

# subset only a:c
# plot.data <- plot.data[which(plot.data$group=='c'),]

setwd("Z:/KBG/05_Models in R/Results/Graphs")
options(OutDec= ",") #Auchtung, global option --> wieder ?ndern!
#options(digits = 0)
ggplot(plot.data, aes(x=FAC1, y=value, color=group)) + # asking it to set the color by the variable "group" is what makes it draw three different lines
  geom_line(aes(linetype=geo), linetype='solid', size=1.5) + 
  labs(x="Faktor 1", y="", title="Anzahl der Bezugsmonate im KBG-Konto nach Erwerbsstatus") +
  scale_color_manual( labels = c("Unselbstständige", "Selbstständige", "Arbeitslose"), values = c("#6975A6", "#F3E96B","#F05837")) +
  #scale_y_continuous(breaks = seq(0.0, 1.0, 0.2), limits = c(0.0,1)) +
  # scale_x_continuous(breaks = seq(-3, 3, 1), limits = c(-3, 3)) +
  #scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0, size = 11),
        #plot.subtitle = element_text(hjust=0, size=12),
        #plot.caption = element_text(hjust=1,size = 7),
        #axis.title.x = element_text(size=12, margin = margin(12,0,0,0)),
        #axis.title.y = element_text(size=12, margin = margin(0,12,0,0)),
        #axis.text = element_text(size=10),
        legend.title = element_blank(),
        legend.text = element_text(size=9),
        legend.key.size = unit(1, "cm"),
        legend.position = "right") +
  geom_ribbon(data=plot.data,aes(ymin=lwr,ymax=upr),alpha=0.3) 
# +
#   ggsave(file="p(eaKBG)_Faktor1_Unselbsstaendige.svg", 
#          width=9, height=5)
options(OutDec= ".") #Auchtung, global option --> wieder ?ndern!


#### 9. Diagnostics ####
# http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/

plot(lm6)


# Residuals vs Fitted. Used to check the linear relationship assumptions. A horizontal line, without distinct patterns is an indication for a linear relationship, what is good.
# 
# Normal Q-Q. Used to examine whether the residuals are normally distributed. It’s good if residuals points follow the straight dashed line.
# 
# Scale-Location (or Spread-Location). Used to check the homogeneity of variance of the residuals (homoscedasticity). Horizontal line with equally spread points is a good indication of homoscedasticity. This is not the case in our example, where we have a heteroscedasticity problem.
# 
# Residuals vs Leverage. Used to identify influential cases, that is extreme values that might influence the regression results when included or excluded from the analysis. This plot will be described further in the next sections.

# Relationship is almost linear
# E(residuals) is approxemitely 0
# little bit heteroscedasticity
# no bigh outliars (not > 3 standard deviations)

# # Test if heteroskedasticity.
# Beusch Pagan-test
bptest(lm6)

#### 10. heteroskedastic robust standard errors ####
# source
# https://datamotus.com/2019/10/30/Weighted-Least-Squares.html
# https://rpubs.com/cyobero/187387

# Load libraries
library("lmtest")
library("sandwich")

# Wenn man sehr wenig  ̈uber die Ursachen der Heteroskedastizit ̈at weiß empfiehl es sich h ̈aufig f ̈ur die Koeffizientensch ̈atzung bei der OLS Methode zu bleiben, da die Koeffizienten mit OLS bekanntlich erwartungstreu (aber nicht effizient) gesch ̈atztwerden, aber anstelle der OLS Standardfehler robuste Standardfehler zu berechnen,wie sie im vorhergehenden Abschnitt beschrieben wurden.

## Robust t test
# H3 verwenden für lineare Modelle
rolm6 <- coeftest(lm6, vcov = vcovHC(lm6, type = "HC3")) # robust version of stata

## stargazer

stargazer(rlm6, type = "text",  single.row = TRUE,
          dep.var.caption = "Modell 6 mit robusten Standardfehlern",
          intercept.bottom = FALSE,
          dep.var.labels = "",
          column.labels = c(
            "(6) robust"))


stargazer(rlm6)



## test with glm
# 
# KBG_work_2020.konto$lm3_completecases <-
#   (
#     complete.cases(
#       KBG_work_2020.konto$f2_4kor,
#       KBG_work_2020.konto$F1_1,
#       KBG_work_2020.konto$S01,
#       KBG_work_2020.konto$S07,
# 
#       KBG_work_2020.konto$FAC1_poly_highcor,
#       KBG_work_2020.konto$FAC2_poly_highcor,
#       KBG_work_2020.konto$FAC3_poly_highcor,
#       KBG_work_2020.konto$FAC4_poly_highcor,
#       KBG_work_2020.konto$KBGm_beteiligt
#     )
#   )
# 
# weights <- matrix(NA, nrow(KBG_work_2020.konto),1)
# KBG_work_2020.konto <- cbind(KBG_work_2020.konto, weights)
# KBG_work_2020.konto[which(KBG_work_2020.konto$lm3_completecases==TRUE), "weights"] <- 1/fitted(lm3)
# 
# wlm3 <- lm(f2_4kor ~ F1_1 + S01  + S07 +
#              FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
#              KBGm_beteiligt,
#            data = KBG_work_2020.konto,
#            weights = weights, na.action = na.omit)
# library(nlme)
# glm3 <- gls(f2_4kor ~ F1_1 + S01  + S07 +
#       FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
#       KBGm_beteiligt,
#     data = KBG_work_2020.konto, na.action = na.omit, weights=varPower())
# 
# glm6 <- gls(f2_4kor ~ F1_1 + S01  + S07 +
#   FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
#   KBGm_beteiligt+ F3_4_2  + F3_4_5 +
#   S09 , na.action = na.omit,
# data = KBG_work_2020.konto, weights=varPower())
# 
# plot(wlm3)
# plot(glm6)
# plot(lm3)
# bptest(lm6)
# bptest(lm6, ~ KBG_work_2020.konto$F1_1 + KBG_work_2020.konto$S01  + KBG_work_2020.konto$S07 +
#          KBG_work_2020.konto$FAC1_poly_highcor + KBG_work_2020.konto$FAC2_poly_highcor + KBG_work_2020.konto$FAC3_poly_highcor + KBG_work_2020.konto$FAC4_poly_highcor +
#          KBG_work_2020.konto$KBGm_beteiligt+ KBG_work_2020.konto$F3_4_2  + KBG_work_2020.konto$F3_4_5 +
#          KBG_work_2020.konto$S09 )
# 




