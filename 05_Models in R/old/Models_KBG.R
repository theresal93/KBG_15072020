#### KBG multinominal logit model ####
setwd("Z:/KBG")


# multinom function from the nnet package 

#### packages ####
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

#### dependent variable variante ####

#zuvor von R_dummies laden
#KBG_work_3 <- read_sav("Y:/common/Projekte/2 Laufende Projekte/Kinderbetreuungsgeldkonto - Evaluierung ab 2018/Quant_BezieherInnen_2019/Daten/KBG_work_3.sav")

source("Z:/KBG/02_Prepare Data in R/R_dummies_KBG.R")

KBG_work_3$gesamtdauerneu <- ifelse(is.na(KBG_work_3$gesamtdauer)==TRUE,
                                    KBG_work_3$f2.4kor,
                                    KBG_work_3$gesamtdauer)

KBG_work_3$system <- ifelse(KBG_work_3$gesamtdauerneu<13 & KBG_work_3$F2.2==1, 1,
                       ifelse(KBG_work_3$gesamtdauerneu>=13 & KBG_work_3$gesamtdauerneu<25 & KBG_work_3$F2.2==1, 2,
                       ifelse(KBG_work_3$gesamtdauerneu>=25 & KBG_work_3$F2.2==1, 3,
                       ifelse(KBG_work_3$F2.2==2 &  is.na(KBG_work_3$gesamtdauerneu)==FALSE, 4, NA))))

KBG_work_3$system <- factor(KBG_work_3$system,labels = c("maximal 1 Jahr", "maximal 2 Jahre", "?ber 2 Jahre",
                                         "Einkommensab?ngig"))

KBG_work_3$variante <- ifelse(KBG_work_3$gesamtdauerneu<13 & KBG_work_3$F2.2==1, 1,
                       ifelse(KBG_work_3$gesamtdauerneu>=13 & KBG_work_3$gesamtdauerneu<25 & KBG_work_3$F2.2==1, 2,
                       ifelse(KBG_work_3$gesamtdauerneu>=25 & KBG_work_3$F2.2==1, 3, NA)))


KBG_work_3$variante <- factor(KBG_work_3$variante,labels = c("maximal 1 Jahr", "maximal 2 Jahre", "?ber 2 Jahre"))

##### additional variables ####

#standard Variante (gew?hnlicher Bezug und individuell festgelegter Bezug)
# Standard: 12 Monate, 24 Monate (Dauer Karenz), 28 Monate (einzeln)
# individueller Bezug ist dazwischen

test<- KBG_work_3$gesamtdauerneu
test[is.na(test)] <- 99

test <- ifelse(!(test %in% c(12, 24,28,99)) , 1,
               ifelse(test %in% c(99), NA, 2))

test<- factor(test, labels = c("nicht standard","standard")) 
summary(test)

KBG_work_3$standard <- test



### Wiedereinstieg ###

KBG_work_3$wiedereinstieg <- rep(NA, nrow(KBG_work_3))
KBG_work_3$wiedereinstieg[which(KBG_work_3$F4.1=="unselbststaending" | KBG_work_3$F4.1=="selbststaending")] <- KBG_work_3$F4.3[which(KBG_work_3$F4.1=="unselbststaending" | KBG_work_3$F4.1=="selbststaending")]
  
KBG_work_3$wiedereinstieg[which(KBG_work_3$F4.1  %in% c("studierend","karenz", "haushalt", "arbeitslos") &  KBG_work_3$F4.4==1)] <- KBG_work_3$F4.5[which(KBG_work_3$F4.1 %in% c("studierend","karenz", "haushalt", "arbeitslos") &  KBG_work_3$F4.4==1)]

summary(KBG_work_3$wiedereinstieg)


#### externe Kinderbetreuung ####
#F5.1- Wird Ihr j?ngstes Kind regelm??ig von Tageseltern, in einer Kinderkrippe oder in einem Kindergarten betreut?
#F5.2- Wie alt war Ihr j?ngstes Kind, als Sie diese Betreuung erstmals in Anspruch genommen haben? (Alter des Kindes in Monaten:)
#F5.3- Ab welchem Alter m?chten Sie f?r Ihr j?ngstes Kind die Betreuung bei Tageseltern, in einer Kinderkrippe oder im Kindergarten in Anspruch nehmen? (Alter des Kindes in Monaten:)

KBG_work_3$betreuungextern <- rep(NA, nrow(KBG_work_3))
KBG_work_3$betreuungextern[which(KBG_work_3$F5.1== 1)] <- KBG_work_3$F5.2[which(KBG_work_3$F5.1==1)] #57x99
KBG_work_3$betreuungextern[which(KBG_work_3$F5.1== 2)] <- KBG_work_3$F5.3[which(KBG_work_3$F5.1==2)] #54x99
KBG_work_3$betreuungextern[which(KBG_work_3$betreuungextern== 99)] <- NA

summary(KBG_work_3$betreuungextern)
summary(as.factor(KBG_work_3$F5.3))
summary(as.factor(KBG_work_3$F5.2))



#### umbenennen aller Variablen von . zu _ ####

#You may need to escape the . (with \\) which is a special character that means "any character"
require(stringr)
colnames(KBG_work_3) <- str_replace(colnames(KBG_work_3), "\\.", "_")

# 
# write_sav(KBG_work_3, "KBG_work_4_R.sav")
# save.dta13(KBG_work_3, "KBG_work_4_R.dta", data.label = NULL, time.stamp = TRUE,
#            convert.factors = TRUE, convert.dates = TRUE, tz = "GMT",
#            add.rownames = FALSE, compress = FALSE, version = 117,
#            convert.underscore = FALSE)

############### MODELS ###############

#### logit ####

## zwei Varianten: Einkommensabh?ngig= 1
##                 Konto=1

KBG_work_3$logiteinkommen <- ifelse(KBG_work_3$F2_2==1, 0,
                             ifelse(KBG_work_3$F2_2==2, 1, NA))

KBG_work_3$logiteinkommen <- factor(KBG_work_3$logiteinkommen,labels = c("Konto", "Einkommensabh?ngig"))


logit1<- glm(logiteinkommen ~ FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2, family = binomial(link = "logit"), data = KBG_work_3)
summary(logit1)

library(mfx)
# average marginal effect
logit1mfx<- logitmfx(logiteinkommen ~ FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2,  data = KBG_work_3, atmean = TRUE)

# average partial effects
logit1mfx<- logitmfx(logiteinkommen ~ FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2,  data = KBG_work_3, atmean = FALSE)



#### multinominal logit ####
## Variablen mlogit1:
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

# relevel base category of dependent variable

KBG_work_3$system <- relevel(KBG_work_3$system, ref = "Einkommensab?ngig")



mlogit1 <- multinom(system ~  S01 + F1_1 + S07+S02+ S05 + S08 + FAC1_2 + FAC2_2 + FAC3_2 + FAC4_2 + FAC5_2 , data=KBG_work_3)
stargazer(mlogit1, type="text", single.row = TRUE)

#### predicted probabilites ####
## for each school level (5 levels)

allmean <- data.frame(F1.1 = c("unselbststaending","selbststaending", "studierend", "karenz", "haushalt", "arbeitslos"),
                      S07 = c("pflichtschule","lehre", "fachschule", "matura", "studium"),
                      S02=rep(mean(KBG_work_3$S02),30),
                      S05=rep(mean(KBG_work_3$S05), 30))

allmean[, c("pred.prob")] <- predict(mlogit1, newdata = allmean, type="probs")
allmean


## Variablen mlogit2:
# F1.1: Erwerbsstatus vor Geburt
# S02: Alter
# S07: Bildungslevel
# S05: Anzahl Kinder
# S01: Geschlecht 
# beziehtPB
# F2.10: wann Entscheidung
# F2.11: FZB
# F3.3: Relevanz tageweise
# S09: Bundesland
# S10: Einwohner Gemeinde
# S11: Einstellung Familie
# S03: lebt mit Partner_in zusammen

KBG_work_3 <- within(KBG_work_3, S03 <- relevel(S03,  ref = "nein"))

mlogit2 <- multinom(system ~  F1.1 + S07 + S02 + S01 + S05 + beziehtPB + F2.10 +F2.11 +
                      + F3.3 + S09 + S10 +S11 + S03 , data=KBG_work_3)
stargazer(mlogit2, type="text", single.row = TRUE)


## Variablen mlogit3:
# F1.1: Erwerbsstatus vor Geburt
# S02: Alter
# S07: Bildungslevel
# S05: Anzahl Kinder
# S01: Geschlecht 
# beziehtPB
# F2.10: wann Entscheidung
# F2.11: FZB
# F3.3: Relevanz tageweise
# S09: Bundesland
# S10: Einwohner Gemeinde
# S11: Einstellung Familie
# F1.3: Netto-EK vor Geburt (nur unselbstst?ndig und selbst?ndig erwerbst?tige)
# F1.2: Anzahl der Stunden vor Geburt 
# F2.14: L?nge Karenz
# F3.2a1, F3.2a2_genutzt,  F3.2b1, F3.2b2_genutzt: Bekanntheit und Nutzung Info-Angebot
# S03: lebt mit Partner_in zusammen

#  F1.2 + F1.3 fallen alle nicht erwerbst?tigen weg

KBG_work_3 <- within(KBG_work_3, F3.2a1 <- relevel(F3.2a1,  ref = "unbekannt"))
KBG_work_3 <- within(KBG_work_3, F3.2b1 <- relevel(F3.2b1,  ref = "unbekannt"))

mlogit3 <- multinom(system ~  S01 + F1.1 + S07+S02+ S05 + beziehtPB + F2.10 +F2.11 + F2.14 + F3.2a1 + F3.2a2_genutzt + F3.2b1 + F3.2b2_genutzt + F3.3 + S09 + S10 +S11 + F1.2 + F1.3 + S03, data=KBG_work_3)
stargazer(mlogit3, type="text", single.row = TRUE)

### verwendete Beobachtungen
# length(residuals(mlogit3))/4
# 
# mf <- model.frame(variante ~  S01 + F1.1 + S07+S02+ S05 + beziehtPB + F2.10 +F2.11 + F2.14 + F3.2a1 + F3.2a2_genutzt + F3.2b1 + F3.2b2_genutzt + F3.3 + S09 + S10 +S11 + F1.2 + F1.3 + F1.2, data=KBG_work_3,
#                   na.action=na.omit)
# lapply(mf,table)
# 
# library(Hmisc)
# describe(variante ~  S01 + F1.1 + S07+S02+ S05 + beziehtPB + F2.10 +F2.11 + F2.14 + F3.2a1 + F3.2a2_genutzt + F3.2b1 + F3.2b2_genutzt + F3.3 + S09 + S10 +S11 + F1.2 + F1.3 + F1.2, data=KBG_work_3)

### Informationskriterien
PseudoR2(c(mlogit1, mlogit2, mlogit3), which = "all")


#### standard OLS ####
OLS1 <- lm(gesamtdauerneu ~ S01 + F1.1 + S07+S02+ S05  + F2.10 +F2.11 + F2.14 + F3.2a1*F3.2a2_genutzt + F3.2b1*F3.2b2_genutzt
                    + F3.3 + S09 + S10 +S11 + F1.2 + F1.3 , data=KBG_work_3)

summary(OLS1)



#### ordered logit ####

model3 <- polr(variante ~ S01_weiblich +
                  F1.1_unselbstaendig + F1.1_selbstaendig + F1.1_studierend + F1.1_karenz + F1.1_haushalt +
                  S07_pflichtschule + S07_lehre + S07_fachschule + S07_matura+
                  S02 + S04 + S05 +
                  S09_bgl + S09_ktn + S09_noe + S09_ooe + S09_stmk + S09_tirol + S09_vbg + S09_wien
                
                , data=KBG_work_3, Hess=TRUE)

coeftest(model3)
