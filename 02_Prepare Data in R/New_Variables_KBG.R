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

source("Z:/KBG/02_Prepare Data in R/R_dummies_KBG.R", echo = TRUE)
#zuvor von R_dummies laden
#KBG_work_3 <- read_sav("Y:/common/Projekte/2 Laufende Projekte/Kinderbetreuungsgeldkonto - Evaluierung ab 2018/Quant_BezieherInnen_2019/Daten/KBG_work_3.sav")



KBG_work_3$gesamtdauerneu <- ifelse(is.na(KBG_work_3$gesamtdauer)==TRUE,
                                    KBG_work_3$f2.4kor,
                                    KBG_work_3$gesamtdauer)

KBG_work_3$system <- ifelse(KBG_work_3$gesamtdauerneu<13 & KBG_work_3$F2.2==1, 1,
                       ifelse(KBG_work_3$gesamtdauerneu>=13 & KBG_work_3$gesamtdauerneu<25 & KBG_work_3$F2.2==1, 2,
                       ifelse(KBG_work_3$gesamtdauerneu>=25 & KBG_work_3$F2.2==1, 3,
                       ifelse(KBG_work_3$F2.2==2 &  is.na(KBG_work_3$gesamtdauerneu)==FALSE, 4, NA))))

KBG_work_3$system <- factor(KBG_work_3$system,labels = c("maximal 1 Jahr", "maximal 2 Jahre", "ueber 2 Jahre",
                                         "Einkommensabaengig"))

KBG_work_3$variante <- ifelse(KBG_work_3$gesamtdauerneu<13 & KBG_work_3$F2.2==1, 1,
                       ifelse(KBG_work_3$gesamtdauerneu>=13 & KBG_work_3$gesamtdauerneu<25 & KBG_work_3$F2.2==1, 2,
                       ifelse(KBG_work_3$gesamtdauerneu>=25 & KBG_work_3$F2.2==1, 3, NA)))


KBG_work_3$variante <- factor(KBG_work_3$variante,labels = c("maximal 1 Jahr", "maximal 2 Jahre", "ueber 2 Jahre"))

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
# aktuell Erwerbstätige, wann sie wiedereingestiegen sind
KBG_work_3$wiedereinstieg[which(KBG_work_3$F4.1=="unselbststaending" | KBG_work_3$F4.1=="selbststaending")] <- KBG_work_3$F4.3[which(KBG_work_3$F4.1=="unselbststaending" | KBG_work_3$F4.1=="selbststaending")]
# aktuell nicht Erwerbstätige, von denen, die vorhaben wiedereinzusteigen, wann haben sie das vor  
KBG_work_3$wiedereinstieg[which(KBG_work_3$F4.1  %in% c("studierend","karenz", "haushalt", "arbeitslos") &  KBG_work_3$F4.4==1)] <- KBG_work_3$F4.5[which(KBG_work_3$F4.1 %in% c("studierend","karenz", "haushalt", "arbeitslos") &  KBG_work_3$F4.4==1)]

summary(KBG_work_3$wiedereinstieg)


#### externe Kinderbetreuung ####
#F5.1- Wird Ihr j?ngstes Kind regelm??ig von Tageseltern, in einer Kinderkrippe oder in einem Kindergarten betreut?
#F5.2- Wie alt war Ihr j?ngstes Kind, als Sie diese Betreuung erstmals in Anspruch genommen haben? (Alter des Kindes in Monaten:)
#F5.3- Ab welchem Alter m?chten Sie f?r Ihr j?ngstes Kind die Betreuung bei Tageseltern, in einer Kinderkrippe oder im Kindergarten in Anspruch nehmen? (Alter des Kindes in Monaten:)

KBG_work_3$betreuungextern <- rep(NA, nrow(KBG_work_3))
# aktuell betreut extern, seit wann
KBG_work_3$betreuungextern[which(KBG_work_3$F5.1== 1)] <- KBG_work_3$F5.2[which(KBG_work_3$F5.1==1)] #57x99
# aktuell nicht betreuet extern haben dies aber vor, wann
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

KBG_work_4 <- KBG_work_3


#### Haushaltseinkommen ####
summary(KBG_work_4$S03)


summary(KBG_work_4$F1_3)
summary(as_factor(KBG_work_4$F1_6))
KBG_work_4$F1_6[which(as_factor(KBG_work_4$F1_6)=='weiß nicht, keine Angabe')] <- NA
KBG_work_4$HHEinkommen <- ifelse(KBG_work_4$S03=='nein', KBG_work_4$F1_3,
                                 ifelse(KBG_work_4$S03=='ja', KBG_work_4$F1_3 + KBG_work_4$F1_6, NA))

# with NA
nrow(KBG_work_4[which(is.na(KBG_work_4$F1_6) & KBG_work_4$S03=='ja' & !is.na(KBG_work_4$F1_3)),]) # 122
nrow(KBG_work_4[which(is.na(KBG_work_4$F1_6) & KBG_work_4$S03=='ja' & is.na(KBG_work_4$F1_3)),]) # 270
nrow(KBG_work_4[which(!is.na(KBG_work_4$F1_6) & KBG_work_4$S03=='ja' & is.na(KBG_work_4$F1_3)),]) #120
nrow(KBG_work_4[which(KBG_work_4$S03=='nein' & is.na(KBG_work_4$F1_3)),]) # 49

#### Einkommen des Mannes und Einkommen der Frau ####
# 
KBG_work_4$Einkommenw <- ifelse(KBG_work_4$S01== 'weiblich', KBG_work_4$F1_3, # Frau beantwortet Fragebogen
                         ifelse(KBG_work_4$S01== 'maennlich', KBG_work_4$F1_6, NA)) # Mann beantwortet Fragebogen

summary(KBG_work_4$Einkommenw)

KBG_work_4$Einkommenm <- ifelse(KBG_work_4$S01== 'maennlich', KBG_work_4$F1_3, # Mann beantwortet Fragebogen
                                ifelse(KBG_work_4$S01== 'weiblich', KBG_work_4$F1_6, NA)) # Frau beantwortet Fragebogen

summary(KBG_work_4$Einkommenm)

#### Wochenarbeitsstunden vor Geburt Mann und Frau ####

KBG_work_4$Stundenw <- ifelse(KBG_work_4$S01== 'weiblich', KBG_work_4$F1_2, # Frau beantwortet Fragebogen
                                ifelse(KBG_work_4$S01== 'maennlich' & KBG_work_4$S03=='ja', KBG_work_4$F1_5, NA)) # Mann beantwortet Fragebogen

summary(KBG_work_4$Stundenw)

KBG_work_4$Stundenm <- ifelse(KBG_work_4$S01== 'maennlich', KBG_work_4$F1_2, # Mann beantwortet Fragebogen
                                ifelse(KBG_work_4$S01== 'weiblich' & KBG_work_4$S03=='ja', KBG_work_4$F1_5, NA)) # Frau beantwortet Fragebogen

summary(KBG_work_4$Stundenm)


#### Erwerbstatus vor der Geburt Mann und Frau ####
KBG_work_4$Erwerbsstatusw <- factor(ifelse(KBG_work_4$S01== 'weiblich', KBG_work_4$F1_1, # Frau beantwortet Fragebogen
                              ifelse(KBG_work_4$S01== 'maennlich', KBG_work_4$F1_4, NA)),
                              labels = levels(as_factor(KBG_work_4$F1_4)))# Mann beantwortet Fragebogen


summary(KBG_work_4$Erwerbsstatusw)

KBG_work_4$Erwerbsstatusm <- factor(ifelse(KBG_work_4$S01== 'maennlich', KBG_work_4$F1_1, # Mann beantwortet Fragebogen
                              ifelse(KBG_work_4$S01== 'weiblich', KBG_work_4$F1_4, NA)),
                              labels = levels(as_factor(KBG_work_4$F1_4))) # Frau beantwortet Fragebogen

summary(KBG_work_4$Erwerbsstatusm)

#### Alter Mann und Frau ####
KBG_work_4$Alterw <- ifelse(KBG_work_4$S01== 'weiblich', KBG_work_4$S02, # Frau beantwortet Fragebogen
                                    ifelse(KBG_work_4$S01== 'maennlich', KBG_work_4$S04, NA)) # Mann beantwortet Fragebogen

summary(KBG_work_4$Alterw)

KBG_work_4$Alterm <- ifelse(KBG_work_4$S01== 'maennlich', KBG_work_4$S02, # Mann beantwortet Fragebogen
                                    ifelse(KBG_work_4$S01== 'weiblich', KBG_work_4$S04, NA)) # Frau beantwortet Fragebogen

summary(KBG_work_4$Alterm)

#### Bildung Mann und Frau ####
KBG_work_4$Bildungw <- factor(ifelse(KBG_work_4$S01== 'weiblich', as_factor(KBG_work_4$S07), # Frau beantwortet Fragebogen
                            ifelse(KBG_work_4$S01== 'maennlich' & KBG_work_4$S03=='ja', as_factor(KBG_work_4$S08), NA)),
                            labels = levels(as_factor(KBG_work_4$S07)))# Mann beantwortet Fragebogen

summary(KBG_work_4$Bildungw)

KBG_work_4$Bildungm <- factor(ifelse(KBG_work_4$S01== 'maennlich', as_factor(KBG_work_4$S07), # Mann beantwortet Fragebogen
                            ifelse(KBG_work_4$S01== 'weiblich' & KBG_work_4$S03=='ja', as_factor(KBG_work_4$S08), NA)),
                            labels = levels(as_factor(KBG_work_4$S07)))# Frau beantwortet Fragebogen

summary(KBG_work_4$Bildungm)


KBG_work_4$KBGm_beteiligt <- ifelse(KBG_work_4$KBGm>0, 1,
                                    ifelse(KBG_work_4$KBGm==0, 0, NA))
summary(as_factor(KBG_work_4$KBGm_beteiligt))

#### 1. still some data preperation ####

# logiteinkommen = 0 if Konto
# logiteinkommen = 1 if Einkommensabhängig

KBG_work_4$logiteinkommen <- ifelse(KBG_work_4$F2_2==1, 0, # Konto
                                    ifelse(KBG_work_4$F2_2==2, 1, NA)) # Einkommensabhängig
KBG_work_4$logiteinkommen <- factor(KBG_work_4$logiteinkommen,labels = c("Konto", "Einkommensabhaengig"))

# relevel base categories for regression
KBG_work_4$logiteinkommen  <- relevel(KBG_work_4$logiteinkommen , "Konto") # base = Konto
KBG_work_4$S01  <- relevel(KBG_work_4$S01 , "weiblich") # base = weiblich
KBG_work_4$F1_1  <- relevel(KBG_work_4$F1_1 , "unselbststaending") # base = unselbststaending
KBG_work_4$S07  <- relevel(KBG_work_4$S07 , "studium") # base = studium 
KBG_work_4$F3_3 <- relevel(as_factor(KBG_work_4$F3_3), "gar nicht relevant") # base = gar nicht relevant
KBG_work_4$S09 <-  relevel(KBG_work_4$S09 , "wien")

KBG_work_4$Erwerbsstatusw  <- relevel(KBG_work_4$Erwerbsstatusw , "unselbststaending") # base = unselbststaending
KBG_work_4$Erwerbsstatusm  <- relevel(KBG_work_4$Erwerbsstatusm , "unselbststaending") # base = unselbststaending
KBG_work_4$Bildungw  <- relevel(KBG_work_4$Bildungw , "studium") # base = studium 
KBG_work_4$Bildungm  <- relevel(KBG_work_4$Bildungm , "studium") # base = studium 

KBG_work_4$F1_4  <- relevel(KBG_work_4$F1_4 , "unselbststaending") # base = unselbststaending
KBG_work_4$S08 <-  relevel(KBG_work_4$S08 , "studium") # base = studium 


#### Imputations for those who did' not work before KBG ####
# 187 NA's
KBG_work_4$F1_3[which(KBG_work_4$F1_1 %in% c("studierend", "karenz", "haushalt", "arbeitslos"))] <- 0 # Einkommen
# 2 NA's
KBG_work_4$F1_2[which(KBG_work_4$F1_1 %in% c("studierend", "karenz", "haushalt", "arbeitslos"))] <- 0 # Wochenstunden


# all 
KBG_work_4[which(KBG_work_4$F1_3!=0 & KBG_work_4$F1_2==0), c("F1_3", "F1_2")]
KBG_work_4[which(KBG_work_4$F1_2!=0 & KBG_work_4$F1_3==0), c("F1_3", "F1_2")]

# 1 is NA in both
KBG_work_4[which(is.na(KBG_work_4$F1_3) & is.na(KBG_work_4$F1_2)), c("F1_3", "F1_2", "id")]
# 1 is NA in F1_2 but is not NA in F1_3
KBG_work_4[which(!is.na(KBG_work_4$F1_3) & is.na(KBG_work_4$F1_2)), c("F1_3", "F1_2", "id")]
# 186 are NA in F1_3 but not in F1_3
KBG_work_4[which(is.na(KBG_work_4$F1_3) & !is.na(KBG_work_4$F1_2)), c("F1_3", "F1_2", "id")]

# --> NA's in Nettostundeneinkommen: 188

Nettostundeneinkommen <- (matrix(99999, nrow(KBG_work_4), 1))
KBG_work_4 <- cbind(KBG_work_4, Nettostundeneinkommen)
rm(Nettostundeneinkommen)
KBG_work_4$Nettostundeneinkommen[which(KBG_work_4$F1_3!=0 & KBG_work_4$F1_2!=0)] <- KBG_work_4[which(KBG_work_4$F1_3!=0 & KBG_work_4$F1_2!=0), "F1_3"] / 4.34524/ KBG_work_4[which(KBG_work_4$F1_3!=0 & KBG_work_4$F1_2!=0), "F1_2"]
  
KBG_work_4$Nettostundeneinkommen[which(KBG_work_4$F1_3==0 & KBG_work_4$F1_2==0)] <- 0
KBG_work_4$Nettostundeneinkommen[which(is.na(KBG_work_4$F1_3) | is.na(KBG_work_4$F1_2))] <- NA
length(KBG_work_4$Nettostundeneinkommen[which(is.na(KBG_work_4$F1_3) | is.na(KBG_work_4$F1_2))])

KBG_work_4[which(KBG_work_4$Nettostundeneinkommen==99999), c("F1_2", "F1_3")]
summary(KBG_work_4$Nettostundeneinkommen)
