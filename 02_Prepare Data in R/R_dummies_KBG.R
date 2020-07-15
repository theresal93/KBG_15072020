#### factor variables R ####
setwd("Z:/KBG")

library(foreign)
library(haven)
library(dplyr)  
library(descr)
require(readstata13)

KBG_work_3 <- read_sav("Y:/common/Projekte/2 Laufende Projekte/Kinderbetreuungsgeldkonto - Evaluierung ab 2018/Quant_BezieherInnen_2019/Daten/KBG_work_3.sav")


# S01 - Geschlecht
# 1=weiblich
# 2=m?nnlich

KBG_work_3$S01[which(KBG_work_3$S01==99)] <- NA
KBG_work_3$S01 <- factor(KBG_work_3$S01, labels = c("weiblich","maennlich")) 

summary((KBG_work_3$S01))

#S03 - leben mit Partnerin
# 1    Ja
# 2  Nein
KBG_work_3$S03[which(KBG_work_3$S03==99)] <- NA
KBG_work_3$S03 <- factor(KBG_work_3$S03, labels = c("ja","nein")) 

summary(factor(KBG_work_3$S03))

##### F1.1 - Erwerbsstatus vor Geburt ####
# value                                        label
# 1                                           Unselbstst?ndig erwerbst?tig
# 2                                             Selbstst?ndig erwerbst?tig
# 3 Studierend bzw. in Ausbildung (Pr?senzdienst, freiwilliges Sozialjahr)
# 4                                                                 Karenz
# 5                                                      im Haushalt t?tig
# 6                                                             Arbeitslos
KBG_work_3$F1.1[which(KBG_work_3$F1.1==99)] <- NA
KBG_work_3$F1.1 <- factor(KBG_work_3$F1.1, labels = c("unselbststaending","selbststaending", "studierend", "karenz", "haushalt", "arbeitslos")) 
summary(factor(KBG_work_3$F1.1))

##### F1.4 - Erwerbsstatus Partner_in vor Geburt ####
# value                                        label
# 1                                           Unselbstst?ndig erwerbst?tig
# 2                                             Selbstst?ndig erwerbst?tig
# 3 Studierend bzw. in Ausbildung (Pr?senzdienst, freiwilliges Sozialjahr)
# 4                                                                 Karenz
# 5                                                      im Haushalt t?tig
# 6                                                             Arbeitslos

# 92 NA's f?r jene ohne Partner_in

KBG_work_3$F1.4[which(KBG_work_3$F1.4==99)] <- NA
KBG_work_3$F1.4 <- factor(KBG_work_3$F1.4, labels = c("unselbststaending","selbststaending", "studierend", "karenz", "haushalt", "arbeitslos")) 
summary(factor(KBG_work_3$F1.4))


#### F2.1 - Beziehen Sie oder Ihr Partner/Ihre Partnerin aktuell KBG? ###

# value label
# 1    ja
# 2  nein
KBG_work_3$F2.1[which(KBG_work_3$F2.1==99)] <- NA
KBG_work_3$F2.1 <- factor(KBG_work_3$F2.1, labels = c("ja","nein")) 
summary(factor(KBG_work_3$F2.1))

#### F2.7 - Partnerschaftsbonus ####

#1 ja
#2 nein Aufteilung passte nicht 
#3 kenne diese Leistung nicht
#99 kA
KBG_work_3$F2.7[which(KBG_work_3$F2.7==99)] <- NA
KBG_work_3$F2.7 <- factor(KBG_work_3$F2.7, labels = c("ja","aufteilung", "unbekannt")) 
summary(factor(KBG_work_3$F2.7))


### zus?tzliche dummy, die nur zwischen beziehen und nicht beziehen unterscheidet ####
#1= bezieht
#0= bezieht nicht
#NA= keine Angabe
F2.7new <- as.numeric(KBG_work_3$F2.7)
F2.7new[is.na(F2.7new)] <- 0

KBG_work_3$beziehtPB <- factor(ifelse(F2.7new==1, 1,
                                      ifelse((F2.7new==0 | F2.7new==2 | F2.7new==3), 0 , NA)))
KBG_work_3$beziehtPB <- factor(KBG_work_3$beziehtPB, labels = c("beziehtnicht","bezieht"))
summary(KBG_work_3$beziehtPB)


##### F2.10 - Wann fiel Entscheidung KBG-Variante ####
# value                       label
# 1 w?hrend der Schwangerschaft
# 2             nach der Geburt
# 99    wei? nicht, keine Angabe (als NA behandelt)
KBG_work_3$F2.10[which(KBG_work_3$F2.10==99)] <- NA
KBG_work_3$F2.10 <- factor(KBG_work_3$F2.10, labels = c("waehrendschwangerschaft","nachgeburt")) 
summary(factor(KBG_work_3$F2.10))


#### F2.11 - Haben Sie Familienzeitbonus bezogen?
# 1 ja
# 2 nein - kenne diese Leistung nicht
# 3 nein - anderer Grund
# 99 ka
KBG_work_3$F2.11[which(KBG_work_3$F2.11==99)] <- NA
KBG_work_3$F2.11 <- factor(KBG_work_3$F2.11, labels = c("beziehtFZB","kenntFZBnicht", "andererGrund")) 
summary(factor(KBG_work_3$F2.11))

#### F2.14 - l?nge Karenz ####
# 1 Karenz l?nger KBG Bezug
# 2 Karenz gleich lang KBG Bezug
# 3 Karenz k?rzer KBG Bezug
# 99 ka
KBG_work_3$F2.14[which(KBG_work_3$F2.14==99)] <- NA
KBG_work_3$F2.14 <- factor(KBG_work_3$F2.14, labels = c("karenzlaenger","kaenzgleich", "kaenzkuerzer")) 
summary(factor(KBG_work_3$F2.14))


#### F3.2a1 ist die Info-Hotline bekannt, wird sie genutzt F3.2a2 ####

# bekannt
# 1 bekannt
# 2 unbekannt
KBG_work_3$F3.2a1[which(KBG_work_3$F3.2a1==99)] <- NA
KBG_work_3$F3.2a1 <- factor(KBG_work_3$F3.2a1, labels = c("bekannt","unbekannt")) 
summary(factor(KBG_work_3$F3.2a1))

# 1 genutzt
# 0 nicht genutzt (bekannt oder nicht)
F3.2a2new <- as.numeric(KBG_work_3$F3.2a2)
F3.2a2new[is.na(F3.2a2new)] <- 0

KBG_work_3$F3.2a2_genutzt <- factor(ifelse(F3.2a2new==1, 1,0), labels = c("ungenutzt","genutzt"))
summary(KBG_work_3$F3.2a2_genutzt)


#### F3.2b1 ist die Online-Rechner bekannt, wird sie genutzt F3.2b2 ####
# bekannt
# 1 bekannt
# 2 unbekannt
KBG_work_3$F3.2b1[which(KBG_work_3$F3.2b1==99)] <- NA
KBG_work_3$F3.2b1 <- factor(KBG_work_3$F3.2b1, labels = c("bekannt","unbekannt")) 
summary(factor(KBG_work_3$F3.2b1))

# 1 genutzt
# 0 nicht genutzt (bekannt oder nicht)
F3.2b2new <- as.numeric(KBG_work_3$F3.2b2)
F3.2b2new[is.na(F3.2b2new)] <- 0

KBG_work_3$F3.2b2_genutzt <- factor(ifelse(F3.2b2new==1, 1,0), labels = c("ungenutzt","genutzt"))
summary(KBG_work_3$F3.2b2_genutzt)

#### F3.3 Wie relevant auch t?gliche Variante ####
# # value               label
# # 1       sehr relevant
# # 2       eher relevant
# # 3 eher nicht relevant
# # 4  gar nicht relevant
# KBG_work_3$F3.3[which(KBG_work_3$F3.3==99)] <- NA
# KBG_work_3$F3.3 <- factor(KBG_work_3$F3.3, labels = c("sehrrelevent","eherrelevant", "ehernichtrelevant", "nichtrelevant")) 
# summary(factor(KBG_work_3$F3.3))

####  F4.1 - aktueller Erwerbsstatus ####
# Labels:
#   value                                                                  label
# 1                                           Unselbstst?ndig erwerbst?tig
# 2                                             Selbstst?ndig erwerbst?tig
# 3 Studierend bzw. in Ausbildung (Pr?senzdienst, freiwilliges Sozialjahr)
# 4                                                                 Karenz
# 5                                                      im Haushalt t?tig
# 6                                                             Arbeitslos
KBG_work_3$F4.1[which(KBG_work_3$F4.1==99)] <- NA
KBG_work_3$F4.1 <- factor(KBG_work_3$F4.1, labels = c("unselbststaending","selbststaending", "studierend", "karenz", "haushalt", "arbeitslos")) 
summary(factor(KBG_work_3$F4.1))

####  F4.8 - aktueller Erwerbsstatus Partner_in ####
# Labels:
#   value                                                                  label
# 1                                           Unselbstst?ndig erwerbst?tig
# 2                                             Selbstst?ndig erwerbst?tig
# 3 Studierend bzw. in Ausbildung (Pr?senzdienst, freiwilliges Sozialjahr)
# 4                                                                 Karenz
# 5                                                      im Haushalt t?tig
# 6                                                             Arbeitslos
KBG_work_3$F4.8[which(KBG_work_3$F4.8==99)] <- NA
KBG_work_3$F4.8 <- factor(KBG_work_3$F4.8, labels = c("unselbststaending","selbststaending", "studierend", "karenz", "haushalt", "arbeitslos")) 
summary(factor(KBG_work_3$F4.8))

#### S07 - h?chste Bildung ####
# value                                 label
# 1 Pflichtschule ohne weitere Ausbildung
# 2               Pflichtschule mit Lehre
# 3       Fachschule (Handelsschule etc.)
# 4                 AHS, BHS (mit Matura)
# 5               Abgeschlossenes Studium
# 99              wei? nicht, keine Angabe
KBG_work_3$S07[which(KBG_work_3$S07==99)] <- NA
KBG_work_3$S07 <- factor(KBG_work_3$S07, labels = c("pflichtschule","lehre", "fachschule", "matura", "studium"))
summary(factor(KBG_work_3$S07))


#### S08 - h?chste Bildung Partner_in ####
# value                                 label
# 1 Pflichtschule ohne weitere Ausbildung
# 2               Pflichtschule mit Lehre
# 3       Fachschule (Handelsschule etc.)
# 4                 AHS, BHS (mit Matura)
# 5               Abgeschlossenes Studium
# 99              wei? nicht, keine Angabe
KBG_work_3$S08[which(KBG_work_3$S08==99)] <- NA
KBG_work_3$S08 <- factor(KBG_work_3$S08, labels = c("pflichtschule","lehre", "fachschule", "matura", "studium"))
summary(factor(KBG_work_3$S08))

#### S09 - Bundesland ####
# value            label
# 1       Burgenland
# 2          K?rnten
# 3 Nieder?sterreich
# 4   Ober?sterreich
# 5         Salzburg
# 6       Steiermark
# 7            Tirol
# 8       Vorarlberg
# 9             Wien
KBG_work_3$S09[which(KBG_work_3$S09==99)] <- NA
KBG_work_3$S09 <- factor(KBG_work_3$S09, labels = c("bgl","ktn", "noe", "ooe", "sbg", "stmk", "tirol", "vbg", "wien"))
summary(factor(KBG_work_3$S09))

#### S10 - Anzahl Einwohner ####
# value                    label
# 1             Bis 2.000 EW
# 2             Bis 5.000 EW
# 3            Bis 10.000 EW
# 4            Bis 20.000 EW
# 5            Bis 50.000 EW
# 6       Mehr als 50.000 EW
# 7                     Wien
# 99 wei? nicht, keine Angabe
KBG_work_3$S10[which(KBG_work_3$S10==99)] <- NA
KBG_work_3$S10 <- factor(KBG_work_3$S10, labels = c("2000","5000", "10000", "20000", "50000", "mehrals50000", "wien"))
summary(factor(KBG_work_3$S10))

#### S11 - Einstellung Familie
# value                    label
# 1              sehr modern
# 2              eher modern
# 3        eher traditionell
# 4        sehr traditionell
# 99 wei? nicht, keine Angabe
KBG_work_3$S11[which(KBG_work_3$S11==99)] <- NA
KBG_work_3$S11 <- factor(KBG_work_3$S11, labels = c("sehrmodern","ehermodern", "ehertraditionell", "sehrtraditionell"))
summary(factor(KBG_work_3$S11))




##### Recoding continous variables #####
 
#### Einkommen ####
KBG_work_3$F1.3[which(KBG_work_3$F1.3== 99999)] <- NA
KBG_work_3$F1.6[which(KBG_work_3$F1.6== 99999)] <- NA
#### Anzahl der Stunden F1.2 
KBG_work_3$F1.2[which(KBG_work_3$F1.2== 99)] <- NA
