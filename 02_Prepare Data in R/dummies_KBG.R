#### dummies for categorial variables ####
setwd("Z:/KBG")

library(foreign)
library(haven)
library(dplyr)  
library(descr)
require(readstata13)
library(stringr)

KBG_work_2 <- read_sav("Y:/common/Projekte/2 Laufende Projekte/Kinderbetreuungsgeldkonto - Evaluierung ab 2018/Quant_BezieherInnen_2019/Daten/KBG_work_2.sav")
View(KBG_work_2)
colnames(KBG_work_2)


# S01 - Geschlecht
# 1=weiblich
# 2=männlich

KBG_work_2$S01_weiblich <- factor(ifelse(KBG_work_2$S01==1, 1,
                ifelse(KBG_work_2$S01==2, 0, NA)))

KBG_work_2$S01_maennlich <- factor(ifelse(KBG_work_2$S01==2, 1,
                                         ifelse(KBG_work_2$S01==1, 0, NA)))

summary(factor(KBG_work_2$S01))
summary(KBG_work_2$S01_weiblich)
summary(KBG_work_2$S01_maennlich)

#S03 - leben mit Partnerin
# 1    Ja
# 2  Nein

KBG_work_2$S03_hatpartner <- factor(ifelse(KBG_work_2$S03==1, 1,
                                     ifelse(KBG_work_2$S03==2, 0, NA)))

KBG_work_2$S03_keinpartner <- factor(ifelse(KBG_work_2$S03==2, 1,
                                           ifelse(KBG_work_2$S03==1, 0, NA)))

summary(factor(KBG_work_2$S03))
summary(KBG_work_2$S03_hatpartner)
summary(KBG_work_2$S03_keinpartner)


##### F1.1 - Erwerbsstatus vor Geburt ####
# value                                        label
# 1                                           Unselbstständig erwerbstätig
# 2                                             Selbstständig erwerbstätig
# 3 Studierend bzw. in Ausbildung (Präsenzdienst, freiwilliges Sozialjahr)
# 4                                                                 Karenz
# 5                                                      im Haushalt tätig
# 6                                                             Arbeitslos

# Unselbstständig
KBG_work_2$F1.1_unselbstaendig <- factor(ifelse(KBG_work_2$F1.1==1, 1,
                                           ifelse(is.na(KBG_work_2$F1.1)==TRUE, NA, 0)))
# Selbstständig
KBG_work_2$F1.1_selbstaendig <- factor(ifelse(KBG_work_2$F1.1==2, 1,
                                                ifelse(is.na(KBG_work_2$F1.1)==TRUE, NA, 0)))
# Studierend
KBG_work_2$F1.1_studierend <- factor(ifelse(KBG_work_2$F1.1==3, 1,
                                              ifelse(is.na(KBG_work_2$F1.1)==TRUE, NA, 0)))
# Karenz
KBG_work_2$F1.1_karenz <- factor(ifelse(KBG_work_2$F1.1==4, 1,
                                            ifelse(is.na(KBG_work_2$F1.1)==TRUE, NA, 0)))
# Studierend
KBG_work_2$F1.1_haushalt <- factor(ifelse(KBG_work_2$F1.1==5, 1,
                                            ifelse(is.na(KBG_work_2$F1.1)==TRUE, NA, 0)))
# Studierend
KBG_work_2$F1.1_arbeitslos <- factor(ifelse(KBG_work_2$F1.1==6, 1,
                                            ifelse(is.na(KBG_work_2$F1.1)==TRUE, NA, 0)))

summary(factor(KBG_work_2$F1.1))
summary(KBG_work_2$F1.1_unselbstaendig)
summary(KBG_work_2$F1.1_selbstaendig)
summary(KBG_work_2$F1.1_studierend)
summary(KBG_work_2$F1.1_karenz)
summary(KBG_work_2$F1.1_haushalt)
summary(KBG_work_2$F1.1_arbeitslos)


##### F1.4 - Erwerbsstatus Partner_in vor Geburt ####
# value                                        label
# 1                                           Unselbstständig erwerbstätig
# 2                                             Selbstständig erwerbstätig
# 3 Studierend bzw. in Ausbildung (Präsenzdienst, freiwilliges Sozialjahr)
# 4                                                                 Karenz
# 5                                                      im Haushalt tätig
# 6                                                             Arbeitslos

# 92 NA's für jene ohne Partner_in

# Unselbstständig
KBG_work_2$F1.4_unselbstaendig <- factor(ifelse(KBG_work_2$F1.4==1, 1,
                                                ifelse(is.na(KBG_work_2$F1.4)==TRUE, NA, 0)))
# Selbstständig
KBG_work_2$F1.4_selbstaendig <- factor(ifelse(KBG_work_2$F1.4==2, 1,
                                              ifelse(is.na(KBG_work_2$F1.4)==TRUE, NA, 0)))
# Studierend
KBG_work_2$F1.4_studierend <- factor(ifelse(KBG_work_2$F1.4==3, 1,
                                            ifelse(is.na(KBG_work_2$F1.4)==TRUE, NA, 0)))
# Karenz
KBG_work_2$F1.4_karenz <- factor(ifelse(KBG_work_2$F1.4==4, 1,
                                        ifelse(is.na(KBG_work_2$F1.4)==TRUE, NA, 0)))
# Studierend
KBG_work_2$F1.4_haushalt <- factor(ifelse(KBG_work_2$F1.4==5, 1,
                                          ifelse(is.na(KBG_work_2$F1.4)==TRUE, NA, 0)))
# Arbeitslos
KBG_work_2$F1.4_arbeitslos <- factor(ifelse(KBG_work_2$F1.4==6, 1,
                                            ifelse(is.na(KBG_work_2$F1.4)==TRUE, NA, 0)))

summary(factor(KBG_work_2$F1.4))
summary(KBG_work_2$F1.4_unselbstaendig)
summary(KBG_work_2$F1.4_selbstaendig)
summary(KBG_work_2$F1.4_studierend)
summary(KBG_work_2$F1.4_karenz)
summary(KBG_work_2$F1.4_haushalt)
summary(KBG_work_2$F1.4_arbeitslos)


#### F2.1 - Beziehen Sie oder Ihr Partner/Ihre Partnerin aktuell KBG? ###

#nicht benötigt

# value label
# 1    ja
# 2  nein

# summary(factor(KBG_work_2$F2.1))
# summary(KBG_work_2$F2.1_aktuellKBG)
# KBG_work_2$F2.1_aktuellKBG <- factor(ifelse(KBG_work_2$F2.1==1, 1,
#                                             ifelse(is.na(KBG_work_2$F2.1)==TRUE, NA, 0)))
# 
# KBG_work_2$F2.1_aktuellkeinKBG <- factor(ifelse(KBG_work_2$F2.1==2, 1,
#                                             ifelse(is.na(KBG_work_2$F2.1)==TRUE, NA, 0)))

#### F2.7 - Partnerschaftsbonus ####

#1 ja
#2 nein Aufteilung passte nicht 
#3 kenne diese Leistung nicht
#99 kA

KBG_work_2$F2.7_beziehtPB <- factor(ifelse(KBG_work_2$F2.7==1, 1,
                                                  ifelse(is.na(KBG_work_2$F2.7)==TRUE | KBG_work_2$F2.7==99, NA, 0)))

KBG_work_2$F2.7_aufteilung <- factor(ifelse(KBG_work_2$F2.7==2, 1,
                                             ifelse(is.na(KBG_work_2$F2.7)==TRUE | KBG_work_2$F2.7==99, NA, 0)))

KBG_work_2$F2.7_unbekannt<- factor(ifelse(KBG_work_2$F2.7==3, 1,
                                             ifelse(is.na(KBG_work_2$F2.7)==TRUE | KBG_work_2$F2.7==99, NA, 0)))


summary(factor(KBG_work_2$F2.7))
summary(KBG_work_2$F2.7_beziehtPB )
summary(KBG_work_2$F2.7_aufteilung)
summary(KBG_work_2$F2.7_unbekannt)



### zusätzliche dummy, die nur zwischen beziehen und nicht beziehen unterscheidet ####

#1= bezieht
#0= bezieht nicht
#NA= keine Angabe

F2.7new <- KBG_work_2$F2.7
F2.7new[is.na(F2.7new)] <- 0

KBG_work_2$beziehtPB <- factor(ifelse(F2.7new==1, 1,
                                          ifelse((F2.7new==0 | F2.7new==2 | F2.7new==3), 0 , NA)))
summary(factor(KBG_work_2$F2.7))
summary(KBG_work_2$beziehtPB)



##### F2.10 - Wann fiel Entscheidung KBG-Variante ####
# value                       label
# 1 während der Schwangerschaft
# 2             nach der Geburt
# 99    weiß nicht, keine Angabe (als NA behandelt)

# während Schwangerschaft
KBG_work_2$F2.10_schwangerschaft <- factor(ifelse(KBG_work_2$F2.10==1, 1,
                                          ifelse(is.na(KBG_work_2$F2.10)==TRUE | KBG_work_2$F2.10==99, NA, 0)))

KBG_work_2$F2.10_nachgeburt <- factor(ifelse(KBG_work_2$F2.10==2, 1,
                                                  ifelse(is.na(KBG_work_2$F2.10)==TRUE | KBG_work_2$F2.10==99, NA, 0)))


summary(factor(KBG_work_2$F2.10))
summary(KBG_work_2$F2.10_schwangerschaft)
summary(KBG_work_2$F2.10_nachgeburt)


#### F2.11 - Haben Sie Familienzeitbonus bezogen?
# 1 ja
# 2 nein - kenne diese Leistung nicht
# 3 nein - anderer Grund
# 99 ka


KBG_work_2$F2.11_beziehtFZB <- factor(ifelse(KBG_work_2$F2.11==1, 1,
                                                  ifelse(is.na(KBG_work_2$F2.11)==TRUE | KBG_work_2$F2.11==99, NA, 0)))

KBG_work_2$F2.11_kenntFZBnicht <- factor(ifelse(KBG_work_2$F2.11==2, 1,
                                                ifelse(is.na(KBG_work_2$F2.11)==TRUE | KBG_work_2$F2.11==99, NA, 0)))

KBG_work_2$F2.11_andererGrund <- factor(ifelse(KBG_work_2$F2.11==3, 1,
                                                ifelse(is.na(KBG_work_2$F2.11)==TRUE | KBG_work_2$F2.11==99, NA, 0)))

summary(factor(KBG_work_2$F2.11))
summary(KBG_work_2$F2.11_beziehtFZB)
summary(KBG_work_2$F2.11_kenntFZBnicht)
summary(KBG_work_2$F2.11_andererGrund)

#### F2.14 - länge Karenz ####
# 1 Karenz länger KBG Bezug
# 2 Karenz gleich lang KBG Bezug
# 3 Karenz kürzer KBG Bezug
# 99 ka


KBG_work_2$F2.14_karenzlaenger <- factor(ifelse(KBG_work_2$F2.14==1, 1,
                                             ifelse(is.na(KBG_work_2$F2.14)==TRUE | KBG_work_2$F2.14==99, NA, 0)))


KBG_work_2$F2.14_kaenzgleich <- factor(ifelse(KBG_work_2$F2.14==2, 1,
                                             ifelse(is.na(KBG_work_2$F2.14)==TRUE | KBG_work_2$F2.14==99, NA, 0)))


KBG_work_2$F2.14_kaenzkuerzer <- factor(ifelse(KBG_work_2$F2.14==3, 1,
                                              ifelse(is.na(KBG_work_2$F2.14)==TRUE | KBG_work_2$F2.14==99, NA, 0)))

summary(factor(KBG_work_2$F2.14))
summary(KBG_work_2$F2.14_karenzlaenger)
summary(KBG_work_2$F2.14_kaenzgleich)
summary(KBG_work_2$F2.14_kaenzkuerzer)

#### F3.2a1 ist die Info-Hotline bekannt, wird sie genutzt F3.2a2 ####

# bekannt
# 1 bekannt
# 0 unbekannt
KBG_work_2$F3.2a1_bekannt <- factor(ifelse(KBG_work_2$F3.2a1==1, 1,0))

# 1 genutzt
# 0 nicht genutzt (bekannt oder nicht)
F3.2a2new <- KBG_work_2$F3.2a2
F3.2a2new[is.na(F3.2a2new)] <- 0
KBG_work_2$F3.2a2_genutzt <- factor(ifelse(F3.2a2new==1, 1,0))
                                           

summary(factor(KBG_work_2$F3.2a1))
summary(KBG_work_2$F3.2a1_bekannt)
summary(factor(KBG_work_2$F3.2a2))
summary(KBG_work_2$F3.2a2_genutzt)


#### F3.2b1 ist die Online-Rechner bekannt, wird sie genutzt F3.2b2 ####
# bekannt
# 1 bekannt
# 0 unbekannt
KBG_work_2$F3.2b1_bekannt <- factor(ifelse(KBG_work_2$F3.2b1==1, 1,0))

# 1 genutzt
# 0 nicht genutzt (bekannt oder nicht)
F3.2b2new <- KBG_work_2$F3.2b2
F3.2b2new[is.na(F3.2b2new)] <- 0
KBG_work_2$F3.2b2_genutzt <- factor(ifelse(F3.2b2new==1, 1,0))


summary(factor(KBG_work_2$F3.2b1))
summary(KBG_work_2$F3.2b1_bekannt)
summary(factor(KBG_work_2$F3.2b2))
summary(KBG_work_2$F3.2b2_genutzt)

#### F3.3 Wie relevant auch tägliche Variante ####
# value               label
# 1       sehr relevant
# 2       eher relevant
# 3 eher nicht relevant
# 4  gar nicht relevant

KBG_work_2$F3.3_sehrrelevant <- factor(ifelse(KBG_work_2$F3.3==1, 1,0))
KBG_work_2$F3.3_eherrelevant <- factor(ifelse(KBG_work_2$F3.3==2, 1,0))
KBG_work_2$F3.3_ehernichtrelevant <- factor(ifelse(KBG_work_2$F3.3==3, 1,0))
KBG_work_2$F3.3_nichtrelevant <- factor(ifelse(KBG_work_2$F3.3==4, 1,0))

summary(factor(KBG_work_2$F3.3))
summary(KBG_work_2$F3.3_sehrrelevant)
summary(factor(KBG_work_2$F3.3_eherrelevant))
summary(KBG_work_2$F3.3_ehernichtrelevant)
summary(KBG_work_2$F3.3_nichtrelevant)


####  F4.1 - aktueller Erwerbsstatus ####
# Labels:
#   value                                                                  label
# 1                                           Unselbstständig erwerbstätig
# 2                                             Selbstständig erwerbstätig
# 3 Studierend bzw. in Ausbildung (Präsenzdienst, freiwilliges Sozialjahr)
# 4                                                                 Karenz
# 5                                                      im Haushalt tätig
# 6                                                             Arbeitslos


# Unselbstständig
KBG_work_2$F4.1_unselbstaendig <- factor(ifelse(KBG_work_2$F4.1==1, 1,
                                                ifelse(is.na(KBG_work_2$F4.1)==TRUE, NA, 0)))
# Selbstständig
KBG_work_2$F4.1_selbstaendig <- factor(ifelse(KBG_work_2$F4.1==2, 1,
                                              ifelse(is.na(KBG_work_2$F4.1)==TRUE, NA, 0)))
# Studierend
KBG_work_2$F4.1_studierend <- factor(ifelse(KBG_work_2$F4.1==3, 1,
                                            ifelse(is.na(KBG_work_2$F4.1)==TRUE, NA, 0)))
# Karenz
KBG_work_2$F4.1_karenz <- factor(ifelse(KBG_work_2$F4.1==4, 1,
                                        ifelse(is.na(KBG_work_2$F4.1)==TRUE, NA, 0)))
# Studierend
KBG_work_2$F4.1_haushalt <- factor(ifelse(KBG_work_2$F4.1==5, 1,
                                          ifelse(is.na(KBG_work_2$F4.1)==TRUE, NA, 0)))
# Arbeitslos
KBG_work_2$F4.1_arbeitslos <- factor(ifelse(KBG_work_2$F4.1==6, 1,
                                            ifelse(is.na(KBG_work_2$F4.1)==TRUE, NA, 0)))


####  F4.8 - aktueller Erwerbsstatus Partner_in ####
# Labels:
#   value                                                                  label
# 1                                           Unselbstständig erwerbstätig
# 2                                             Selbstständig erwerbstätig
# 3 Studierend bzw. in Ausbildung (Präsenzdienst, freiwilliges Sozialjahr)
# 4                                                                 Karenz
# 5                                                      im Haushalt tätig
# 6                                                             Arbeitslos


# Unselbstständig
KBG_work_2$F4.8_unselbstaendig <- factor(ifelse(KBG_work_2$F4.8==1, 1,
                                                ifelse(is.na(KBG_work_2$F4.8)==TRUE, NA, 0)))
# Selbstständig
KBG_work_2$F4.8_selbstaendig <- factor(ifelse(KBG_work_2$F4.8==2, 1,
                                              ifelse(is.na(KBG_work_2$F4.8)==TRUE, NA, 0)))
# Studierend
KBG_work_2$F4.8_studierend <- factor(ifelse(KBG_work_2$F4.8==3, 1,
                                            ifelse(is.na(KBG_work_2$F4.8)==TRUE, NA, 0)))
# Karenz
KBG_work_2$F4.8_karenz <- factor(ifelse(KBG_work_2$F4.8==4, 1,
                                        ifelse(is.na(KBG_work_2$F4.8)==TRUE, NA, 0)))
# Studierend
KBG_work_2$F4.8_haushalt <- factor(ifelse(KBG_work_2$F4.8==5, 1,
                                          ifelse(is.na(KBG_work_2$F4.8)==TRUE, NA, 0)))
# Arbeitslos
KBG_work_2$F4.8_arbeitslos <- factor(ifelse(KBG_work_2$F4.8==6, 1,
                                            ifelse(is.na(KBG_work_2$F4.8)==TRUE, NA, 0)))

summary(factor(KBG_work_2$F4.8))
summary(KBG_work_2$F4.8_unselbstaendig)
summary(KBG_work_2$F4.8_selbstaendig)
summary(KBG_work_2$F4.8_studierend)
summary(KBG_work_2$F4.8_karenz)
summary(KBG_work_2$F4.8_haushalt)
summary(KBG_work_2$F4.8_arbeitslos)



#### S07 - höchste Bildung ####
# value                                 label
# 1 Pflichtschule ohne weitere Ausbildung
# 2               Pflichtschule mit Lehre
# 3       Fachschule (Handelsschule etc.)
# 4                 AHS, BHS (mit Matura)
# 5               Abgeschlossenes Studium
# 99              weiß nicht, keine Angabe


KBG_work_2$S07_pflichtschule <- factor(ifelse(KBG_work_2$S07==1, 1,
                                              ifelse(is.na(KBG_work_2$S07)==TRUE | KBG_work_2$S07==99, NA, 0)))

KBG_work_2$S07_lehre <- factor(ifelse(KBG_work_2$S07==2, 1,
                                              ifelse(is.na(KBG_work_2$S07)==TRUE | KBG_work_2$S07==99, NA, 0)))

KBG_work_2$S07_fachschule <- factor(ifelse(KBG_work_2$S07==3, 1,
                                              ifelse(is.na(KBG_work_2$S07)==TRUE | KBG_work_2$S07==99, NA, 0)))

KBG_work_2$S07_matura <- factor(ifelse(KBG_work_2$S07==4, 1,
                                              ifelse(is.na(KBG_work_2$S07)==TRUE | KBG_work_2$S07==99, NA, 0)))

KBG_work_2$S07_studium<- factor(ifelse(KBG_work_2$S07==5, 1,
                                              ifelse(is.na(KBG_work_2$S07)==TRUE | KBG_work_2$S07==99, NA, 0)))

summary(factor(KBG_work_2$S07))
summary(KBG_work_2$S07_pflichtschule)
summary(KBG_work_2$S07_lehre)
summary(KBG_work_2$S07_fachschule)
summary(KBG_work_2$S07_matura)
summary(KBG_work_2$S07_studium)

#### S08 - höchste Bildung Partner_in ####
# value                                 label
# 1 Pflichtschule ohne weitere Ausbildung
# 2               Pflichtschule mit Lehre
# 3       Fachschule (Handelsschule etc.)
# 4                 AHS, BHS (mit Matura)
# 5               Abgeschlossenes Studium
# 99              weiß nicht, keine Angabe


KBG_work_2$S08_pflichtschule <- factor(ifelse(KBG_work_2$S08==1, 1,
                                              ifelse(is.na(KBG_work_2$S08)==TRUE | KBG_work_2$S08==99, NA, 0)))

KBG_work_2$S08_lehre <- factor(ifelse(KBG_work_2$S08==2, 1,
                                      ifelse(is.na(KBG_work_2$S08)==TRUE | KBG_work_2$S08==99, NA, 0)))

KBG_work_2$S08_fachschule <- factor(ifelse(KBG_work_2$S08==3, 1,
                                           ifelse(is.na(KBG_work_2$S08)==TRUE | KBG_work_2$S08==99, NA, 0)))

KBG_work_2$S08_matura <- factor(ifelse(KBG_work_2$S08==4, 1,
                                       ifelse(is.na(KBG_work_2$S08)==TRUE | KBG_work_2$S08==99, NA, 0)))

KBG_work_2$S08_studium<- factor(ifelse(KBG_work_2$S08==5, 1,
                                       ifelse(is.na(KBG_work_2$S08)==TRUE | KBG_work_2$S08==99, NA, 0)))


summary(factor(KBG_work_2$S08))
summary(KBG_work_2$S08_pflichtschule)
summary(KBG_work_2$S08_lehre)
summary(KBG_work_2$S08_fachschule)
summary(KBG_work_2$S08_matura)
summary(KBG_work_2$S08_studium)

#### S09 - Bundesland ####
# value            label
# 1       Burgenland
# 2          Kärnten
# 3 Niederösterreich
# 4   Oberösterreich
# 5         Salzburg
# 6       Steiermark
# 7            Tirol
# 8       Vorarlberg
# 9             Wien


KBG_work_2$S09_bgl <-  factor(ifelse(KBG_work_2$S09==1, 1,
                                       ifelse(is.na(KBG_work_2$S09)==TRUE | KBG_work_2$S09==99, NA, 0)))

KBG_work_2$S09_ktn <-  factor(ifelse(KBG_work_2$S09==2, 1,
                                     ifelse(is.na(KBG_work_2$S09)==TRUE | KBG_work_2$S09==99, NA, 0)))

KBG_work_2$S09_noe <-  factor(ifelse(KBG_work_2$S09==3, 1,
                                     ifelse(is.na(KBG_work_2$S09)==TRUE | KBG_work_2$S09==99, NA, 0)))

KBG_work_2$S09_ooe <-  factor(ifelse(KBG_work_2$S09==4, 1,
                                     ifelse(is.na(KBG_work_2$S09)==TRUE | KBG_work_2$S09==99, NA, 0)))

KBG_work_2$S09_sbg <-  factor(ifelse(KBG_work_2$S09==5, 1,
                                     ifelse(is.na(KBG_work_2$S09)==TRUE | KBG_work_2$S09==99, NA, 0)))


KBG_work_2$S09_stmk <-  factor(ifelse(KBG_work_2$S09==6, 1,
                                     ifelse(is.na(KBG_work_2$S09)==TRUE | KBG_work_2$S09==99, NA, 0)))

KBG_work_2$S09_tirol <-  factor(ifelse(KBG_work_2$S09==7, 1,
                                     ifelse(is.na(KBG_work_2$S09)==TRUE | KBG_work_2$S09==99, NA, 0)))

KBG_work_2$S09_vbg <-  factor(ifelse(KBG_work_2$S09==8, 1,
                                     ifelse(is.na(KBG_work_2$S09)==TRUE | KBG_work_2$S09==99, NA, 0)))

KBG_work_2$S09_wien <-  factor(ifelse(KBG_work_2$S09==9, 1,
                                     ifelse(is.na(KBG_work_2$S09)==TRUE | KBG_work_2$S09==99, NA, 0)))


summary(factor(KBG_work_2$S09))
summary(KBG_work_2$S09_bgl)
summary(KBG_work_2$S09_ktn)
summary(KBG_work_2$S09_noe)
summary(KBG_work_2$S09_ooe)
summary(KBG_work_2$S09_sbg)
summary(KBG_work_2$S09_stmk)
summary(KBG_work_2$S09_tirol)
summary(KBG_work_2$S09_vbg)
summary(KBG_work_2$S09_wien)


#### S10 - Anzahl Einwohner ####
# value                    label
# 1             Bis 2.000 EW
# 2             Bis 5.000 EW
# 3            Bis 10.000 EW
# 4            Bis 20.000 EW
# 5            Bis 50.000 EW
# 6       Mehr als 50.000 EW
# 7                     Wien
# 99 weiß nicht, keine Angabe

summary(factor(KBG_work_2$S10))


KBG_work_2$S10_2000 <- factor(ifelse(KBG_work_2$S10==1, 1,
                                     ifelse(is.na(KBG_work_2$S10)==TRUE | KBG_work_2$S10==99, NA, 0)))

KBG_work_2$S10_5000 <- factor(ifelse(KBG_work_2$S10==2, 1,
                                     ifelse(is.na(KBG_work_2$S10)==TRUE | KBG_work_2$S10==99, NA, 0)))

KBG_work_2$S10_10000 <- factor(ifelse(KBG_work_2$S10==3, 1,
                                     ifelse(is.na(KBG_work_2$S10)==TRUE | KBG_work_2$S10==99, NA, 0)))

KBG_work_2$S10_20000 <- factor(ifelse(KBG_work_2$S10==4, 1,
                                     ifelse(is.na(KBG_work_2$S10)==TRUE | KBG_work_2$S10==99, NA, 0)))


KBG_work_2$S10_50000 <- factor(ifelse(KBG_work_2$S10==5, 1,
                                      ifelse(is.na(KBG_work_2$S10)==TRUE | KBG_work_2$S10==99, NA, 0)))

KBG_work_2$S10_mehrals50000 <- factor(ifelse(KBG_work_2$S10==6, 1,
                                      ifelse(is.na(KBG_work_2$S10)==TRUE | KBG_work_2$S10==99, NA, 0)))

KBG_work_2$S10_wien<- factor(ifelse(KBG_work_2$S10==7, 1,
                                             ifelse(is.na(KBG_work_2$S10)==TRUE | KBG_work_2$S10==99, NA, 0)))


summary(factor(KBG_work_2$S10))
summary(KBG_work_2$S10_2000)
summary(KBG_work_2$S10_5000)
summary(KBG_work_2$S10_10000)
summary(KBG_work_2$S10_20000)
summary(KBG_work_2$S10_50000)
summary(KBG_work_2$S10_mehrals50000)
summary(KBG_work_2$S10_wien)

#### S11 - Einstellung Familie
# value                    label
# 1              sehr modern
# 2              eher modern
# 3        eher traditionell
# 4        sehr traditionell
# 99 weiß nicht, keine Angabe

summary(factor(KBG_work_2$S11))

KBG_work_2$S11_sehrmodern <- factor(ifelse(KBG_work_2$S11==1, 1,
                                     ifelse(is.na(KBG_work_2$S11)==TRUE | KBG_work_2$S11==99, NA, 0)))

KBG_work_2$S11_ehermodern <- factor(ifelse(KBG_work_2$S11==2, 1,
                                           ifelse(is.na(KBG_work_2$S11)==TRUE | KBG_work_2$S11==99, NA, 0)))

KBG_work_2$S11_ehertraditionell <- factor(ifelse(KBG_work_2$S11==3, 1,
                                           ifelse(is.na(KBG_work_2$S11)==TRUE | KBG_work_2$S11==99, NA, 0)))

KBG_work_2$S11_sehrtraditionell <- factor(ifelse(KBG_work_2$S11==4, 1,
                                           ifelse(is.na(KBG_work_2$S11)==TRUE | KBG_work_2$S11==99, NA, 0)))

summary(factor(KBG_work_2$S11))
summary(KBG_work_2$S11_sehrmodern)
summary(KBG_work_2$S11_ehermodern)
summary(KBG_work_2$S11_ehertraditionell)
summary(KBG_work_2$S11_sehrtraditionell)


############ zusätzliche Variablen ##################


#### gesamtdauerneu: wenn kein Partner_in dann nur eigener Bezug ansonsten f.2.4kor+f.2.5kor
KBG_work_2$gesamtdauerneu <- ifelse(is.na(KBG_work_2$gesamtdauer)==TRUE,
                                    KBG_work_2$f2.4kor,
                                    KBG_work_2$gesamtdauer)

#### system nach konto (3 Varianten) und EA abhängig ####
KBG_work_2$system <- ifelse(KBG_work_2$gesamtdauerneu<13 & KBG_work_2$F2.2==1, 1,
                            ifelse(KBG_work_2$gesamtdauerneu>=13 & KBG_work_2$gesamtdauerneu<25 & KBG_work_2$F2.2==1, 2,
                                   ifelse(KBG_work_2$gesamtdauerneu>=25 & KBG_work_2$F2.2==1, 3,
                                          ifelse(KBG_work_2$F2.2==2 &  is.na(KBG_work_2$gesamtdauerneu)==FALSE, 4, NA))))

KBG_work_2$system <- factor(KBG_work_2$system,labels = c("maximal 1 Jahr", "maximal 2 Jahre", "über 2 Jahre",
                                                         "Einkommensabängig"))

#### system nach 3 variante ####
KBG_work_2$variante <- ifelse(KBG_work_2$gesamtdauerneu<13 & KBG_work_2$F2.2==1, 1,
                              ifelse(KBG_work_2$gesamtdauerneu>=13 & KBG_work_2$gesamtdauerneu<25 & KBG_work_2$F2.2==1, 2,
                                     ifelse(KBG_work_2$gesamtdauerneu>=25 & KBG_work_2$F2.2==1, 3, NA)))


KBG_work_2$variante <- factor(KBG_work_2$variante,labels = c("maximal 1 Jahr", "maximal 2 Jahre", "über 2 Jahre"))

### Wiedereinstieg ###
####  F4.1 - aktueller Erwerbsstatus ####
# Labels:
#   value                                                                  label
# 1                                           Unselbstständig erwerbstätig
# 2                                             Selbstständig erwerbstätig
# 3 Studierend bzw. in Ausbildung (Präsenzdienst, freiwilliges Sozialjahr)
# 4                                                                 Karenz
# 5                                                      im Haushalt tätig
# 6                                                             Arbeitslos
KBG_work_2$F4.1[which(KBG_work_2$F4.1==99)] <- NA
KBG_work_2$F4.1 <- factor(KBG_work_2$F4.1, labels = c("unselbststaending","selbststaending", "studierend", "karenz", "haushalt", "arbeitslos")) 
summary(factor(KBG_work_2$F4.1))


KBG_work_2$wiedereinstieg <- rep(NA, nrow(KBG_work_2))
KBG_work_2$wiedereinstieg[which(KBG_work_2$F4.1=="unselbststaending" | KBG_work_2$F4.1=="selbststaending")] <- KBG_work_2$F4.3[which(KBG_work_2$F4.1=="unselbststaending" | KBG_work_2$F4.1=="selbststaending")]

KBG_work_2$wiedereinstieg[which(KBG_work_2$F4.1  %in% c("studierend","karenz", "haushalt", "arbeitslos") &  KBG_work_2$F4.4==1)] <- KBG_work_2$F4.5[which(KBG_work_2$F4.1 %in% c("studierend","karenz", "haushalt", "arbeitslos") &  KBG_work_2$F4.4==1)]
KBG_work_2$wiedereinstieg[which(KBG_work_2$wiedereinstieg== 99)] <- NA

summary(as.factor(KBG_work_2$wiedereinstieg))


#### externe Kinderbetreuung ####
#F5.1- Wird Ihr jüngstes Kind regelmäßig von Tageseltern, in einer Kinderkrippe oder in einem Kindergarten betreut?
#F5.2- Wie alt war Ihr jüngstes Kind, als Sie diese Betreuung erstmals in Anspruch genommen haben? (Alter des Kindes in Monaten:)
#F5.3- Ab welchem Alter möchten Sie für Ihr jüngstes Kind die Betreuung bei Tageseltern, in einer Kinderkrippe oder im Kindergarten in Anspruch nehmen? (Alter des Kindes in Monaten:)

KBG_work_2$betreuungextern <- rep(NA, nrow(KBG_work_2))
KBG_work_2$betreuungextern[which(KBG_work_2$F5.1== 1)] <- KBG_work_2$F5.2[which(KBG_work_2$F5.1==1)] #57x99
KBG_work_2$betreuungextern[which(KBG_work_2$F5.1== 2)] <- KBG_work_2$F5.3[which(KBG_work_2$F5.1==2)] #54x99
KBG_work_2$betreuungextern[which(KBG_work_2$betreuungextern== 99)] <- NA

summary(KBG_work_2$betreuungextern)
summary(as.factor(KBG_work_2$F5.3))
summary(as.factor(KBG_work_2$F5.2))



#### umbenennen aller Variablen von . zu _ ####

#You may need to escape the . (with \\) which is a special character that means "any character"
colnames(KBG_work_2) <- str_replace(colnames(KBG_work_2), "\\.", "_")



write_sav(KBG_work_2, "KBG_work_4.sav")
save.dta13(KBG_work_2, "KBG_work_4.dta", data.label = NULL, time.stamp = TRUE,
           convert.factors = TRUE, convert.dates = TRUE, tz = "GMT",
           add.rownames = FALSE, compress = FALSE, version = 117,
           convert.underscore = FALSE)





