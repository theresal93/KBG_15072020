
library(haven)
library(dplyr)  
library(descr)

KBG_work_1 <- read_sav("Y:/common/Projekte/2 Laufende Projekte/Kinderbetreuungsgeldkonto - Evaluierung ab 2018/Quant_BezieherInnen_2019/Daten/KBG_work_1.sav")
View(KBG_work_1)
colnames(KBG_work_1)


# Zuschreiben von Faktor zu Person

KBG_work_1$Indicator <- matrix(NA,nrow(KBG_work_1))
x <- matrix(NA, nrow(KBG_work_1))

for (i in 1:nrow(KBG_work_1)){
  
 #min da bei uns so kodiert, dass stimme sehr zu =1, dh je geringer umso eher strebt man dorthin
 x[i] <- which(KBG_work_1[i,]==(min(KBG_work_1[i, "FAC1_1"], KBG_work_1[i, "FAC2_1"], KBG_work_1[i, "FAC3_1"], KBG_work_1[i, "FAC4_1"], KBG_work_1[i, "FAC5_1"])), arr.ind=TRUE)[,2]

 KBG_work_1$Indicator[i] <- ifelse(x[i]== 116, 1, 
                                   ifelse(x[i] == 117, 2,
                                   ifelse(x[i]==118, 3, 
                                   ifelse(x[i]==119, 4,
                                   ifelse(x[i]==120,5, NA )))))
 
}


#write_sav(KBG_work_1, "KBG_work_1_TL.sav")


# # Test ob gleich, wenn statt min, FAC *(-1) und max
# KBG_work_1$FAC1_1_rev <- KBG_work_1$FAC1_1 * (-1)
# KBG_work_1$FAC2_1_rev <- KBG_work_1$FAC2_1 * (-1)
# KBG_work_1$FAC3_1_rev <- KBG_work_1$FAC3_1 * (-1)
# KBG_work_1$FAC4_1_rev <- KBG_work_1$FAC4_1 * (-1)
# KBG_work_1$FAC5_1_rev <- KBG_work_1$FAC5_1 * (-1)
# 
# 
# KBG_work_1$Indicator_rev <- matrix(NA,nrow(KBG_work_1))
# x <- matrix(NA, nrow(KBG_work_1))
# 
# for (i in 1:nrow(KBG_work_1)){
#    
#    #min da bei uns so kodiert, dass stimme sehr zu =1, dh je geringer umso eher strebt man dorthin
#    x[i] <- which(KBG_work_1[i,]==(max(KBG_work_1[i, "FAC1_1_rev"], KBG_work_1[i, "FAC2_1_rev"], KBG_work_1[i, "FAC3_1_rev"], KBG_work_1[i, "FAC4_1_rev"], KBG_work_1[i, "FAC5_1_rev"])), arr.ind=TRUE)[,2]
#    
#    KBG_work_1$Indicator_rev[i] <- ifelse(x[i]== 123, 1, 
#                                      ifelse(x[i] == 124, 2,
#                                             ifelse(x[i]==125, 3, 
#                                                    ifelse(x[i]==126, 4,
#                                                           ifelse(x[i]==127,5, NA )))))
#    
# }
# 
# #summary(as.factor(KBG_work_1$Indicator_rev))
# 
# #kein Unterschied, passt

# Aufteilung Indikator 
summary(as.factor(KBG_work_1$Indicator_rev))
#1   2   3   4   5 
#201 205 229 161 204 

# Anzahl Männer und Frauen
# Frauen=1: 905
# Männer=2:  95
summary(as.factor(KBG_work_1$S01))

# subsample nur Männer nur Frauen

# Frauen
KBG_work_f<- subset(KBG_work_1, KBG_work_1$S01==1)
# Männer
KBG_work_m<- subset(KBG_work_1, KBG_work_1$S01==2)

# Anteil Frauen, in Indikator
summary(as.factor(KBG_work_f$Indicator))/905

# Anteil Männer in Indikator
summary(as.factor(KBG_work_m$Indicator))/95



# Aufteilung Indikator nach Variante: F2.2
KBG_work_1$F2.2 <- as.factor(KBG_work_1$F2.2)

source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(KBG_work_1, row.vars = "Indicator", col.vars = "F2.2", type = "f")

CrossTable(KBG_work_1$F2.1 , as.factor(KBG_work_1$Indicator))

gKBG_work_1 <- group_by(KBG_work_1, KBG_work_1$Indicator)
summarize(gKBG_work_1, F2.1)







summary(as.factor(KBG_work_1$Indicator))

group_by(KBG_work_1, KBG_work_1$Indicator)

######## Berechnung Berechtigung Familienzeitbonus ##########

KBG_work_2 <- read_sav("Y:/common/Projekte/2 Laufende Projekte/Kinderbetreuungsgeldkonto - Evaluierung ab 2018/Quant_BezieherInnen_2019/Daten/KBG_work_2.sav")
View(KBG_work_2)
colnames(KBG_work_2)


# wenn KBGm/(KBGw+KBGw) >= 0.4
# neue Variable KBGm/(KBGw+KBGw)


KBG_work_2$bonus <- ifelse((KBG_work_2$KBGm/(KBG_work_2$KBGw+KBG_work_2$KBGm) >= 0.4 & 
                               KBG_work_2$KBGw/(KBG_work_2$KBGw+KBG_work_2$KBGm) >= 0.4) 
                           &  KBG_work_2$KBGw >= 4 & KBG_work_2$KBGm >= 4, 1,0)

summary(as.factor(KBG_work_2$bonus))
summary(as.factor(KBG_work_2$F2.7))

# Nach meiner Definition des Partnerschaftsbonus sollten 15 einen bekommen, 
# es geben aber 55 Personen an, dass sie Partnerschaftsbonus beziehen.
# 3 davon weil die in Dauer der Monate korrigiert wurden.
# Auch 4 Monatskriterium ändert daran nichts

#Diejenigen, die sagen, dass sie Partnerschaftsbonus beziehen, obwohl sie nicht berechtigt sind
#43 Personen
View(KBG_work_2[which(KBG_work_2$bonus==0 & KBG_work_2$F2.7==1), c("KBGw", "KBGm", "F2.1", "F2.4","F2.11", "F2.7", "F2.4", "F2.5", "falsch1", "falsch2")])
# von den 43 Personen bezogen 28 Familienzeitbonus
View(KBG_work_2[which(KBG_work_2$bonus==0 & KBG_work_2$F2.7==1 & KBG_work_2$F2.11==1), c("KBGw", "KBGm", "F2.1", "F2.4","F2.11", "F2.7", "F2.4", "F2.5", "falsch1", "falsch2")])


View(KBG_work_2[which(KBG_work_2$bonus==0 & KBG_work_2$F2.7==1 & KBG_work_2$F2.11!=1), c("KBGw", "KBGm", "F2.1", "F2.4","F2.11", "F2.7", "F2.4", "F2.5", "falsch1", "falsch2")])

## Vgl. mit jenen, die aktuell beziehen?? F2.1, S06 wann geboren##
View(KBG_work_2[which(KBG_work_2$bonus==0 & KBG_work_2$F2.7==1 & KBG_work_2$F2.1==1), c("KBGw", "KBGm", "F2.1", "S06", "F2.4","F2.11", "F2.7", "F2.4", "F2.5", "falsch1", "falsch2")])
# von diesen 43 Paaren beziehen 34 aktuell KBG


# 11, die in meiner Berechnung berechtigt sind, geben auch an, dass sie Partnerschaftsbonus beziehen.
KBG_work_2[which(KBG_work_2$bonus==1 & KBG_work_2$F2.7==1), c("KBGw", "KBGm", "F2.7", "F2.4", "F2.5", "falsch1", "falsch2")]
# von den 4 übrigen kennt 1 Paar diese Leistung nicht, 1x weiß nicht, und 4x Aufteilung passt nicht
# alle 4 beziehen aktuell noch
KBG_work_2[which(KBG_work_2$bonus==1 & KBG_work_2$F2.7!=1), c("KBGw", "KBGm", "F2.7", "F2.1", "F2.4", "F2.5", "falsch1", "falsch2")]




View(KBG_work_2[, c("KBGw", "KBGm", "S06", "F2.1")])
######### Analyse Dauer Karenz #########


KBG_work_2plus <- read_sav("Z:/KBG/KBG_work_2plus.sav")
colnames(KBG_work_2plus)

#Analyse Konto max ein Jahr

# Meine Berechnung länger, sagen selbst weniger
#49 ( kann eig nicht als gleich interpretiert werden hauptsächlich EK abh.)
längergleich <- KBG_work_2plus[which(KBG_work_2plus$F2.14weigene==1 & KBG_work_2plus$F2.14w==2 ),
                               c("KBGw", "wiedereinstieg", "variante")]
# 16 (2 und 4)
längerkürzer <- KBG_work_2plus[which(KBG_work_2plus$F2.14weigene==1 & KBG_work_2plus$F2.14w==3 ),
                               c("KBGw", "wiedereinstieg",  "variante")]
# 57 (1/2 davon kann als länger interpretiert werden)
gleichlänger <- KBG_work_2plus[which(KBG_work_2plus$F2.14weigene==2 & KBG_work_2plus$F2.14w==1 ),
                               c("KBGw", "wiedereinstieg", "variante")]
#39 (1/3 davon kann als kürzer interpretiert werden - haupts 2 und 3)
gleichkürzer <- KBG_work_2plus[which(KBG_work_2plus$F2.14weigene==2 & KBG_work_2plus$F2.14w==3 ),
                               c("KBGw", "wiedereinstieg", "variante")]
#9
kürzerlänger <- KBG_work_2plus[which(KBG_work_2plus$F2.14weigene==3 & KBG_work_2plus$F2.14w==1 ),
                               c("KBGw", "wiedereinstieg", "variante")]

#50 (kann eigentlich nicht als gleich interpretiert werden - hautsächlich 2)
kürzergleich <- KBG_work_2plus[which(KBG_work_2plus$F2.14weigene==3 & KBG_work_2plus$F2.14w==2 ),
                               c("KBGw", "wiedereinstieg", "variante")]

# 174+105+25=304
gleichgleich <- KBG_work_2plus[which(KBG_work_2plus$F2.14weigene==2 & KBG_work_2plus$F2.14w==2 ), c("KBGw", "wiedereinstieg", "notmissinginF2.14wandF2.14weigene")]
längerlänger <- KBG_work_2plus[which(KBG_work_2plus$F2.14weigene==1 & KBG_work_2plus$F2.14w==1 ), c("KBGw", "wiedereinstieg", "notmissinginF2.14wandF2.14weigene")]
kürzerkürzer <- KBG_work_2plus[which(KBG_work_2plus$F2.14weigene==3 & KBG_work_2plus$F2.14w==3 ), c("KBGw", "wiedereinstieg", "notmissinginF2.14wandF2.14weigene")]

# 304+50+9+39+57+16+49=524
# 100+90+10+10+16= 226 können nicht anders interpretiert werden

# in ca. 2/3 der Fälle sagen gleich obwohl eigentlich eindeutig länger



#### Test ob obwohl aktueller Bezug angegeben eigentlich keiner mehr vorhanden sein sollte ####
install.packages("lubridate")
require(lubridate)
d <- as.Date(KBG_work_2$S06)
month(d) <- month(d) + (KBG_work_2$KBGw+KBG_work_2$KBGm)
KBG_work_2$enddate <- as.Date( paste( year(d), month(d) , day(d) , sep = "-" )  , format = "%Y-%m-%d" )
KBG_work_2[c(1:10), c("KBGw", "KBGm", "S06", "F2.1", "enddate")]

View(KBG_work_2[which(KBG_work_2$F2.1==1 & KBG_work_2$enddate < as.Date("2019-06-01")),
           c("KBGw", "KBGm", "S06", "F2.1", "enddate")])
     
