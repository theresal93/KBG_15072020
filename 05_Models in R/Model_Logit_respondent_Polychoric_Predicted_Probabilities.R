############################### Here the Model_Logit_respondent_Polychoric models are used to calculate predicted probabilities and the visualization of the results




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
library(ggplot2)
library(svglite)

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



##### 1. Final models for predicted probabilities ####

# model 3 full model
logit3_poly_highcor <- glm(
  logiteinkommen ~
    F1_1  + S02 + S07 + S05  +
    FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
    F2_10 + F3_3 + KBGm_beteiligt ,# use highcor poly PCA
  data = KBG_work_4,
  family = binomial(link = "logit")
)
logit3mfx_part_poly_highcor <- logitmfx(
  logiteinkommen ~
    F1_1  + S02 + S07 + S05  +
    FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
    F2_10 + F3_3 + KBGm_beteiligt ,
  data = KBG_work_4,
  atmean = FALSE
)
logit3mfx_part_poly_highcor

# model 4.2 Erwerbstätigen model
logit4.2_poly_highcor <- glm(
  logiteinkommen ~
    F1_1  +  S07 + S05    +
    FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
    F2_10 + F3_3 + KBGm_beteiligt +
    Nettostundeneinkommen
  ,
  data = KBG_work_4,
  family = binomial(link = "logit")
)
logit4.2mfx_part_poly_highcor <- logitmfx(
  logiteinkommen ~
    F1_1   + S07 + S05   +
    FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
    F2_10 + F3_3 + KBGm_beteiligt +
    Nettostundeneinkommen
  ,
  data = KBG_work_4,
  atmean = FALSE
)
logit4.2mfx_part_poly_highcor


#### 2. Predicted Probabilities ####
FAC2_range <- seq(from=min(KBG_work_4$FAC2_poly_highcor), to=max(KBG_work_4$FAC2_poly_highcor), by=.01)
FAC1_range <- seq(from=min(KBG_work_4$FAC1_poly_highcor), to=max(KBG_work_4$FAC1_poly_highcor), by=.01)

status <- c("unselbststaending", "selbststaending", "arbeitslos")
for( i in 1:length(status)){
  setup_interval <- data.frame(F1_1 = rep(status[i], length(FAC1_range)),
                               S02 = rep(mean(KBG_work_4$S02), 1),
                               S07 = rep("studium", 1),
                               S05= rep(mean(KBG_work_4$S05), 1),
                               FAC1_poly_highcor = FAC1_range ,
                               FAC2_poly_highcor = rep(mean(KBG_work_4$FAC1_poly_highcor), length(FAC1_range)),
                               FAC3_poly_highcor = rep(mean(KBG_work_4$FAC3_poly_highcor), length(FAC1_range)),
                               FAC4_poly_highcor = rep(mean(KBG_work_4$FAC4_poly_highcor), length(FAC1_range)),
                               F2_10 = rep("waehrendschwangerschaft", length(FAC1_range)),
                               F3_3 = rep("gar nicht relevant", length(FAC1_range)),
                               KBGm_beteiligt = rep(0, length(FAC1_range), 1))
  pp <- predict(logit3_poly_highcor, newdata = setup_interval, type = "link", se.fit=TRUE)
  
  pred <- exp(pp$fit)/(1+exp(pp$fit)) # is equal to th result if we do type = "response" in predict
  upr <- exp(pp$fit+1.96*pp$se.fit)/(1+exp(pp$fit+1.96*pp$se.fit))
  lwr <- exp(pp$fit-1.96*pp$se.fit)/(1+exp(pp$fit-1.96*pp$se.fit))
  intervals <- cbind(lwr, upr)
  pframe <- cbind(setup_interval, pred, intervals)
  
  assign(paste0('pframe_', status[i]), pframe)
  }

head(pframe_unselbststaending$pred)
head(pframe_selbststaending$pred)
head(pframe_arbeitslos$pred)

plot.data <- data.frame(a=pframe_unselbststaending$pred, b=pframe_selbststaending$pred, c=pframe_arbeitslos$pred, FAC1=FAC1_range)
plot.data <- gather(plot.data, key=group, value=prob, a:c)

nrow(pframe_unselbststaending) + nrow(pframe_selbststaending) + nrow(pframe_arbeitslos)
nrow(plot.data)
plot.data <- rbind(
cbind(plot.data[which(plot.data$group=="a"),], pframe_unselbststaending[, 12:ncol(pframe_unselbststaending)]),
cbind(plot.data[which(plot.data$group=="b"),], pframe_selbststaending[, 12:ncol(pframe_selbststaending)]),
cbind(plot.data[which(plot.data$group=="c"),], pframe_arbeitslos[, 12:ncol(pframe_arbeitslos)]))

#test
plot.data[which(plot.data$prob!=plot.data$pred),]
plot.data$prob <- NULL

# subset only a
# plot.data <- plot.data[which(plot.data$group=='a'),]

setwd("Z:/KBG/05_Models in R/Results/Graphs")
options(OutDec= ",") #Auchtung, global option --> wieder ?ndern!
#options(digits = 0)
ggplot(plot.data, aes(x=FAC1, y=pred, color=group)) + # asking it to set the color by the variable "group" is what makes it draw three different lines
  geom_line(aes(linetype=geo), linetype='solid', size=1.5) + 
  labs(x="Faktor 1", y="", title="Wahrscheinlichkeit, dass eaKBG gewäht wird nach Erwerbsstatus") +
  scale_color_manual( labels = c("Unselbstständige", "Selbstständige", "Arbeitslose"), values = c("#6975A6", "#F3E96B","#F05837")) +
  scale_y_continuous(breaks = seq(0.0, 1.0, 0.2), limits = c(0.0,1)) +
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

# Achtung: In plot Dargestellt mit allen 3 wäre die Interaktion zwischen FAC2*Erwerbsstatus. Diese ist nicht siginifikant immer signfifikant, deshalb überlappen sie teilweise. Eventuell wäre es besser, es für alle einzeln darzustellen.


#### Function plot prediced probabilites ####
# eventuell noch machen