############################### Here the Model_Logit_respondent_Polychoric models are used to calculate predicted probabilities and the visualization of the results



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

# for comparison purposed the follwing levels of the variables/the means of the variables are used:
# unselbstaendige, studium, mean(Anzahl Kinder), mean(FAC1), FAC2 -3.011454 und 2.058546, mean(FAC3), mean(FAC4),
# waehrendschwagerschaft (Entscheidung welches System), gar nicht relevant (tageweise), keine Väterbeteiligung am KBG

setup_3 <- data.frame(F1_1 = rep("unselbststaending", 1),
                      S02 = rep(mean(KBG_work_4$S02), 1),
                      S07 = rep("studium", 1),
                      S05= rep(mean(KBG_work_4$S05), 2),
                      FAC1_poly_highcor = rep(mean(KBG_work_4$FAC1_poly_highcor), 1),
                      FAC2_poly_highcor = rep(c(-3.011454, 2.058546), 1),
                      FAC3_poly_highcor = rep(mean(KBG_work_4$FAC3_poly_highcor), 1),
                      FAC4_poly_highcor = rep(mean(KBG_work_4$FAC4_poly_highcor), 1),
                      F2_10 = rep("waehrendschwangerschaft", 1),
                      F3_3 = rep("gar nicht relevant", 1),
                      KBGm_beteiligt = rep(0, 1))
Transition_3 <- cbind(setup_3, predict(logit3_poly_highcor, newdata = setup_3, type = "response"))                  
colnames(Transition_3)[12] <- "predicted probability"                    

setup_4.2 <- data.frame(F1_1 = rep("unselbststaending", 1),
                        S02 = rep(mean(KBG_work_4$S02), 1),
                        S07 = rep("studium", 1),
                        S05= rep(mean(KBG_work_4$S05), 2),
                        FAC1_poly_highcor = rep(mean(KBG_work_4$FAC1_poly_highcor), 1),
                        FAC2_poly_highcor = rep(c(-3, 2), 1),
                        FAC3_poly_highcor = rep(mean(KBG_work_4$FAC3_poly_highcor), 1),
                        FAC4_poly_highcor = rep(mean(KBG_work_4$FAC4_poly_highcor), 1),
                        F2_10 = rep("waehrendschwangerschaft", 1),
                        F3_3 = rep("gar nicht relevant", 1),
                        KBGm_beteiligt = rep(0, 1),
                        Nettostundeneinkommen = rep(mean(KBG_work_4[which(!is.na(KBG_work_4$Nettostundeneinkommen)), "Nettostundeneinkommen"]), 1))
Transition_4.2 <- cbind(setup_4.2, predict(logit4.2_poly_highcor, newdata = setup_4.2, type = "response"))                  
colnames(Transition_4.2)[13] <- "predicted probability"      

#### 8. visulize predicted probabilites ####
# see
# https://blogs.uoregon.edu/rclub/2016/04/05/plotting-your-logistic-regression-models/


FAC2_range <- seq(from=min(KBG_work_4$FAC2_poly_highcor), to=max(KBG_work_4$FAC2_poly_highcor), by=.01)
FAC1_range <- seq(from=min(KBG_work_4$FAC1_poly_highcor), to=max(KBG_work_4$FAC1_poly_highcor), by=.01)

# unselbstständige
unselbststaendige_logits <- (logit3_poly_highcor$coef[1] +
                               logit3_poly_highcor$coef[2]*0 + 
                               logit3_poly_highcor$coef[3]*0 + 
                               logit3_poly_highcor$coef[4]*0 + 
                               logit3_poly_highcor$coef[5]*0 + 
                               logit3_poly_highcor$coef[6]*0 + 
                               
                               logit3_poly_highcor$coef[7]* mean(KBG_work_4$S02) + 
                               
                               logit3_poly_highcor$coef[8]*0 + 
                               logit3_poly_highcor$coef[9]*0 + 
                               logit3_poly_highcor$coef[10]*0 + 
                               logit3_poly_highcor$coef[11]*0 + 
                               
                               logit3_poly_highcor$coef[12]*mean(KBG_work_4$S05) +
                               
                               logit3_poly_highcor$coef[13]*mean(KBG_work_4$FAC1_poly_highcor)  +
                               logit3_poly_highcor$coef[14]*FAC2_range +
                               logit3_poly_highcor$coef[15]*mean(KBG_work_4$FAC3_poly_highcor) +
                               logit3_poly_highcor$coef[16]*mean(KBG_work_4$FAC4_poly_highcor) +
                               logit3_poly_highcor$coef[17]*0 +
                               logit3_poly_highcor$coef[18]*0 +
                               logit3_poly_highcor$coef[19]*0 +
                               logit3_poly_highcor$coef[20]*0 +
                               
                               logit3_poly_highcor$coef[21]*0)

# selbstständige
selbststaendige_logits <- (logit3_poly_highcor$coef[1] +
                             logit3_poly_highcor$coef[2]*1 + # only thing changed
                             logit3_poly_highcor$coef[3]*0 + 
                             logit3_poly_highcor$coef[4]*0 + 
                             logit3_poly_highcor$coef[5]*0 + 
                             logit3_poly_highcor$coef[6]*0 + 
                             
                             logit3_poly_highcor$coef[7]* mean(KBG_work_4$S02) + 
                             
                             logit3_poly_highcor$coef[8]*0 + 
                             logit3_poly_highcor$coef[9]*0 + 
                             logit3_poly_highcor$coef[10]*0 + 
                             logit3_poly_highcor$coef[11]*0 + 
                             
                             logit3_poly_highcor$coef[12]*mean(KBG_work_4$S05) +
                             
                             logit3_poly_highcor$coef[13]*mean(KBG_work_4$FAC2_poly_highcor) +
                             logit3_poly_highcor$coef[14]*FAC2_range +
                             logit3_poly_highcor$coef[15]*mean(KBG_work_4$FAC3_poly_highcor) +
                             logit3_poly_highcor$coef[16]*mean(KBG_work_4$FAC4_poly_highcor) +
                             logit3_poly_highcor$coef[17]*0 +
                             logit3_poly_highcor$coef[18]*0 +
                             logit3_poly_highcor$coef[19]*0 +
                             logit3_poly_highcor$coef[20]*0 +
                             
                             logit3_poly_highcor$coef[21]*0)
# Arbeitslos
arbeitslos_logits <- (logit3_poly_highcor$coef[1] +
                        logit3_poly_highcor$coef[2]*0 + 
                        logit3_poly_highcor$coef[3]*0 + 
                        logit3_poly_highcor$coef[4]*0 + 
                        logit3_poly_highcor$coef[5]*0 + 
                        logit3_poly_highcor$coef[6]*1 + 
                        
                        logit3_poly_highcor$coef[7]* mean(KBG_work_4$S02) + 
                        
                        logit3_poly_highcor$coef[8]*0 + 
                        logit3_poly_highcor$coef[9]*0 + 
                        logit3_poly_highcor$coef[10]*0 + 
                        logit3_poly_highcor$coef[11]*0 + 
                        
                        logit3_poly_highcor$coef[12]*mean(KBG_work_4$S05) +
                        
                        logit3_poly_highcor$coef[13]*mean(KBG_work_4$FAC2_poly_highcor)  +
                        logit3_poly_highcor$coef[14]*FAC2_range +
                        logit3_poly_highcor$coef[15]*mean(KBG_work_4$FAC3_poly_highcor) +
                        logit3_poly_highcor$coef[16]*mean(KBG_work_4$FAC4_poly_highcor) +
                        logit3_poly_highcor$coef[17]*0 +
                        logit3_poly_highcor$coef[18]*0 +
                        logit3_poly_highcor$coef[19]*0 +
                        logit3_poly_highcor$coef[20]*0 +
                        
                        logit3_poly_highcor$coef[21]*0)


a_probs <- exp(unselbststaendige_logits)/(1 + exp(unselbststaendige_logits))
b_probs <- exp(selbststaendige_logits)/(1 + exp(selbststaendige_logits))
c_probs <- exp(arbeitslos_logits)/(1 + exp(arbeitslos_logits))


## with a ggplot
library(ggplot2); library(tidyr)
# first you have to get the information into a long dataframe, which is what ggplot likes :)
plot.data <- data.frame(a=a_probs, b=b_probs, c=c_probs, FAC2=FAC2_range)
plot.data <- gather(plot.data, key=group, value=prob, a:c)
head(plot.data)


# # confidence intervals
# status <- c("unselbststaending", "selbststaending", "arbeitslos")
# for( i in 1:length(status)){
# setup_interval <- data.frame(F1_1 = rep(status[i], length(FAC2_range)),
#                       S02 = rep(mean(KBG_work_4$S02), 1),
#                       S07 = rep("studium", 1),
#                       S05= rep(mean(KBG_work_4$S05), 2),
#                       FAC1_poly_highcor = rep(mean(KBG_work_4$FAC1_poly_highcor), length(FAC2_range)),
#                       FAC2_poly_highcor = FAC2_range,
#                       FAC3_poly_highcor = rep(mean(KBG_work_4$FAC3_poly_highcor), length(FAC2_range)),
#                       FAC4_poly_highcor = rep(mean(KBG_work_4$FAC4_poly_highcor), length(FAC2_range)),
#                       F2_10 = rep("waehrendschwangerschaft", length(FAC2_range)),
#                       F3_3 = rep("gar nicht relevant", length(FAC2_range)),
#                       KBGm_beteiligt = rep(0, length(FAC2_range)))
# pp <- predict(logit3_poly_highcor, newdata = setup_interval, type = "response", se.fit=TRUE)
# #linkinv <- family(logit3_poly_highcor)$linkinv 
# 
# setup_interval$pred0 <- pp$fit
# #alpha <- 0.95
# #sc <- abs(qnorm((1-alpha)/2))  ## Normal approx. to likelihood
# 
# critval <- 1.96 
# upr <- pp$fit + (critval * pp$se.fit)
# lwr <- pp$fit - (critval * pp$se.fit)
# fit <- pp$fit
# 
# fit2 <- logit3_poly_highcor$family$linkinv(fit)
# upr2 <- logit3_poly_highcor$family$linkinv(upr)
# lwr2 <- logit3_poly_highcor$family$linkinv(lwr)
# 
# pframe <- (cbind(setup_interval, fit2 ,lwr2, upr2))
#                          
# # pframe <- transform(setup_interval,
# #                     lwr=(pred0-sc*pp$se.fit),
# #                     upr=(pred0+sc*pp$se.fit))
# assign(paste0('pframe_', status[i]), pframe)
# }
# 
# options(OutDec= ",") #Auchtung, global option --> wieder ?ndern!
# #options(digits = 0)
# 
# nrow(pframe_unselbststaending) + nrow(pframe_selbststaending) + nrow(pframe_arbeitslos)
# nrow(plot.data)
# data <- rbind(
# cbind(plot.data[which(plot.data$group=="a"),], pframe_unselbststaending[, 12:15]),
# cbind(plot.data[which(plot.data$group=="b"),], pframe_selbststaending[, 12:15]),
# cbind(plot.data[which(plot.data$group=="c"),], pframe_arbeitslos[, 12:15]))
# 
# #test
# data[which(data$pred0!=data$prob),]


ggplot(plot.data, aes(x=FAC2, y=prob, color=group)) + # asking it to set the color by the variable "group" is what makes it draw three different lines
  geom_line(aes(linetype=geo), linetype='solid', size=1.5) + 
  labs(x="Faktor 1", y="", title="Wahrscheinlichkeit, dass eaKBG gewäht wird nach Erwerbsstatus") +
  scale_color_manual( labels = c("Unselbstständige", "Selbstständige", "Arbeitslose"), values = c("#6975A6", "#F3E96B","#F05837")) +
  #scale_y_continuous(breaks = seq(0.0, 1.0, 0.2), limits = c(0.3,0.8)) +
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
        legend.position = "right") 
