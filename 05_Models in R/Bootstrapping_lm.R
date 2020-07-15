####################### Bootstrapping the linear model ####################


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



# source bootstrapping regression: https://socialsciences.mcmaster.ca/jfox/Books/Companion/appendices/Appendix-Bootstrapping.pdf


#### 1. lm6: Hard Facts model plus factor scores plus additional variables plus F3_4 plus Bundesland ####
index <- which( colnames(KBG_work_2020.konto) %in% c("f2_4kor", "F1_1", "S01", "S07",
                                                     "FAC1_poly_highcor", "FAC2_poly_highcor",  "FAC3_poly_highcor",  "FAC4_poly_highcor",
                                                     "KBGm_beteiligt", "F3_4_2", "F3_4_5", "S09"))
# for boot dataset without NA's is needed
KBG_work_2020.konto_lm6_na.omit <- na.omit(KBG_work_2020.konto[, index])


lm6 <- lm(
  f2_4kor ~ F1_1 + S01  + S07 +
    FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
    + F3_4_2  + F3_4_5 +
    S09+ KBGm_beteiligt  , 
  data = KBG_work_2020.konto_lm6_na.omit
)
summary(lm6)



# old boostrapping 
# bs <- function(formula, data, indices){
#                        data <- data[indices,] # select obs. in bootstrap sample 
#                        mod <- lm(formula , data = data)
#                        return(coef(mod)) # return coefficient vector
# }
# 
# library(boot)
# boot <- boot(data = KBG_work_2020.konto_lm6_na.omit, statistic = bs, R = 1000, formula= f2_4kor ~ F1_1 + S01  + S07 +
#                FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
#                KBGm_beteiligt+ F3_4_2  + F3_4_5 +
#                S09)
# 
# boot
# plot(boot, index=12) # Factor 1
# plot(boot, index=13) # Factor 1
# plot(boot, index=14) # Factor 1
# plot(boot, index=15) # Factor 1
# 
# # get 95% confidence intervals
# boot.ci(boot, type="bca", index=12) 
# boot.ci(boot, type="bca", index=13) 
# boot.ci(boot, type="bca", index=14) 
# boot.ci(boot, type="bca", index=15) 
# 
# duncan.array <- boot.array(boot)
# duncan.array[1:2,]
# dim(duncan.array)
# 
# plot(boot, index=15)
# jack.after.boot(boot, index=12, main="(a) Faktor 1 Koeffizient")
# jack.after.boot(boot, index=13, main="(b) Faktor 2 Koeffizient")
# jack.after.boot(boot, index=14, main="(c) Faktor 3 Koeffizient")
# jack.after.boot(boot, index=15, main="(d) Faktor 4 Koeffizient")


#### Bootstrapping ####
library(car)
boot <- Boot(lm6, R=1000, method=c("case"))
summary(boot, high.moments=TRUE) # The  bootstrap  estimates  of  skewness  and  kurtosis  are  included  in  the  output  by  the  argumenthigh.moments=TRUE;

Confint(boot, level=.95, type="bca")
hist(boot, legend="separate")


# There is a separate histogram for each bootstrapped quantity, here each coefficient.  In addition to the histograms we also get kernel density estimates and the normal density based on the bootstrapmean and standard deviation.  The vertical dashed line marks the original point-estimate, and the thick horizontal line gives a confidence interval based on the bootstrap.  Whereas the two density estimates for the intercept are similar, the normal approximation is poor for the other coefficients,and confidence intervals are not close to symmetric about the original values.  This suggests that inference from the bootstrap is different from the asymptotic theory, and that the bootstrap is likelyto  be  more  accurate  in  this  small  sample.  

