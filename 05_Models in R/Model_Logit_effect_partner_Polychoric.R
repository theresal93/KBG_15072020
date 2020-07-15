####################################### Model Polychoric including partner variables ###############################################
# This file contains the logit model 3 and logit model 4.2 of the Model_Logit_respondent_Polychoric.R file.
# Now the partner variables S08 (education), S04 (age), F1_4 (Erwerbssatus vor der Geburt) are added.
# The results from both models show that partner charachteristics do not have an effect on the choice of the model
# The only exception to that is if the partner is was im "Haushalt tätig" before the child was born. In this case the likelihood of 
# choosing the EA KBG decreases.
# Note: The models shown can only be calculated for those who have a partner (because of complete cases)

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

setwd("Z:/KBG")
# import the needed variables
# source polychoric PCA
source("Z:/KBG/03_Factor Analysis (PCA)/PCA_Polychor.R")
# note: also dependency to R_dummies
source("Z:/KBG/02_Prepare Data in R/New_Variables_KBG.R")
rm(F2.7new, F3.2a2new, F3.2b2new, KBG_work_3, test)

# merge pcascores with KBG_work_4
KBG_work_2020 <- cbind(KBG_work_4, pca_ord_full_scores, pca_ord_highcor_scores)
rm(KBG_work_4)

#### 3. logit3: Hard Facts model plus factor scores plus additional variables ####
# highcor poly model
logit3_poly_highcor <- glm(
  logiteinkommen ~
    F1_1 + S01 + S02 + S07 + S05  +
    FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
    F2_10 + F3_3 + KBGm_beteiligt +
    S08 + S04 + F1_4,
  data = KBG_work_2020,
  family = binomial(link = "logit")
)
logit3mfx_part_poly_highcor <- logitmfx(
  logiteinkommen ~
    F1_1 + S01 +  S02 + S07 + S05  +
    FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
    F2_10 + F3_3 + KBGm_beteiligt +
    S08 + S04 + F1_4,
  data = KBG_work_2020,
  atmean = FALSE
)
logit3mfx_part_poly_highcor

#### 4. logit4: Erwerbstätigen Model: Subset model to include income ####
logit4.2_poly_highcor <- glm(
  logiteinkommen ~
    F1_1  + + S07 + S05    +
    FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
    F2_10 + F3_3 + KBGm_beteiligt +
    Nettostundeneinkommen +
    S08 + S04 + F1_4
  ,
  data = KBG_work_2020,
  family = binomial(link = "logit")
)


logit4.2mfx_part_poly_highcor <- logitmfx(
  logiteinkommen ~
    F1_1   + S07 + S05   +
    FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
    F2_10 + F3_3 + KBGm_beteiligt +
    Nettostundeneinkommen +
    S08 + S04 + F1_4
  ,
  data = KBG_work_2020,
  atmean = FALSE
)
logit4.2mfx_part_poly_highcor
