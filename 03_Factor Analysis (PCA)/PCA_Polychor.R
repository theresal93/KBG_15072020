##################### PCA with polychoric correlation matrix ###########################
# in principle this file was copied from parts of PCA and hierachical_cat_and highcor.R
# for more details check out hierachical_cat_and highcor.R

# 1. PCA polychoric with 5 factors - full model
#    for ordered variables (as in our case) polychoric correlation matrix rather than 'standard'
#    matrix is needed and considered for the PCA in this script.
#    Varimax rotated PCA with 5 factors (eigenvalue > 1)
#    All 18 attitude variables are included
#    After PCA a visual inspection of the cluster tendency is shown

# 2. PCA polychoric with 4 factors - with variables correlation > 0.3
#    same as 1. but only variables with correlations in polychoric matrix > 0.3 are included
#    i.e. F5_4_1_f, F5_4_2_f, F3_5_3_f, F3_5_4_f are excluded
#    As a result only 4 factors have eigenvalue > 1. --> PCA is only performed with 4 factors

## load packages
require(nnet)
require(clValid)
library(cluster)
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
library(Hmisc)
library(corrplot)
library(psych)
library(ltm)
library(factoextra)
library(polycor)

setwd(
  "Y:/common/Projekte/2 Laufende Projekte/Kinderbetreuungsgeldkonto - Evaluierung ab 2018/Quant_BezieherInnen_2019/Daten"
)


# load data
KBG_work_4 <- read_sav("KBG_work_4.sav")

# f have reverse order
summary(as_factor(KBG_work_4[, grepl("1_7" , names(KBG_work_4))]))
KBG_work_4[, grepl("1_7_[0-9]_f" , names(KBG_work_4))]
KBG_work_4[, grepl("5_4_[0-9]_f" , names(KBG_work_4))]
KBG_work_4[, grepl("3_5_[0-9]_f" , names(KBG_work_4))]

# main dataset
attitude <-
  cbind(KBG_work_4[, grepl("1_7_[0-9]_f" , names(KBG_work_4))],
        KBG_work_4[, grepl("5_4_[0-9]_f" , names(KBG_work_4))],
        KBG_work_4[, grepl("3_5_[0-9]_f" , names(KBG_work_4))])


summary(as_factor(attitude))


###### 1. PCA polychoric with 5 factors - full model ####

## 1.1 polychoric correlation
pc_full <-
  psych::polychoric(data.frame((attitude)), correct = .1) # polychoric correlation

pc_full$correlations <- pc_full$rho
corrplot(pc_full$correlations, method = "number")

## 1.2  PCA analysis
# not possible to just use cor= 'poly' in principal function because error:
# The problem is that the psych functions expect input in very particular format, in this case, if you want to input a contingency matrix, it must be a class table, not a matrix. If you give it matrix input, it doesn't figure out your data is a contingency table and tries to do something else on it that produces nonsense results, and has to be continuity corrected.
# see details: https://stats.stackexchange.com/questions/295833/what-does-correct-for-continuity-mean


# solution:
# 1 clculate polycor matrix (see pc)
# 2 use polycor matrix as input for principal component analysis (pc$correlations)

## 1.2 PCA  
pca_ord_full <-
  principal(
    pc_full$correlations,
    nfactors = 5,
    rotate = "varimax",
    scores = TRUE
  )

# factor scores
# if matrix is used as input for principal function, scores need to be calculated seperately (scores)
pca_calc_full_scores <- psych::factor.scores(attitude, pca_ord_full)
pca_ord_full_scores <- pca_calc_full_scores$scores
colnames(pca_ord_full_scores) <- c("RC1", "RC2", "RC3", "RC4", "RC5")

# factor loadings
pca_ord_full$loadings
colnames(pca_ord_full$loadings) <- c("RC1", "RC2", "RC3", "RC4", "RC5")
# factor values
pca_ord_full$values # 5 are above 1

pca_ord_full_scores[1:10, ]
KBG_work_4$Indicator[1:10]

## 1.3 Visual inspection of the data (based on factor scores)

## 1.3.1 Plot factor scores
# does not contain any meaningful clusters
fviz_pca_ind(
  prcomp(pca_ord_full_scores),
  title = "PCA - Factor scores of categorical factor analysis (full model)",
  geom = "point",
  ggtheme = theme_classic()
)


#### 2. PCA polychoric with 4 factors - with variables correlation > 0.3  ####

# subset only variables > 0.3 correlation
attitude <- subset(attitude, select= -c(F5_4_1_f, F5_4_2_f, F3_5_3_f, F3_5_4_f))

## 2.1 polychorc matrix
pc_highcor <-
  psych::polychoric(data.frame((attitude)), correct = .1) # polychoric correlation

pc_highcor$correlations <- pc_highcor$rho
corrplot(pc_highcor$correlations, method = "number")


## 2.2  PCA analysis
# not possible to just use cor= 'poly' in principal function because error:
# The problem is that the psych functions expect input in very particular format, in this case, if you want to input a contingency matrix, it must be a class table, not a matrix. If you give it matrix input, it doesn't figure out your data is a contingency table and tries to do something else on it that produces nonsense results, and has to be continuity corrected.
# see details: https://stats.stackexchange.com/questions/295833/what-does-correct-for-continuity-mean


# solution:
# 1 clculate polycor matrix (see pc)
# 2 use polycor matrix as input for principal component analysis (pc$correlations)

##  PCA  
pca_ord_highcor <-
  principal(
    pc_highcor$correlations,
    nfactors = 4,
    rotate = "varimax",
    scores = TRUE
  )

# factor scores
# if matrix is used as input for principal function, scores need to be calculated seperately (scores)
pca_calc_highcor_scores <- psych::factor.scores(attitude, pca_ord_highcor)
pca_ord_highcor_scores <- pca_calc_highcor_scores$scores
colnames(pca_ord_highcor_scores) <- c("RC1", "RC2", "RC3", "RC4")

# factor loadings
pca_ord_highcor$loadings
colnames(pca_ord_highcor$loadings) <- c("RC1", "RC2", "RC3", "RC4")
# factor values
pca_ord_highcor$values # 4 are above 1

pca_ord_highcor_scores[1:10, ]
KBG_work_4$Indicator[1:10]

## 2.3 Visual inspection of the data (based on factor scores)

## 2.3.1 Plot factor scores
# does not contain any meaningful clusters
fviz_pca_ind(
  prcomp(pca_ord_highcor_scores),
  title = "PCA - Factor scores of categorical factor analysis",
  geom = "point",
  ggtheme = theme_classic()
)

## clean everything
rm(attitude, KBG_work_4, pca_calc_full_scores, pca_calc_highcor_scores)

colnames(pca_ord_full_scores) <- c("FAC1_poly_full", "FAC2_poly_full", "FAC3_poly_full", "FAC4_poly_full", "FAC5_poly_full")
colnames(pca_ord_highcor_scores) <- c("FAC1_poly_highcor", "FAC2_poly_highcor", "FAC3_poly_highcor", "FAC4_poly_highcor")
