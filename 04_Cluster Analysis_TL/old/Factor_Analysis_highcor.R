#### PCA Anaysis where less correlated are excluded ####
setwd(
  "Y:/common/Projekte/2 Laufende Projekte/Kinderbetreuungsgeldkonto - Evaluierung ab 2018/Quant_BezieherInnen_2019/Daten"
)


################################# This script contains ################################
# k-means cluster analysis

# 1. correlation analysis and data exclusion
#    evaluate which variables have correlation > 0.29
#    retain variables that habe correlation > 0.29 (set highcor)
# 2. PCA with subset of highly correlated data (no polichoric matrix was used, for details see:)
#    Varimax rotated PCA with 4 factors (eigenvalue > 1)
# 3. Retained PCA factor scores are used as input for k-means cluster analysis
# 4. Cluster Analysis k-means without PCA: selection based on section 2. PCA
#    use results of 2. to determine which varaialbes load heavy on factors
#    select two hightest loading variables for each factor (= set attitude_highloadcor)
#    use attitude_highloadcor as input for k-means cluster analysis

---------------------------------------------------------------------------------------


## packages
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
library(Hmisc)
library(corrplot)
library(psych)
library(clValid)

#### 1. correlation analysis and data exclusion ####

KBG_work_4 <- read_sav("KBG_work_4.sav")
# dataset
attitude <-
  cbind(KBG_work_4[, grepl("1_7_[0-9]_f" , names(KBG_work_4))],
        KBG_work_4[, grepl("5_4_[0-9]_f" , names(KBG_work_4))],
        KBG_work_4[, grepl("3_5_[0-9]_f" , names(KBG_work_4))])

## correlation analysis
# each variable needs at least one correlation coefficient with an absolute
# value higher than 0.3, which is the minimum value proposed by Kinnear and Gray
# (1994) as a criterion for inclusion of variables into analysis

attitude.rcorr <- rcorr(as.matrix(attitude))
attitude.rcorr
attitude.coeff <- round(attitude.rcorr$r, digits = 2)
attitude.p <- attitude.rcorr$P

cor(attitude)
corrplot(cor(attitude))

## data exclusion
# first exclude those with correlation smaller than 0.29
lesscor <-
  attitude[, c("F5_4_1_f", "F5_4_2_f", "F3_5_2_f" , "F3_5_3_f", "F3_5_4_f")]
#,
#F1_7_6_f, F1_7_2_f,  F5_4_5_f, F3_5_5_f # lessless cor

## main dataset
attitude_highcor <-
  subset(attitude,
         select = -c(F5_4_1_f, F5_4_2_f, F3_5_2_f, F3_5_3_f, F3_5_4_f))


#### 2. PCA with subset of highly correlated data ####

## retaining 4 components eigenvalue > 1
fit_highcor <-
  principal(
    attitude_highcor,
    nfactors = 4,
    rotate = "varimax",
    scores = TRUE
  )

fit_highcor # print results , RC3 is RC2 and vic
colnames(fit_highcor$loadings)[1:3] <- c("RC1", "RC2", "RC3")
fit_highcor$values # 4 are above 1
fit_highcor$loadings # loadings

scores_highcor <-
  fit_highcor$scores # factor scores --> input for factor analysis
colnames(scores_highcor)[1:3] <- c("RC1", "RC2", "RC3")

#### 3. Cluster Analysis k-means ####
# Prepare Data
# scores_stand <- scale(fit_highcor) # standardize variables
# is not needed in our cases because factors have same scales

## 3.1 input data
scores_highcor <- fit_highcor$scores

## 3.2 Determine number of clusters
factoextra::fviz_nbclust(scores_highcor,
                         kmeans,
                         method = c("silhouette", "wss", "gap_stat"))
factoextra::fviz_nbclust(scores_highcor, kmeans, method = c("gap_stat"))

## 3.3 Cluser analysis
# 2 cluster solution
fit_cluster <- kmeans(scores_highcor, 2, nstart = 25)

# get cluster means
aggregate(scores_highcor,
          by = list(fit_cluster$cluster),
          FUN = mean)

# Visualize cluster analysis
factoextra::fviz_cluster(fit_cluster,
                         data = scores_highcor,
                         palette = "jco",
                         ggtheme = theme_minimal())


## 3.4 Validate Cluster analysis
# https://www.sciencedirect.com/science/article/abs/pii/S003132031200338X

test <-
  clValid(
    as.data.frame(scores_highcor),
    2,
    clMethods = "kmeans",
    validation = "internal",
    maxitems = 10000000,
    metric = "euclidean",
    neighbSize = 10,
    dropEvidence = NULL,
    verbose = FALSE
  )
summary(test)

#### 4. Cluster Analysis k-means without PCA: selection based on section 2. PCA subset of highly correlated data ####
## 4.1 input data
# fit_highcor <- principal(attitude_highcor, nfactors=4, rotate="varimax", scores=TRUE)
# fit_highcor
# Loadings:
#            RC1    RC2    RC3    RC4
# F1_7_1_f  0.770               -0.112
# F1_7_2_f -0.109 -0.190 -0.220  0.683
# F1_7_3_f         0.280         0.675
# F1_7_4_f  0.784 -0.153
# F1_7_5_f  0.551 -0.119  0.276 -0.149
# F1_7_6_f                0.623
# F5_4_3_f -0.154  0.484  0.297  0.459
# F5_4_4_f  0.776
# F5_4_5_f               -0.606  0.487
# F5_4_6_f -0.280  0.510  0.208  0.190
# F3_5_1_f  0.223 -0.182  0.741
# F3_5_5_f         0.728 -0.253
# F3_5_6_f         0.719 -0.235
#
# RC1   RC2   RC3   RC4
# SS loadings    2.303 1.735 1.692 1.451
# Proportion Var 0.177 0.133 0.130 0.112
# Cumulative Var 0.177 0.311 0.441 0.552

# RC 1: F1_7_4_f , F5_4_4_f
# RC 2: F3_5_5_f,  F3_5_6_f
# RC 3: F3_5_1_f,  F1_7_6_f
# RC 4: F1_7_2_f,  F1_7_3_f

## select highload variables
attitude_highloadcor <-
  subset(
    attitude,
    select = c(
      F1_7_4_f ,
      F5_4_4_f,
      F3_5_5_f,
      F3_5_6_f,
      F3_5_1_f,
      F1_7_6_f,
      F1_7_2_f,
      F1_7_3_f
    )
  )



## 4.2 Determine number of clusters
factoextra::fviz_nbclust(attitude_highloadcor,
                         kmeans,
                         method = c("silhouette", "wss", "gap_stat"))
factoextra::fviz_nbclust(attitude_highloadcor, kmeans, method = c("gap_stat"))

## 4.3 k-means cluster analysis
fit_cluster <-
  kmeans(attitude_highloadcor, 2, nstart = 25) # 2 cluster solution
# get cluster means
aggregate(attitude_highloadcor,
          by = list(fit_cluster$cluster),
          FUN = mean)

# Visualize use PCA to reduce dimensions
factoextra::fviz_cluster(fit_cluster,
                         data = attitude_highloadcor,
                         palette = "jco",
                         ggtheme = theme_minimal())

## 4.4 validation of k-means cluster analysis
#https://www.sciencedirect.com/science/article/abs/pii/S003132031200338X

library(clValid)
test <-
  clValid(
    as.data.frame(attitude_highloadcor),
    2,
    clMethods = "kmeans",
    validation = "internal",
    maxitems = 10000000,
    metric = "euclidean",
    neighbSize = 10,
    dropEvidence = NULL,
    verbose = FALSE
  )
summary(test)
