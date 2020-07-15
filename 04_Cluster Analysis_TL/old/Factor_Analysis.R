#### KBG PCA and Clusteranalysis k-means ####
setwd(
  "Y:/common/Projekte/2 Laufende Projekte/Kinderbetreuungsgeldkonto - Evaluierung ab 2018/Quant_BezieherInnen_2019/Daten"
)


################################# This script contains ################################
# k-means cluster analysis

# 1. PCA: (no polichoric matrix was used, for details see:)
#    Varimax rotated PCA with 5 factors (eigenvalue > 1)
# 2. Retained PCA factor scores are used as input for k-means cluster analysis
# 3. Cluster Analysis k-means without PCA: "random selection"
#    select a set of variables that are less correlated (by hand)
# 4. Cluster Analysis k-means without PCA: selection based on section 1. PCA
#    use results of 1. to determine which varaialbes load heavy on factors
#    select two hightest loading variables for each factor (= set attitude_highload)
#    use attitude_highload as input for cluster analysis---------------------------------------------------------------------------------------

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

## load dataset
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


#### 1. PCA (Varimax Rotated) ####
# Varimax Rotated Principal Components

## 1.1 Correlation analysis ##

# each variable needs at least one correlation coefficient with an absolute
# value higher than 0.3, which is the minimum value proposed by Kinnear and Gray
# (1994) as a criterion for inclusion of variables into analysis

# not fulfilled by all variables see e.g. 1_7_6, 5_4_1, 5_4_2
cor(attitude)
corrplot(cor(attitude))

## 1.2 PCA
# prepare data
# attitude <- na.omit(attitude) # listwise deletion of observations, here not needed

# retaining 5 components eigenvalue > 1
fit <-
  principal(attitude,
            nfactors = 5,
            rotate = "varimax",
            scores = TRUE)
fit # print results , RC3 is RC2 and vice versa

colnames(fit$loadings) <- c("RC1", "RC2", "RC3", "RC4", "RC5")
fit$values # 5 are above 1
fit$loadings # loadings

scores <- fit$scores # factor scores --> input for factor analysis
colnames(scores) <- c("RC1", "RC2", "RC3", "RC4", "RC5")
## the higher the score the more this observation goes in this direction der Zustimmung
# sollte man für die Ladungen der Variablen nicht auch die negativen berücksichtigen?
# d.h. z.B. F5_4_5_f läd stark negativ auf RC3

# corrleation matrix of factor scores
corrscores <- rcorr(as.matrix(scores))
round(corrscores$r, digits = 100)
pairs(fit$scores)

#### 2. Cluster Analysis k-means ####

## 2.1 input data
scores <- fit$scores

## 2.2 Determine number of clusters (3 methods)
factoextra::fviz_nbclust(scores, kmeans, method = c("wss"))
factoextra::fviz_nbclust(scores, kmeans, method = c("silhouette"))
factoextra::fviz_nbclust(scores, kmeans, method = c("gap_stat"))

## 2.3 K-Means Cluster Analysis
fit_cluster <- kmeans(scores, 5, nstart = 25) # 5 cluster solution

# get cluster means
aggregate(scores, by = list(fit_cluster$cluster), FUN = mean)

# Visualize clustering
factoextra::fviz_cluster(fit_cluster,
                         data = scores,
                         palette = "jco",
                         ggtheme = theme_minimal())

## 2.4 Validity measures
# details see:
# https://www.sciencedirect.com/science/article/abs/pii/S003132031200338X

install.packages("clValid")
library(clValid)
test <- clValid(
  as.data.frame(scores),
  5
  ,
  clMethods = "kmeans",
  validation = "internal",
  maxitems = 10000000,
  metric = "euclidean",
  neighbSize = 10,
  dropEvidence = NULL,
  verbose = FALSE
)
summary(test)

# connectedness of the clusters, as determined  by  the  k-nearest  neighbors element [0, infinity)
# should be minimized

# Silhouette value measures the degree of confidence in a particular
# clusteringassignment and lies in the interval [-1,1]
# should be maximized

# Dunn Index is the ratio between the smallest distance between observations not in the same cluster to the
# largest intra-cluster distance, element [0,infinty)
# should be maximized.


#### 3. Cluster Analysis k-means without PCA: "random selection" ####

## 3.1 input data
# more or less random selection of variables that are less correlated (because for clustering variables should not be correlated)
attitude_uncor <-
  subset(
    attitude,
    select = c(
      F5_4_1_f,
      F5_4_2_f,
      F3_5_2_f,
      F3_5_3_f,
      F3_5_4_f,
      F3_5_5_f,
      F1_7_1_f,
      F3_5_1_f,
      F5_4_3_f,
      F1_7_2_f
    )
  )

## 3.2 Determine number of clusters
factoextra::fviz_nbclust(attitude_uncor, kmeans, method = c("wss"))
factoextra::fviz_nbclust(attitude_uncor, kmeans, method = c("silhouette"))
factoextra::fviz_nbclust(attitude_uncor, kmeans, method = c("gap_stat"))

## 3.2 K-Means Cluster Analysis
fit_cluster <-
  kmeans(attitude_uncor, 3, nstart = 25) # 5 cluster solution
# get cluster means
aggregate(attitude_uncor,
          by = list(fit_cluster$cluster),
          FUN = mean)

# Visualize Clustering
factoextra::fviz_cluster(fit_cluster,
                         data = attitude_uncor,
                         palette = "jco",
                         ggtheme = theme_minimal())

## 3.3 Validate clustering
install.packages("clValid")
library(clValid)
test <-
  clValid(
    as.data.frame(attitude_uncor),
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

#### 4. Cluster Analysis k-means without PCA: selection based on section 1. PCA ####
## 4.1 Input data
# select only one or two variables that load heavily on factor ##
# fit <- principal(attitude, nfactors=5, rotate="varimax", scores=TRUE)
# fit
# Loadings:
#             RC1    RC2    RC3    RC4    RC5
# F1_7_1_f  0.743               -0.159
# F1_7_2_f               -0.149  0.686
# F1_7_3_f         0.390         0.540
# F1_7_4_f  0.755 -0.163
# F1_7_5_f  0.550         0.342        -0.221
# F1_7_6_f                0.568
# F5_4_1_f -0.166         0.299         0.476
# F5_4_2_f  0.319  0.443  0.196 -0.118  0.127
# F5_4_3_f -0.163  0.570  0.226  0.327  0.108
# F5_4_4_f  0.754               -0.125
# F5_4_5_f               -0.573  0.520
# F5_4_6_f -0.312  0.555  0.145
# F3_5_1_f  0.205         0.753
# F3_5_2_f -0.254                0.551
# F3_5_3_f  0.111                       0.643
# F3_5_4_f -0.123                       0.714
# F3_5_5_f         0.628 -0.354
# F3_5_6_f -0.105  0.639 -0.350
#
# RC1   RC2   RC3   RC4   RC5
# SS loadings    2.406 1.856 1.824 1.534 1.258
# Proportion Var 0.134 0.103 0.101 0.085 0.070
# Cumulative Var 0.134 0.237 0.338 0.423 0.493

# Varialbes which load heavily
# RC 1: F1_7_4_f , F5_4_4_f
# RC 2: F3_5_6_f,  F3_5_5_f
# RC 3: F3_5_1_f,  F5_4_5_f
# RC 4: F1_7_2_f,  F3_5_2_f
# RC 5: F3_5_4_f,  F3_5_3_f

# select highload variables
attitude_highload <-
  subset(
    attitude,
    select = c(
      F1_7_4_f ,
      F5_4_4_f,
      F3_5_6_f,
      F3_5_5_f,
      F3_5_1_f,
      F5_4_5_f,
      F1_7_2_f,
      F3_5_2_f,
      F3_5_4_f,
      F3_5_3_f
    )
  )

## 4.2 Determine number of clusters
factoextra::fviz_nbclust(attitude_highload,
                         kmeans,
                         method = c("silhouette", "wss", "gap_stat"))
factoextra::fviz_nbclust(attitude_highload, kmeans, method = c("gap_stat"))

## 4.3 K-Means Cluster Analysis
fit_cluster <-
  kmeans(attitude_highload, 2, nstart = 25) # 5 cluster solution
# get cluster means
aggregate(attitude_highload,
          by = list(fit_cluster$cluster),
          FUN = mean)

# Visualize use PCA to reduce dimensions
factoextra::fviz_cluster(fit_cluster,
                         data = attitude_highload,
                         palette = "jco",
                         ggtheme = theme_minimal())

## 4.3 Validate clustering
#https://www.sciencedirect.com/science/article/abs/pii/S003132031200338X

test <-
  clValid(
    as.data.frame(attitude_highload),
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







#### Factor analysis with ordered variable - wrong we want PCA ####

# Factor analysis - wrong, we want PCA
library(klaR)
attitude_uncor <- as.data.frame(attitude_uncor)
fit <- kmodes(as_factor(attitude_uncor), 3)