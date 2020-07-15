#### KBG PCA and Cluster analysis hierachical ####
setwd(
  "Y:/common/Projekte/2 Laufende Projekte/Kinderbetreuungsgeldkonto - Evaluierung ab 2018/Quant_BezieherInnen_2019/Daten"
)

################################# This script contains #################################################
# hierachical cluster analysis

# 1. PCA (Varimax rotated & with polychoric correlation matrix)
#    for ordered variables (as in our case) polychoric correlation matrix rather than 'standard'
#    matrix is needed and considered for the PCA in this script.
#    Varimax rotated PCA with 5 factors (eigenvalue > 1)
#    After PCA a visual inspection of the cluster tendency is shown

# [Nebenanalyse] exclude less correlated ones
#    When you run this part of the script only cor > 0.3 is considered for PCA

# 2. Hierarchical cluster analysis
#    Retained PCA factor scores are used as input for hierarchical cluster analysis
#    Algorithm: ward, metric: Euclidean

# 3. Cluster Analysis hierarchical without PCA: selection based on section 1. PCA 
#    use results of 1. to determine which variables load heavy on factors
#    select two highest loading variables for each factor (= set attitude_highload)
#    use attitude_highload as input for hierarchical cluster analysis
#    (if also [Nebenanalyse is sourced] the same analysis can be done for attitude_highloadcor
#     which means that only the high correlated variables can be part of the highest loading variables)

--------------------------------------------------------------------------------------------------------


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


#### Factor Analysis - not needed we do PCA ####
# factor analysis without polychoric matrix
fac <- fa(attitude,
          nfactors = 5,
          rotate = "varimax",
          scores = TRUE)
# factor anlayisis with polychoric matrix
fac_ord <-
  fa(
    attitude,
    nfactors = 5,
    rotate = "varimax",
    scores = TRUE,
    cor = "poly"
  )


#### 1. PCA (Varimax roatated & with polychoric correlation matrix)  ####
## 1.1 calculate the right correlation matrix (pc$correlations )

# for numeric (SPSS result and see also: )
pca <-
  principal(attitude,
            nfactors = 5,
            rotate = "varimax",
            scores = TRUE) # spss result


# for ordered (as in our case) polychoric correlation matix is needed
# pc <-
#   polycor::hetcor(data.frame(as_factor(attitude))) # polychoric correlation

# Using the polychoric function in the psych package, we find the same answer as the hetcor function from polycor if we do not apply the correction for continuity, but somewhat different values if we do correct for continuity. I recommend the correction.
# here I have used a correction of 0.1
# see discussion: https://stackoverflow.com/questions/13818139/what-is-the-meaning-of-the-warning-message-about-logp-when-calculating-a-polyc

pc <-
  psych::polychoric(data.frame((attitude)), correct = .1) # polychoric correlation

pc$correlations <- pc$rho
corrplot(pc$correlations, method = "number")

#### [Nebenanalyse] exclude less correlated ones ####
# attitude <- subset(attitude, select= -c(F5_4_1_f, F5_4_2_f, F3_5_3_f, F3_5_4_f))
# pc <- polycor::hetcor(data.frame(as_factor(attitude))) # polychoric correlation
# pc <-
#  psych::polychoric(data.frame((attitude)), correct = .1) # polychoric correlation
# pc$correlations <- pc$rho

# ---------------------------------------------------------------------------------

## 1.2  PCA analysis
# not possible to just use cor= 'poly' in principal function because error:
# The problem is that the psych functions expect input in very particular format, in this case, if you want to input a contingency matrix, it must be a class table, not a matrix. If you give it matrix input, it doesn't figure out your data is a contingency table and tries to do something else on it that produces nonsense results, and has to be continuity corrected.
# see details: https://stats.stackexchange.com/questions/295833/what-does-correct-for-continuity-mean
  
# solution:
# 1 clculate polycor matrix (see pc)
# 2 use polycor matrix as input for principal component analysis (pc$correlations)

## PCA  
pca_ord <-
  principal(
    pc$correlations,
    nfactors = 5,
    rotate = "varimax",
    scores = TRUE
  )

# factor scores
# if matrix is used as input for principal function, scores need to be calculated seperately (scores)
pca_calc_scores <- psych::factor.scores(attitude, pca_ord)
pca_ord_scores <- pca_calc_scores$scores
colnames(pca_ord_scores) <- c("RC1", "RC2", "RC3", "RC4", "RC5")

# factor loadings
pca_ord$loadings
colnames(pca_ord$loadings) <- c("RC1", "RC2", "RC3", "RC4", "RC5")
# factor values
pca_ord$values # 5 are above 1

pca_ord_scores[1:10, ]
KBG_work_4$Indicator[1:10]

## 1.3 Visual inspection of the data (based on factor scores)

## 1.3.1 Plot factor scores
# does not contain any meaningful clusters
fviz_pca_ind(
  prcomp(pca_ord_scores),
  title = "PCA - Factor scores of categorical factor analysis",
  geom = "point",
  ggtheme = theme_classic()
)

## 1.3.2 compute cluster tendency
# Compute Hopkins statistic dataset
res <-
  get_clust_tendency(pca_ord_scores,
                     n = nrow(pca_ord_scores) - 1,
                     graph = FALSE)
res$hopkins_stat
# It can be seen that the data set is  clusterable (the H value = 0.67 which is  above the threshold 0.5).

# compute dissimilary matrix
# Red: high similarity (ie: low dissimilarity) | Blue: low similarity
dist_ord <- dist(pca_ord_scores, method = "euclidean")
fviz_dist(dist_ord)



#### 2. Hieracical cluster analysis ####

#
# Two different algorithms are found in the literature for Ward clustering. The one used by option "ward.D" (equivalent to the only Ward option "ward" in R versions <= 3.0.3) does not implement Ward's (1963) clustering criterion, whereas option "ward.D2" implements that criterion (Murtagh and Legendre 2014). With the latter, the dissimilarities are squared before cluster updating. Note that agnes(*, method="ward") corresponds to hclust(*, "ward.D2").
#

## 2.1 hierachical cluster anlysis
dist_ord <- dist(pca_ord_scores, method = "euclidean") # metric= euclidean
hclust_ward_ord <- hclust(dist_ord, method = 'ward.D2') # algorithm= ward
cut_ward_ord <- cutree(hclust_ward_ord, k = 3) # select k= number of clusters

## 2.2 Visualize cluster analysis

# Cluster dendrogramm
library(dendextend)
ward_dend_obj <- as.dendrogram(hclust_ward_ord)
ward_col_dend <- color_branches(ward_dend_obj, h = 28)
plot(ward_col_dend)

# We can also view our results by using fviz_cluster. This provides a nice illustration of the clusters. If there are more than two dimensions (variables) fviz_cluster will perform principal component analysis (PCA) and plot the data points according to the first two principal components that explain the majority of the variance.

# plot
fviz_cluster(
  list(data = pca_ord_scores, cluster = cut_ward_ord),
  palette = "jco",
  ggtheme = theme_minimal()
)

## 2.3 Validity of hierachical cluster analysis
test <-
  clValid(
    as.data.frame(pca_ord_scores),
    nClust = 3,
    clMethods = "hierarchical",
    validation = "internal",
    maxitems = 1000,
    metric = "euclidean",
    neighbSize = 10,
    dropEvidence = NULL,
    verbose = FALSE
  )
summary(test)


#### 3. Cluster Analysis hierachical without PCA: selection based on section 1. PCA ####
## 3.1 Input data
pca_ord <-
  principal(
    pc$correlations,
    nfactors = 4,
    rotate = "varimax",
    scores = TRUE
  )
pca_ord$loadings

# Variables which load heavily
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


## 3.2 Visual inspection of the data

## 3.2.1 Plot factor scores

# with polycor
# pc_highload <-
#   psych::polychoric(data.frame((attitude_highload)), correct = .1) # polychoric correlation
# pc_highload$correlations <- pc_highload$rho
# fviz_pca_ind(
#   prcomp(pc_highload$correlations),
#   title = "PCA - Factor scores of categorical factor analysis",
#   geom = "point",
#   ggtheme = theme_classic()
# )
# without polycor
fviz_pca_ind(
  prcomp(attitude_highload),
  title = "PCA - Factor scores of categorical factor analysis",
  geom = "point",
  ggtheme = theme_classic()
)

## 3.2.2 Compute Hopkins statistic
res <-
  get_clust_tendency(attitude_highload,
                     n = nrow(attitude_highload) - 1,
                     graph = FALSE)
res$hopkins_stat
# It can be seen that the data set is not really clusterable.

# 3.2.3 compute dissimilary matrix
# Red: high similarity (ie: low dissimilarity) | Blue: low similarity
dist_ord <- dist(attitude_highload, method = "euclidean")
fviz_dist(dist_ord)


## 3.3 hieracical cluster analysis 
dist_ord <- dist(attitude_highload, method = "euclidean")
hclust_ward_ord <- hclust(dist_ord, method = 'ward.D2')
plot(hclust_ward_ord)
cut_ward_ord <- cutree(hclust_ward_ord, k = 2)

## 3.4 Visualize hierachical cluster analysis
# Cluster dendrogramm
library(dendextend)
ward_dend_obj <- as.dendrogram(hclust_ward_ord)
ward_col_dend <- color_branches(ward_dend_obj, h = 28)
plot(ward_col_dend)

# plot
# We can also view our results by using fviz_cluster. This provides a nice illustration of the clusters. If there are more than two dimensions (variables) fviz_cluster will perform principal component analysis (PCA) and plot the data points according to the first two principal components that explain the majority of the variance.
fviz_cluster(
  list(data = attitude_highload, cluster = cut_ward_ord),
  palette = "jco",
  ggtheme = theme_minimal()
)

## 3.5 Validity of hierachical cluster analysis
test <-
  clValid(
    as.data.frame(pca_ord_scores),
    nClust = 3,
    clMethods = "hierarchical",
    validation = "internal",
    maxitems = 1000,
    metric = "euclidean",
    neighbSize = 10,
    dropEvidence = NULL,
    verbose = FALSE
  )
summary(test)