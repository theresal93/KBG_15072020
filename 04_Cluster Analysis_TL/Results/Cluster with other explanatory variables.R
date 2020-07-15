#### KBG Cluster Analysis with single factor and additonal variables ####
setwd(
  "Y:/common/Projekte/2 Laufende Projekte/Kinderbetreuungsgeldkonto - Evaluierung ab 2018/Quant_BezieherInnen_2019/Daten"
)


source("Z:/KBG/02_Prepare Data in R/R_dummies_KBG.R")
source("Z:/KBG/05_Models in R/Models_KBG.R")


library(cluster)
## load packages
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
library(ltm)
library(factoextra)
library(polycor)
library(Rtsne)

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

#### 1. PCA (Varimax roatated & with polychoric correlation matrix)  ####
## 1.1 calculate the right correlation matrix (pc$correlations )


# for ordered (as in our case) polychoric correlation matix is needed
pc <-
  psych::polychoric(data.frame((attitude)), correct = .1) # polychoric correlation

pc$correlations <- pc$rho
corrplot(pc$correlations, method = "number")

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

# cbind Data
data <- KBG_work_3[which(KBG_work_3$S01==1),]
  
data <- cbind(pca_ord_scores[,1], KBG_work_3$F2_4, KBG_work_3$S01)
colnames(data)[1:3] <- c("RC1", "F2_4", "S01")

data <- cbind(pca_ord_scores[,1], KBG_work_3$F2_4, KBG_work_3$S02)
colnames(data)[1:3] <- c("RC1", "F2_4", "S02")
data <- as.data.frame(data)
#data$S01 <- factor(data$S01)
data <- data[which(complete.cases(data)),]
You can clearly see that our data is mixed with both numerical and factor variables. Therefore, the first thing we must do is calculate the gower coefficient for the dataset. This is done with the “daisy” function from the “cluster” package.
https://dpmartin42.github.io/posts/r/cluster-mixed-types




#
# Two different algorithms are found in the literature for Ward clustering. The one used by option "ward.D" (equivalent to the only Ward option "ward" in R versions <= 3.0.3) does not implement Ward's (1963) clustering criterion, whereas option "ward.D2" implements that criterion (Murtagh and Legendre 2014). With the latter, the dissimilarities are squared before cluster updating. Note that agnes(*, method="ward") corresponds to hclust(*, "ward.D2").
#

## 2.1 hierachical cluster anlysis
dist_ord <- dist(data, method = "euclidean") # metric= euclidean
hclust_ward_ord <- hclust(dist_ord, method = 'ward.D2') # algorithm= ward
cut_ward_ord <- cutree(hclust_ward_ord, k = 2) # select k= number of clusters

## 2.2 Visualize cluster analysis

# Cluster dendrogramm
library(dendextend)
ward_dend_obj <- as.dendrogram(hclust_ward_ord)
ward_col_dend <- color_branches(ward_dend_obj, h = 28)
plot(ward_col_dend)

# We can also view our results by using fviz_cluster. This provides a nice illustration of the clusters. If there are more than two dimensions (variables) fviz_cluster will perform principal component analysis (PCA) and plot the data points according to the first two principal components that explain the majority of the variance.

# plot
fviz_cluster(
  list(data = data, cluster = cut_ward_ord),
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



#### Clusterng mixed data ####
gower_dist <- daisy(data,
                    metric = "gower")
summary(gower_dist) # correct
 # I = interval, N = nominal

gower_mat <- as.matrix(gower_dist)

# Output most similar pair

data[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# Output most dissimilar pair

data[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# Calculate silhouette width for many k using PAM

sil_width <- c(NA)

for(i in 2:10){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot sihouette width (higher is better)

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)


pam_fit <- pam(gower_dist, diss = TRUE, k = 2)

pam_results <- data %>%
  dplyr::select(1:3) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary

data[pam_fit$medoids, ]


tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

plot(data$RC1, data$F2_4)














