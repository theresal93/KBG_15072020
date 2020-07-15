############################## Cluster Analysis with hyracical clustering ###################################
setwd("Y:/common/Projekte/2 Laufende Projekte/Kinderbetreuungsgeldkonto - Evaluierung ab 2018/Quant_BezieherInnen_2019/Daten")

#### packages ####
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
library(ggplot2)
library(factoextra)

KBG_work_4 <- read_sav("KBG_work_4.sav")
attitude <- cbind(KBG_work_4[ , grepl( "1_7_[0-9]_f" , names( KBG_work_4 ) )], # hoch stimme sehr zu
                  KBG_work_4[ , grepl( "5_4_[0-9]_f" , names( KBG_work_4 ) )],
                  KBG_work_4[ , grepl( "3_5_[0-9]_f" , names( KBG_work_4 ) )])

attitude_highloadcor<- subset(attitude, select=c(F1_7_4_f , F5_4_4_f, F3_5_5_f,  F3_5_6_f, F3_5_1_f,  F1_7_6_f,
                                                 F1_7_2_f,  F1_7_3_f))
pairs(attitude_highloadcor)

#### Factor analysis ####

#Varimax Rotated Principal Components
# prepare data
attitude <- na.omit(attitude) # listwise deletion of observations
# retaining 5 components
fit <- principal(attitude, nfactors=5, rotate="varimax", scores=TRUE)
fit # print results , RC3 is RC2 and vice versa
colnames(fit$loadings) <- c("RC1", "RC2", "RC3", "RC4", "RC5")
fit$values # 5 are above 1
fit$loadings # loadings
fit$scores # factor scores --> input for factor analysis
colnames(fit$scores) <- c("RC1", "RC2", "RC3", "RC4", "RC5")
## the higher the score the more this observation goes in this direction der Zustimmung
# sollte man für die Ladungen der Variablen nicht auch die negativen berücksichtigen?
# d.h. z.B. F5_4_5_f läd stark negativ auf RC3
attitude_scores <- cbind(attitude, fit$scores)

fit$scores[1:10,]
KBG_work_4$Indicator[1:10]



#### cluster analysis - hieracical #####
# 
# Two different algorithms are found in the literature for Ward clustering. The one used by option "ward.D" (equivalent to the only Ward option "ward" in R versions <= 3.0.3) does not implement Ward's (1963) clustering criterion, whereas option "ward.D2" implements that criterion (Murtagh and Legendre 2014). With the latter, the dissimilarities are squared before cluster updating. Note that agnes(*, method="ward") corresponds to hclust(*, "ward.D2").
# 

scores <- fit$scores

dist_mat <- dist(scores, method = 'euclidean')
hclust_ward <- hclust(dist_mat, method = 'ward.D2')
plot(hclust_ward)
cut_ward <- cutree(hclust_ward, k = 4)

rect.hclust(hclust_ward , k = 4, border = 2:6)
#abline(h = 27, col = 'red')

suppressPackageStartupMessages(library(dendextend))
ward_dend_obj <- as.dendrogram(hclust_ward)
ward_col_dend <- color_branches(avg_dend_obj, h = 27)
plot(ward_col_dend)

attitude_scores_cl <- mutate(attitude_scores, cluster = cut_ward)
count(attitude_score_cl,cluster) # gleiches Ergebnis wie in spss

ggplot(attitude_scores_cl, aes(x=attitude_scores_cl$RC1, y = attitude_scores_cl$RC3, color = factor(cluster))) + geom_point()

# We can also view our results by using fviz_cluster. This provides a nice illustration of the clusters. If there are more than two dimensions (variables) fviz_cluster will perform principal component analysis (PCA) and plot the data points according to the first two principal components that explain the majority of the variance.

fviz_cluster(list(data = scores, cluster = cut_ward),  palette = "jco",
             ggtheme = theme_minimal())

aggregate(attitude_scores_cl, by= list(attitude_scores_cl$cluster), FUN = mean)


library(clValid)
test <- clValid(as.data.frame(scores), 2, clMethods = "hierarchical", validation ="internal", 
                maxitems = 10000000, metric = "euclidean",neighbSize = 10, 
                dropEvidence=NULL, verbose=FALSE)
summary(test)



#### cluster Analyse ohne Faktoranalyse ####


attitude_highloadcor<- subset(attitude, select=c(F1_7_4_f , F5_4_4_f, F3_5_5_f,  F3_5_6_f, F3_5_1_f,  F1_7_6_f,
                                                 F1_7_2_f,  F1_7_3_f))

dist_mat <- dist(attitude_highloadcor, method = 'euclidean')
hclust_ward <- hclust(dist_mat, method = 'ward.D2')
plot(hclust_ward)
cut_ward <- cutree(hclust_ward, k = 2)

suppressPackageStartupMessages(library(dendextend))
ward_dend_obj <- as.dendrogram(hclust_ward)
ward_col_dend <- color_branches(ward_dend_obj, h = 27)
plot(ward_col_dend)

attitudehighloadcor_scores_cl <- mutate(attitude_highloadcor, cluster = cut_ward)
count(attitudehighloadcor_scores_cl,cluster) # gleiches Ergebnis wie in spss

fviz_cluster(list(data = attitude_highloadcor, cluster = cut_ward),  palette = "jco",
             ggtheme = theme_minimal())

ggplot(attitudehighloadcor_scores_cl, aes(x=attitude_scores_cl$RC1, y = attitude_scores_cl$RC4, color = factor(cluster))) + geom_point()

aggregate(attitudehighloadcor_scores_cl, by= list(attitudehighloadcor_scores_cl$cluster), FUN = mean)
