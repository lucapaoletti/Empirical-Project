####LIBRARY####
library(ggplot2)
library(tree)
library(ISLR)
library(gbm)
library(JOUSBoost)
library(maboost)
library(rpart)
library(nnet)
library(tidyverse)
library(caret)
library(tibble)
library(MASS)
library(class)
library(maps)
library(cluster)
library(factoextra)
library(ggpubr)
library(patchwork)
library(ggfortify)
####################
####UNSUPERVISED####
####################

####DATA PREPROCESSING####
#import
data = read.csv("C:/Users/Luca/Desktop/UNI/STATALE/LEZIONI/SL/CountryData/Country-data.csv", 
                header = TRUE)
head(data)
summary(data)
nrow(data)
data$country[data$country == 'United States'] = 'USA'
data$country[data$country == 'United Kingdom'] = 'UK'
#data$country[data$country == 'United Kingdom'] = 

#unique(data$country)

b1 = ggplot(data, aes(x = '', y = exports))+
  geom_boxplot(outlier.colour="red") +
  xlab('') +
  ylab('Exports')

b2 = ggplot(data, aes(x = '', y = imports))+
  geom_boxplot(outlier.colour="red") +
  xlab('') +
  ylab('Imports')

b3 = ggplot(data, aes(x = '', y = income))+
  geom_boxplot(outlier.colour="red") +
  xlab('') +
  ylab('Income')

b4 = ggplot(data, aes(x = '', y = inflation))+
  geom_boxplot(outlier.colour="red") +
  xlab('') +
  ylab('Inflation')


b5 = ggplot(data, aes(x = '', y = gdpp))+
  geom_boxplot(outlier.colour="red") +
  xlab('') +
  ylab('GDPP')

(b1|b2)/(b3|b4|b5)


#health plot
h = ggplot(data, aes(x = child_mort, y = life_expec)) +
  geom_point()

h1 = ggplot(data, aes(x = child_mort, y = total_fer)) +
  geom_point()

h2 = ggplot(data, aes(x = total_fer, y = gdpp)) +
  geom_point()

h|(h1/h2)
#economic feature
data %>% arrange(desc(gdpp)) %>% 
  top_n(10) %>%
  ggplot(aes(x = country, y = gdpp)) +
  geom_bar(stat = 'identity') +
  coord_flip()


#correlation
corrplot::corrplot(cor(data[, -1]), method = 'number', type = 'lower')
#check correlation between child_mort and life_expec
ggplot(data, aes(x = child_mort, y = life_expec)) +
  geom_point()

#check correlation between child_mort and life_expec
ggplot(data, aes(x = child_mort, y = total_fer)) +
  geom_point()

#remove child_mort because hig correlation with life_expec and total_fer
corrplot::corrplot(cor(data[, -c(1, 2)]), method = 'number', type = 'lower')

#check high correlation between income and gdpp
ggplot(data, aes(x = gdpp, y = income)) +
  geom_point()

#remove income because high correlated with gdpp (gddp more improtant imo with repsect to income)
corrplot::corrplot(cor(data[, -c(1, 2, 6)]), method = 'number', type = 'lower')

#check high correlation bwteen life_expec and total_fer
ggplot(data, aes(x = life_expec, y = total_fer)) +
  geom_point()
#try to keep them
#refine general dataset
country = data$country
data_scale = scale(data[, -c(1, 2, 6)])
data = data[, -c(1, 2, 6)]
colnames(data)

####clustering####
rownames(data_scale) = country
rownames(data) = country
# total within sum of square method
k = c(2:10)
WSS = c()
for (x in k) {
  kmodel = kmeans(data_scale, centers = x, nstart = 1, iter.max = 100)
  wss = kmodel$tot.withinss
  WSS = append(WSS, wss)
}
#?kmeans
k_df = data.frame('K' = k, 'TWSS' = WSS)
ggplot(k_df, aes(x = K, y = TWSS)) +
  geom_point() +
  geom_line()


#K = 3
kmodel = kmeans(data_scale, centers = 3, nstart = 1, iter.max = 100)
table(kmodel$cluster)

aggregate(data, by=list(cluster=kmodel$cluster), mean)

data_cluster = cbind(data, cluster = kmodel$cluster)


map = map_data('world')


thismap <- mutate(map, fill = ifelse(region %in% row.names(subset(data_cluster, subset = cluster == 1)), 'Cluster 1', 
                                     ifelse(region %in% row.names(subset(data_cluster, subset = cluster == 2)), 'Cluster 2', 
                                            ifelse(region %in% row.names(subset(data_cluster, subset = cluster == 3)), 'Cluster 3', 
                                                   'Note Assigned'))))
unique(thismap$region)

map3 = ggplot(thismap, aes(long, lat, fill = fill, group=group)) + 
  geom_polygon(colour="white") + 
  ggtitle("Map of World with K = 3") +
  guides(fill=guide_legend(title="Clusters"))

#K = 4
kmodel = kmeans(data_scale, centers = 4, nstart = 1, iter.max = 100)
table(kmodel$cluster)

aggregate(data, by=list(cluster=kmodel$cluster), mean)

data_cluster = cbind(data, cluster = kmodel$cluster)


map = map_data('world')


thismap <- mutate(map, fill = ifelse(region %in% row.names(subset(data_cluster, subset = cluster == 1)), 'Cluster 1', 
                                     ifelse(region %in% row.names(subset(data_cluster, subset = cluster == 2)), 'Cluster 2', 
                                            ifelse(region %in% row.names(subset(data_cluster, subset = cluster == 3)), 'Cluster 3', 
                                                   ifelse(region %in% row.names(subset(data_cluster, subset = cluster == 4)), 'Cluster 4', 
                                                          'Note Assigned')))))
#unique(thismap$region)
ggplot(thismap, aes(long, lat, fill = fill, group=group)) + 
  geom_polygon(colour="white") + 
  ggtitle("Map of World with K = 4") +
  guides(fill=guide_legend(title="Clusters"))
#silhouette method
silhouette_score <- function(k){
  km <- kmeans(data_scale, centers = k, nstart=1, iter.max = 100)
  ss <- silhouette(km$cluster, dist(data_scale))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)

k_df = data.frame('K' = k, 'Average Silhouette' = avg_sil)
ggplot(k_df, aes(x = K, y = Average.Silhouette)) +
  geom_point() +
  geom_line()

km = kmeans(data_scale, centers = 3, nstart = 1, iter.max = 100)
D <- daisy(data_scale)
plot(silhouette(km$cluster, D), col=1:3, main = '')
abline(v = 0.22)

#hierarchical
d <- dist(data_scale, method = "euclidean") # Euclidean distance matrix.
H.fit <- hclust(d, method="ward.D2")
s.fit <- hclust(d, method="single")
c.fit <- hclust(d, method="complete")
a.fit <- hclust(d, method="average")

cor(d, cophenetic(H.fit)) #0.5290291
cor(d, cophenetic(s.fit)) #0.7604288
cor(d, cophenetic(c.fit)) #0.7337634
cor(d, cophenetic(a.fit)) #0.8394248

plot(H.fit) # display dendogram
#abline(h=15, col="red")
groups <- cutree(H.fit, k=3) # cut tree into 3 clusters
# draw dendogram with red borders around the 3 clusters
rect.hclust(H.fit, k=3, border="red") 
#groups


aggregate(data, by=list(cluster=groups), mean)
data_cluster = cbind(data, cluster = groups)


map = map_data('world')


thismap <- mutate(map, fill = ifelse(region %in% row.names(subset(data_cluster, subset = cluster == 1)), 'Cluster 1', 
                                     ifelse(region %in% row.names(subset(data_cluster, subset = cluster == 2)), 'Cluster 2', 
                                            ifelse(region %in% row.names(subset(data_cluster, subset = cluster == 3)), 'Cluster 3', 
                                                   'Note Assigned'))))
#unique(thismap$region)

ggplot(thismap, aes(long, lat, fill = fill, group=group)) + 
  geom_polygon(colour="white") + 
  ggtitle("Map of World with K = 3") +
  guides(fill=guide_legend(title="Clusters"))

####PCA####
#from correlation

n <- nrow(data)
p <- ncol(data)

# descriptives:
medie <- colMeans(data)
scarto <- apply(data, 2, sd)
descrittive<-round(cbind(medie, scarto),2)
descrittive

rho <- cor(data)
#eigen(rho)
autoval <- eigen(rho)$values
autovec <- eigen(rho)$vectors

# Select components
pvarsp = autoval/p
pvarspcum = cumsum(pvarsp)
pvarsp

# Scree Diagram:
plot(autoval, type="b", main="Scree Diagram", xlab="Components", ylab="Eigenvalue")
abline(h=1, lwd=3, col="red")

#4PC seems to be right

comp<-round(cbind(-eigen(rho)$vectors[,1]*sqrt(autoval[1]),-eigen(rho)$vectors[,2]*sqrt(autoval[2])),3)
rownames(comp)<-row.names(descrittive)
colnames(comp)<-c("Comp1","Comp2")
comp

# the sum of the squares of the values of each row of the component matrix is the respective commonality
comunality<-comp[,1]^2+comp[,2]^2
comp<-cbind(comp,comunality)
comp

# scores:
oecd.scale <- scale(data, T, T)
punteggi <- oecd.scale%*%autovec[,1:2]
# standardized scores
punteggiz<-round(cbind(-punteggi[,1]/sqrt(autoval[1]),-punteggi[,2]/sqrt(autoval[2])),2)
plot(punteggiz, main="Score plot",
     xlab="comp1",ylab="comp2", type = 'n')
text(punteggiz, rownames(data), cex = 0.8)
abline(v=0,h=0,col="red")


# loadinngs
plot(comp[,1:2], main="Loadings plot",
     xlab="comp1",ylab="comp2", xlim=range(-1,1), type = 'n')
text(comp, rownames(comp))
abline(v=0,h=0,col="red")


#pca with princomp
#### princomp :
acp<-princomp(data, cor=T)
sum = summary(princomp(data, cor=T))
plot(acp)
# number of components:
screeplot(princomp(data, cor=T), main = '')
# plots:
score_df = cbind(princomp(data, cor=T)$scores, kmodel$cluster)
score_df = score_df[, c(1, 2, 8)]
colnames(score_df) = c('Comp.1', 'Comp.2', 'Group')
score_df = as.data.frame(score_df)
ggplot(score_df, aes(x = Comp.1, y = Comp.2, color = as.factor(Group))) +
  geom_point()
plot(princomp(data, cor=T)$scores, type = 'n')
text(princomp(data, cor=T)$scores, rownames(data), cex = 0.8)
biplot(acp, cex = 0.8)

#prcomop
pca = prcomp(data_scale)
autoplot(pca)
sum = summary(pca)
prop_var = as.data.frame(sum$importance[2, ])
prop_var['PC'] = rownames(prop_var)
colnames(prop_var) = c('Proportion of variance', 'PC')
cum_var = as.data.frame(sum$importance[3, ])
cum_var['PC'] = rownames(cum_var)
colnames(cum_var) = c('Cumulative Proportion', 'PC')

prop = ggplot(prop_var, aes(x = PC, y = `Proportion of variance`)) +
  geom_bar(stat = 'identity')
cum = ggplot(cum_var, aes(x = PC, y = `Cumulative Proportion`)) +
  geom_bar(stat = 'identity')

prop / cum
score_df$Group = as.factor(score_df$Group)
autoplot(pca, data = score_df, colour = 'Group')

autoplot(pca, data = score_df,
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3, loadings.label.colour = 'black')
