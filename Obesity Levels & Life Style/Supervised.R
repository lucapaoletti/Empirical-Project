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

##################
####SUPERVISED####
##################

####DATA PREPROCESSING####
#import
data = read.csv("C:/Users/Luca/Desktop/UNI/STATALE/LEZIONI/SL/ObesityDataSet_raw_and_data_sinthetic (2)/ObesityDataSet_raw_and_data_sinthetic.csv", 
                sep = ',', header = TRUE)
head(data)

#y freq
ggplot(as.data.frame(table(data$NObeyesdad)), aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = 'identity') +
  xlab('Type of Obesity') +
  ylab('Frequence') +
  coord_flip()

#categorical and not

b1 = ggplot(data, aes(x = '', y = FCVC))+
  geom_boxplot(outlier.colour="red") +
  xlab('') +
  ylab('FCVC')

b2 = ggplot(data, aes(x = '', y = NCP))+
  geom_boxplot(outlier.colour="red") +
  xlab('') +
  ylab('NCP')

b3 = ggplot(data, aes(x = '', y = CH2O))+
  geom_boxplot(outlier.colour="red") +
  xlab('') +
  ylab('CH2O')

b1|b2|b3

#Variable transformation
#FCVC vegetable in the meal
data$FCVC = ifelse(data$FCVC < 2, 'Never', 
       ifelse(data$FCVC == 3, 'Always', 'Sometimes'))


#NCP number of main meals
data$NCP = ifelse(data$NCP == 3, '3', ifelse(data$NCP<3, '1 or 2', 'more than 3'))

#CH20 consumption of water
data$CH2O = ifelse(data$CH2O < 1, 'Less than 1', ifelse(data$CH2O>2, 'More than 2', '1 or 2'))

cat_var = c('Gender', 'family_history_with_overweight', 'FAVC', 'CAEC', 
            'SMOKE', 'SCC', 'CALC', 'MTRANS', 'NObeyesdad', 'FCVC', 'NCP', 'CH2O') #, 'FCVC', 'NCP', 'CH2O'
noncat_var = setdiff(colnames(data), cat_var)
for (var in cat_var){
  data[,var] = as.factor(data[,var])
}

levels(data$NObeyesdad) = c('Insuff_w', 'Norm_w', 'OB1', 'OB2', 'OB3', 'OW1', 'OW2')
#correlation matrix for non categorical
corrplot::corrplot(cor(data[, noncat_var]), method = 'number') #no high correlation

#check missing values
count_na = c()
for (col in colnames(data)) {
  count_na = append(count_na, sum(is.na(data[, col])))
}

df_na = data.frame('Feature' = colnames(data), 'NA count' = count_na)
ggplot(df_na, aes(x = Feature, y = NA.count)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  coord_flip() +
  ylab('NA')


#how change y due to change in x
b1 = ggplot(data, aes(x = NObeyesdad, y = Age))+
  geom_boxplot(outlier.colour="red") +
  xlab('') +
  ylab('Age')
#OW1 has a lot of outliers with respect to var AGE

b2 = ggplot(data, aes(x = NObeyesdad, y = Height))+
  geom_boxplot(outlier.colour="red") +
  xlab('') +
  ylab('Height')
#OB2 and OW2 

b3 = ggplot(data, aes(x = NObeyesdad, y = Weight))+
  geom_boxplot(outlier.colour="red") +
  xlab('') +
  ylab('Weight')
#OW2
b1|b2/b3


#some disribution plot
#height and weight
p1 = ggplot(data, aes(x = Height, y = Weight, color = NObeyesdad)) +
  geom_point(size = 2) #make sense

#age and weight
p2 = ggplot(data, aes(x = Weight, y = Age, color = NObeyesdad)) +
  geom_point(size = 2) #doesn't make sense

#age and height
p3 = ggplot(data, aes(x = Height, y = Age, color = NObeyesdad)) +
  geom_point(size = 2) #doesn't make sense

p1|p2/p3

####DECISION TREE####
train=sample(1:nrow(data),2/3 * nrow(data))
#nrow(data)
tree.carseats=tree(NObeyesdad~.,data=data, subset = train, split = 'deviance')
plot(tree.carseats)
text(tree.carseats,pretty=0)
summary(tree.carseats)



imp_logit = as.data.frame(varImp(tree.carseats))
imp_logit['feature'] = rownames(imp_logit)
ggplot(imp_logit, aes(x = feature, y = Overall)) +
  geom_bar(stat = 'identity') +
  coord_flip()



tree.pred=predict(tree.carseats,data[-train,],type="class")
conf_matrix = with(data[-train,],table(tree.pred,NObeyesdad))
conf = confusionMatrix(tree.pred, data[-train, ]$NObeyesdad)
df_metrics = as.data.frame(conf$byClass)
df_metrics = df_metrics[, c('Precision', 'Recall', 'F1', 'Balanced Accuracy')]
df_metrics['Class'] = levels(data$NObeyesdad)

df_metrics = df_metrics %>% pivot_longer(!Class, names_to = 'Metric', values_to = 'Value')

ggplot(df_metrics, aes(x = Metric, y = Value, fill = Value)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~Class)

df_pred = data.frame('pred' = tree.pred, 'True' = data[-train, ]$NObeyesdad)
df_pred['Hit'] = ifelse(df_pred$pred == df_pred$True, 'Hit', 'Not Hit')  
pred_dt = ggplot(df_pred, aes(x = True, fill = pred)) +
  geom_bar(stat = 'count') +
  coord_flip() +
  ggtitle('Decision Tree')



#boosting method
gdis<-maboost(NObeyesdad~.,data=data[train,],iter=200,nu=2
              ,breg="l2", type="sparse",bag.frac=1,random.feature=FALSE
              ,random.cost=TRUE, C50tree=FALSE, maxdepth=6,verbose=FALSE)
summary(gdis)
imp1 = varplot.maboost(gdis, type = 'scores')



#?maboost
pred.gdis= predict(gdis,data[-train, ],type="class")


conf_matrix = with(data[-train,],table(pred.gdis,NObeyesdad))
conf = confusionMatrix(pred.gdis, data[-train, ]$NObeyesdad)
df_metrics = as.data.frame(conf$byClass)
df_metrics = df_metrics[, c('Precision', 'Recall', 'F1', 'Balanced Accuracy')]
df_metrics['Class'] = levels(data$NObeyesdad)

df_metrics = df_metrics %>% pivot_longer(!Class, names_to = 'Metric', values_to = 'Value')

ggplot(df_metrics, aes(x = Metric, y = Value, fill = Value)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~Class)

df_pred = data.frame('pred' = pred.gdis, 'True' = data[-train, ]$NObeyesdad)
df_pred['Hit'] = ifelse(df_pred$pred == df_pred$True, 'Hit', 'Not Hit')  
pred_gd = ggplot(df_pred, aes(x = True, fill = pred)) +
  geom_bar(stat = 'count') +
  coord_flip() +
  ggtitle('Gradient Boosting')

  


####MULTICLASS LOGISTIC####
# Fit the model
train_df = data[train, ]
logit <- nnet::multinom(NObeyesdad ~Gender + Weight + Height + Age + FAVC +
                          FCVC + NCP + CAEC + CH2O + FAF + SCC + TUE, data = train_df)
logit.pred <- predict(logit, data[-train, -which(names(train_df) %in% 
                                                   c('MTRANS', 'CALC', 'SMOKE', 'family_history_with_overweight', 'NObeyesdad'))])
sum = summary(logit)
sum$residuals
plot(logit)
#summary(logit)

imp_logit = as.data.frame(scale(varImp(logit)))
imp_logit['feature'] = rownames(imp_logit)
imp2 = ggplot(imp_logit, aes(x = feature, y = Overall)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  ggtitle('Logistic Regression') +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())


conf_matrix = with(data[-train,],table(logit.pred,NObeyesdad))
conf = confusionMatrix(logit.pred, data[-train, ]$NObeyesdad)
df_metrics = as.data.frame(conf$byClass)
df_metrics = df_metrics[, c('Precision', 'Recall', 'F1', 'Balanced Accuracy')]
df_metrics['Class'] = levels(data$NObeyesdad)

df_metrics = df_metrics %>% pivot_longer(!Class, names_to = 'Metric', values_to = 'Value')

ggplot(df_metrics, aes(x = Metric, y = Value, fill = Value)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~Class)

df_pred = data.frame('pred' = logit.pred, 'True' = data[-train, ]$NObeyesdad)
df_pred['Hit'] = ifelse(df_pred$pred == df_pred$True, 'Hit', 'Not Hit')  
pred_lr = ggplot(df_pred, aes(x = True, fill = pred)) +
  geom_bar(stat = 'count') +
  coord_flip() +
  ggtitle('Logistic Regression')
####KNN####
k = 1:20
acc = c()

for (x in k){
  pred = knn(train = data[train, noncat_var], test = data[-train, noncat_var], 
             cl = data[train, 'NObeyesdad'], k = x)
  acc_k = mean(pred == data[-train, ]$NObeyesdad)
  acc = append(acc, acc_k)
}

df_acc = data.frame('K' = k, 'Accuracy' = acc)
ggplot(df_acc, aes(x = K, y = Accuracy)) +
  geom_line() +
  geom_point()


pred = knn(train = data[train, 1:ncol(data)-1], test = data[-train, 1:ncol(data)-1], 
           cl = data[train, 'NObeyesdad'], k = 3)



conf_matrix = with(data[-train,],table(pred,NObeyesdad))

conf = confusionMatrix(pred, data[-train, ]$NObeyesdad)
conf$byClass
df_metrics = as.data.frame(conf$byClass)
df_metrics = df_metrics[, c('Precision', 'Recall', 'F1', 'Balanced Accuracy')]
df_metrics['Class'] = levels(data$NObeyesdad)

df_metrics = df_metrics %>% pivot_longer(!Class, names_to = 'Metric', values_to = 'Value')

ggplot(df_metrics, aes(x = Metric, y = Value, fill = Value)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~Class)

df_pred = data.frame('pred' = pred, 'True' = data[-train, ]$NObeyesdad)
df_pred['Hit'] = ifelse(df_pred$pred == df_pred$True, 'Hit', 'Not Hit')  
pred_knn = ggplot(df_pred, aes(x = True, fill = pred)) +
  geom_bar(stat = 'count') +
  coord_flip() +
  ggtitle('KNN with K = 3')


(pred_dt|pred_gd)/(pred_lr|pred_knn)
df_imp = data.frame('Feature' = names(imp1), 'Importance' = scale(imp1))
imp1 = ggplot(df_imp, aes(x = Feature, y = Importance)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  ylab('Overall') +
  ggtitle('Gradient Boosting')+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

imp1|imp2
