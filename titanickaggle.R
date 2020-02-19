library(readr)
test <- read_csv("~/kaggle challenges/titanic/test.csv")
train <- read_csv("~/kaggle challenges/titanic/train.csv")
train <- train[,-c(1,4,9,11)]
test <- test[,-c(3,8,10)]
# train <- train[-which(train$Parch==6),]
length(which(is.na(train$Age)==T))
length(which(is.na(train$Fare)==T))
hist(train$Age)

train$Survived=as.factor(train$Survived)
train$Sex=as.factor(train$Sex)
train$Pclass=as.factor(train$Pclass)
train$SibSp=as.factor(train$SibSp)
train$Embarked=as.factor(train$Embarked)
# train$Parch=as.factor(train$Parch)
test$Sex=as.factor(test$Sex)
test$Pclass=as.factor(test$Pclass)
test$SibSp=as.factor(test$SibSp)
test$Embarked=as.factor(test$Embarked)
# test$Parch=as.factor(test$Parch)
ii1=which(test$SibSp==8)
ii2=which(test$Parch==9)
ii=c(ii1,ii2)

# mm1=glm(Survived~., family = "binomial", data = train)
# summary(mm1)
# 
# pp=predict.glm(mm1, newdata=test[-ii,], type = "response")
# pp[which(pp<0.5)]=0
# pp[which(pp>0.5)]=1

# install.packages("class")
# library(class)
# mm2=knn(train=train, test=test, k=2, cl=train$Survived)

install.packages("randomForest")
library(randomForest)
xtrain=train[,-1]
ytrain=train$Survived
train1=rfImpute(Survived~., data=train, ntree = 10000)
train1$Age=round(train1$Age)
mm3=randomForest(x=train1[,-1], y=ytrain, ntree = 100000)

ii3=which(is.na(test$Age)==T)
test$Fare[153]=30
Agetest=test$Age
Agetest1=Agetest[-ii3]
xtest=test[-ii3,]
mm4=randomForest(x=xtest[,-c(1,4)], y=Agetest1, ntree = 100000)
Agetest[ii3]=predict(mm4, newdata = test[ii3,-c(1,4)])
test$Age=Agetest

pp=predict(mm3, newdata = test[,-1]) 
pptest=cbind(Survived,test)
csvfile=pptest[,c(1,2)]
names(csvfile)=c("Survived","PassengerId")
write.csv(csvfile, file = "C:\\Users\\Abraham\\Documents\\kaggle challenges\\titanic\\titanickaggle.csv", row.names = F)








