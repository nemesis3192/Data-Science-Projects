library(dplyr)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)


#Load datasets
train <- read.csv('train.csv',stringsAsFactors = F)
test <- read.csv('test.csv',stringsAsFactors = F)
gender_submission <- read.csv('gender_submission.csv',stringsAsFactors = F)

#Basic survival data
table(train$Survived)

#Survival probablity 
prop.table(table(train$Survived))

#Add new fields
train$isChild <- ifelse(train$Age < 18,1,0)
train$FareCat <- case_when(
  train$Fare < 10 ~ '<10',
  train$Fare < 20 & train$Fare >= 10 ~ '10-20',
  train$Fare < 30 & train$Fare >= 20 ~ '10-30',
  train$Fare < 50 & train$Fare >= 30 ~ '30-50',
  train$Fare < 100 & train$Fare >= 50 ~ '50-100',
  train$Fare < 250 & train$Fare >= 100 ~ '100-250',
  train$Fare < 500 & train$Fare >= 250 ~ '250-500',
  train$Fare > 500 ~ '>500'
)


aggregate(Survived~isChild+Sex+FareCat,data = train,FUN = function(x){sum(x)/length(x)})

#Decision tree
tree <- rpart(Survived~Sex+Pclass+Age+SibSp+Parch+Fare+Embarked,data = train,method = 'class')

fancyRpartPlot(tree)


#First prediction
prediction <- predict(tree,test,type = 'class')
submission <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
write.csv(submission,'titanicTreeModel.csv')


#Feature engineering
test$Survived <- NA
combined <- rbind(train,test)
combined$Title <- sapply(combined$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})

combined$Title[combined$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combined$Title[combined$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combined$Title[combined$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combined$Title <- factor(combined$Title)

combined$FamilySize <- combined$SibSp + combined$Parch + 1

combined$Surname <- sapply(combined$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combined$FamilyID <- paste(as.character(combined$FamilySize), combined$Surname, sep="")
combined$FamilyID[combined$FamilySize <= 2] <- 'Small'
famIDs <- data.frame(table(combined$FamilyID))

combined$FamilyID[combined$FamilyID %in% famIDs$Var1] <- 'Small'
combined$FamilyID <- factor(combined$FamilyID)
combined$Title <- factor(combined$Title)


train <- combined[1:891,]
test <- combined[892:1309,]

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, 
             method="class")



#Random Forests: Ensemble

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combined[!is.na(combined$Age),], 
                method="anova")
combined$Age[is.na(combined$Age)] <- predict(Agefit, combined[is.na(combined$Age),])


combined[which(combined$Embarked == ''),'Embarked'] <- 'S'
combined$Embarked <- factor(combined$Embarked)

combined[which(is.na(combined$Fare)),'Fare'] <- median(combined$Fare,na.rm = T)


combined$FamilyID2 <- combined$FamilyID
combined$FamilyID2 <- as.character(combined$FamilyID2)
combined$FamilyID2[combined$FamilySize <= 3] <- 'Small'
combined$FamilyID2 <- factor(combined$FamilyID2)