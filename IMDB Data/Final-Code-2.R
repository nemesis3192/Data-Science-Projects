library(randomForest)
library(pROC)
library(ggplot2)
library(lattice)
library(nnet)

#Storing the data into data frame
data1 = read.csv("C:/Users/Rucha Dighe/Documents/IIT Kanpur/Capstone/Movie data/All years_23-4-18.csv", header = TRUE, sep = ",")

#Setting categorical variable to as.factor and numercial variables to as.numeric
MovieName=as.factor(data1$Movie.Name)
Month = as.factor(data1$Month)
Christmas= as.logical(data1$Christmas)
Cricket= as.logical(data1$Cricket)
Normal= as.logical(data1$Normal)
Diwali= as.logical(data1$Diwali)
Holi=as.logical(data1$Holi)
IPL=as.logical(data1$IPL)
Rakshabandhan=as.logical(data1$Rakshabandhan)
Independence=as.logical(data1$Independence)
Valentine=as.logical(data1$Valentine)
SummerVac=as.logical(data1$SummerVac)
Republic=as.logical(data1$Republic)
Navratri=as.logical(data1$Navratri)
Ganesh=as.logical(data1$Ganesh)
Comedy=as.logical(data1$Comedy)
Action=as.logical(data1$Action)
Crime=as.logical(data1$Crime)
Drama=as.logical(data1$Drama)
Mystery=as.logical(data1$Mystery)
Thriller=as.logical(data1$Thriller)
Romance=as.logical(data1$Romance)
Horror=as.logical(data1$Horror)
Sport=as.logical(data1$Sport)
Adventure=as.logical(data1$Adventure)
Biography=as.logical(data1$Biography)
History=as.logical(data1$History)
Musical=as.logical(data1$Musical)
Family=as.logical(data1$Family)
Certificate=as.factor(data1$Certificate)
StarPower=as.factor(data1$StarPower)
Screen=as.numeric(data1$Screen)
Result=as.factor(data1$Result)
BoxOffRes=as.factor(data1$BoxOffRes)
Budget=as.numeric(data1$Budget)

#Creating train and test data
set.seed(123)
ind<- sample(2, nrow(data1), replace=TRUE, prob = c(0.7,0.3))
train<- data1[ind==1,]
test<-data1[ind==2,]


str(data1)
summary(data1)
table(data1$Result)


# Plotting Frequency graphs for categorical variables 
#and point graphs for continous variables

ggplot(data = data1, aes(x = Result, y = Comedy)) + geom_line()
ggplot(data = data1, aes(x = Result, y = Screen)) + geom_line()
ggplot(data1, aes(x=Christmas,fill=Result)) +geom_bar(aes(y = (..count..)/sum(..count..)))
ggplot(data1, aes(x=Cricket,fill=Result)) +geom_bar()
ggplot(data1, aes(x=Normal,fill=Result)) +geom_bar()
ggplot(data1, aes(x=Diwali,fill=Result)) +geom_bar(aes(y = (..count..)/sum(..count..)))
ggplot(data1, aes(x=Holi,fill=Result)) +geom_bar(aes(y = (..count..)/sum(..count..)))
ggplot(data1, aes(x=IPL,fill=Result)) +geom_bar(aes(y = (..count..)/sum(..count..)))
ggplot(data1, aes(x=Rakshabandhan,fill=Result)) +geom_bar()
ggplot(data1, aes(x=Valentine,fill=Result)) +geom_bar()
ggplot(data1, aes(x=Ganesh,fill=Result)) +geom_bar()
ggplot(data1, aes(x=Independence,fill=Result)) +geom_bar(aes(y = (..count..)/sum(..count..)))
ggplot(data1, aes(x=SummerVac,fill=Result)) +geom_bar(aes(y = (..count..)/sum(..count..)))
ggplot(data1, aes(x=Republic,fill=Result)) +geom_bar()
ggplot(data1, aes(x=Navratri,fill=Result)) +geom_bar()
ggplot(data1, aes(x=Comedy,fill=Result)) +geom_bar()
ggplot(data1, aes(x=Drama,fill=Result)) +geom_bar()
ggplot(data1, aes(x=Action,fill=Result)) + geom_bar(aes(y = (..count..)/sum(..count..)))
ggplot(data1, aes(x=Thriller,fill=Result)) +geom_bar()
ggplot(data1, aes(x=Crime,fill=Result)) +geom_bar()
ggplot(data1, aes(x=Mystery,fill=Result)) +geom_bar()
ggplot(data1, aes(x=Romance,fill=Result)) +geom_bar()
ggplot(data1, aes(x=Horror,fill=Result)) +geom_bar()
ggplot(data1, aes(x=Sport,fill=Result)) +geom_bar()
ggplot(data1, aes(x=Adventure,fill=Result)) +geom_bar()
ggplot(data1, aes(x=Biography,fill=Result)) +geom_bar()
ggplot(data1, aes(x=History,fill=Result)) +geom_bar()
ggplot(data1, aes(x=Musical,fill=Result)) +geom_bar()
ggplot(data1, aes(x=Adventure,fill=Result)) +geom_bar()
ggplot(data1, aes(x=Family,fill=Result)) +geom_bar()
ggplot(data1, aes(x=Certificate,fill=Result)) +geom_bar()
ggplot(data1, aes(x=StarPower,fill=Result)) +geom_line()


##Decision Tree and Random forest models
##Confusion matrix for each model
##Roc curve for Random Forest Model

#Model 1- with all independent variables
#Decison Tree
dt1<- ctree(Result~ Christmas+Cricket+Diwali+Holi+IPL+Rakshabandhan+
              Independence+Valentine+SummerVac+Republic+Navratri+Ganesh+Drama+
              Comedy+Action+Crime+Thriller+Romance+Mystery+Adventure+	Horror+
              Sport+Biography+History+Musical+Family+StarPower+Screen,
            data=train, controls = ctree_control(mincriterion = 0.90, minsplit=10))
plot(dt1)
#Predict & Confusion Matrix
dt1pd<-predict(dt1, test)
confusionMatrix(dt1pd, test$Result)

#Random Forest
rf1=randomForest(Result~Christmas+	Cricket+	Normal+	Diwali+	Holi+	IPL+	Rakshabandhan+
                   Independence+	Valentine+	SummerVac+	Republic+	Navratri+	Ganesh+	Drama+Comedy+
                   Action+	Crime+	Thriller+	Romance+	Mystery+	Adventure+	Horror+Sport+	Biography+
                   History+	Musical+	StarPower+Screen,
                 data= train, ntree= 500, mtry= 8, importance=TRUE, proximity=TRUE)

#Predict & Confusion Matrix
rf1pd<- predict(rf1, test)
confusionMatrix(rf1pd, test$Result)

#Importance of each variable
importance(rf1)
varImpPlot(rf1)

#ROC Curve
rf1pdwithprob<- predict(rf1, test, type = "prob")
auc<- auc(test$Result, rf1pdwithprob[,2])
plot(roc(test$Result, rf1pdwithprob[,2]))

#Model 2 with only genre variables
#Decison Tree
dt2<- ctree(Result~ Drama+Comedy+Action+Crime+Thriller+Romance+Mystery+Adventure+Horror+
              Sport+	Biography+	History+	Musical+	Family,
            data=train, controls = ctree_control(mincriterion = 0.10, minsplit=10))
plot(dt2)
#Predict & Confusion Matrix
dt2pd<-predict(dt2, test)
confusionMatrix(dt2pd, test$Result)


#Random Forest
rf2=randomForest(Result~Drama+Comedy+Action+Crime+Thriller+Romance+Mystery+Adventure+Horror+
                   Sport+	Biography+	History+	Musical+	Family,
                 data= train, ntree= 100, mtry= 2, importance=TRUE, proximity=TRUE)


#Predict & Confusion Matrix
rf2pd<- predict(rf2, test)
confusionMatrix(rf2pd, test$Result)

#Importance of each variable
importance(rf2)
varImpPlot(rf2)

#ROC Curve
rf2pdwithprob<- predict(rf2, test, type = "prob")
auc<- auc(test$Result, rf2pdwithprob[,2])
plot(roc(test$Result, rf2pdwithprob[,2]))


#Model 3 with season variables
#Decision Tree
t3<- ctree(Result~ Christmas+Cricket+Normal+Diwali+Holi+IPL+Rakshabandhan+Independence+
             Valentine+SummerVac+Republic+Navratri+Ganesh+Screen,
           data=train, controls = ctree_control(mincriterion = 0.10, minsplit=50))
plot(t3)
#Predict & Confusion Matrix
t3pd<-predict(t3, test)
confusionMatrix(t3pd, test$Result)

#Random Forest
rf3=randomForest(Result~Christmas+Cricket+Normal+Diwali+Holi+IPL+Rakshabandhan+Independence+
                   Valentine+SummerVac+Republic+Navratri+Ganesh,
                 data= train, ntree= 300, mtry= 8, importance=TRUE, proximity=TRUE)


#Predict & Confusion Matrix
rf3pd<- predict(rf3, test)
confusionMatrix(rf3pd, test$Result)

#Importance of each variable
importance(rf3)
varImpPlot(rf3)

#ROC Curve
rf3pdwithprob<- predict(rf3, test, type = "prob")
auc<- auc(test$Result, rf3pdwithprob[,2])
plot(roc(test$Result, rf3pdwithprob[,2]))



