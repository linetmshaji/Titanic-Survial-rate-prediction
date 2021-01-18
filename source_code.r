#load libraries
library("dplyr")
library("caTools")
library("caret")

#?caTools


#set working directory
setwd("C:/Users/Anna/Documents/dsproject")
getwd()



#load datasets
dft1<-read.csv("ptrain.csv")
dft2<-read.csv("ptest.csv")
df3<-read.csv("psub.csv")

#merging and binding dataset
m1<-merge(x=dft2,y=df3, by = "PassengerId")
View(head(m1,5))
View(head(dft1,5))
m2<- rbind(m1,dft1)

#merged dataframe for Analysis
TitanicDS<-m2
View(TitanicDS)
duplicated(TitanicDS)

#cleaning
str(TitanicDS)
TitanicDS$PassengerId<-as.factor(TitanicDS$PassengerId)
TitanicDS$Pclass<-as.numeric(TitanicDS$Pclass)
TitanicDS$Name<-as.factor(TitanicDS$Name)
TitanicDS$Sex<-as.factor(TitanicDS$Sex)
TitanicDS$Ticket<-as.factor(TitanicDS$Ticket)
TitanicDS$Embarked<-as.factor(TitanicDS$Embarked)
TitanicDS$Survived<-as.factor(TitanicDS$Survived)
TitanicDS$Cabin<-as.factor(TitanicDS$Cabin)
TitanicDS$SibSp<-as.numeric(TitanicDS$SibSp)
TitanicDS$Parch<-as.numeric(TitanicDS$Parch)
TitanicDS$Fare<-as.numeric(TitanicDS$Fare)




summary(TitanicDS)
#check for Null Values
table(is.na(TitanicDS))
table(is.na(TitanicDS$Survived))
table(is.na(TitanicDS$Pclass))
table(is.na(TitanicDS$Sex))
table(is.na(TitanicDS$Age))
table(is.na(TitanicDS$Fare))

#missing Values are recorded in the Age and Fare field. 
#we replace this with the mean age 
TitanicDS$Age[is.na(TitanicDS$Age)]<-mean(TitanicDS$Age,na.rm = TRUE)
View(TitanicDS)
table(is.na(TitanicDS$Age))


TitanicDS$Fare[is.na(TitanicDS$Fare)]<-mean(TitanicDS$Fare,na.rm = TRUE)
View(TitanicDS)
table(is.na(TitanicDS$Fare))


#exploratory analysis
#total count of men and women on the ship
genderCount<-table(TitanicDS$Sex)
genderCount
barplot(genderCount, main = "Male Vs Female Present on Ship"
        ,xlab = "Gender",ylab = "Count",col = c("orange","brown"))

#Number of Survivors against Dead
survive<-table(TitanicDS$Survived)
survive
barplot(survive, main = "Number of Survivors Against Non Survivors",
        names.arg = c("No","Yes"),
        ylab = "Count",col=c("red","green"),
        xlab = "Survival")

#gender survival ratio
GenderSur<-table(TitanicDS$Survived,TitanicDS$Sex)
GenderSur
barplot(GenderSur,main = "Male to Female Survival",beside = T,col=c("red","green"))
legend("topleft", 
       legend = c("Died", "Survived"), 
       col = c("red", 
               "green"), 
       pch = c(15,15), #shape
       bty = "n", 
       pt.cex = 2,#size 
       cex = 1.2, #size
       horiz = F , 
)

#gender Distribution In the Various Classes
genderclass<-table(TitanicDS$Sex,TitanicDS$Pclass)
genderclass
barplot(genderclass,main = "Gender Count Per Ship Class",
        beside = T,col = c("orange","brown"),names.arg = c("1st Class","2nd Class","3rd class")
)
legend("top", 
       legend = c("Female", "Male"), 
       col = c("orange", 
               "brown"), 
       pch = c(15,15), #shape
       bty = "n", 
       pt.cex = 2,#size 
       cex = 1.2, #size
       horiz = F  
)

#gender survival count in classes 
GenderCS<-table(TitanicDS$Sex,TitanicDS$Pclass,TitanicDS$Survived)
GenderCS


gender.names=c("Female","Male")
P <- array(GenderCS,dim = c(2,3,2),dimnames = list(gender.names))
print(P)

##The Survival Group For the Class Gender Count(Died)
dgc<-(P[,,1])
dgc
barplot(dgc,main = "Class Gender Survival Count (Died)",beside = T,
        names.arg = c("1st Class","2nd Class","3rd class"),col = c("orange","brown"))
legend("top", 
       legend = c("Female", "Male"), 
       col = c("orange", 
               "brown"), 
       pch = c(15,15), #shape
       bty = "n", 
       pt.cex = 2,#size 
       cex = 1.2, #size
       horiz = F  
)
        

##The Survival Group For the Class Gender Count(Survived)
cgsS<-(P[,,2])
barplot(cgsS,main = "Class Gender Survival Count (Survived)",beside = T,
        names.arg = c("1st Class","2nd Class","3rd class"),col = c("orange","brown"))
        legend("top", 
               legend = c("Female", "Male"), 
               col = c("orange","brown"), 
               pch = c(15,15), #shape
               bty = "n", 
               pt.cex = 2,#size 
               cex = 1.2, #size
               horiz = F  )

#age classification
#age distribution'
summary(TitanicDS$Age)
Agesplit<-cut(TitanicDS$Age,breaks = c(0,12,24,48,90),right = FALSE)
Agedis<-table(Agesplit)
Agedis
barplot(Agedis,main = "Age Distribution On Ship",names.arg = c("Children","Young Adult","Adult","Old Peeps")
        ,ylab="count",col =rainbow(6) )

#survival count vs age
Agesurv<-table(TitanicDS$Survived,Agesplit)
Agesurv
barplot(Agesurv,main = "Age Distribution Survival Ratio",beside = T,names.arg =c("Children","Young Adult","Adult","Old Peeps" )
        ,col = c("red","green"))
legend("topright",
       legend = c("Died", "Survived"), 
       col = c("red", 
               "green"), 
       pch = c(15,15), #shape
       bty = "n", 
       pt.cex = 2,#size 
       cex = 1.2, #size
       horiz = F  
)

#analytics
#from our Exploratory Analysis we can say
#class,age and gender are the core determinant of survival rate on the Titaic 
SubTitanicDS<-select(TitanicDS,Survived,Sex,Age,Pclass)
View(SubTitanicDS)



#We now split the dataset into training and testing on survived
set.seed(5555)
dfsplit<-sample.split(SubTitanicDS$Survived,SplitRatio = 0.7)
trainingds<-subset(SubTitanicDS,dfsplit==TRUE)
testingds<-subset(SubTitanicDS,dfsplit==FALSE)
View(trainingds)
View(testingds)

#building Our Model
fitCtrl = trainControl (method = "cv", number = 10)
Model = train (Survived ~., trControl = fitCtrl, 
                  method = "gbm", data = trainingds, verbose = FALSE)
summary(Model)

#Testing Our Model
ModelPred<-predict(Model,testingds)
ModelPred

#saving Prediction
write.csv(ModelPred, file = "Prediction.csv")
read.csv("Prediction.csv")

#checking The Accuracy of the model
confusionMatrix(ModelPred,testingds$Survived)


#create Random Person to check if He or She will survive 
Pclass=3
Sex="female"
Age=35

Linet=data.frame(c(Sex,Age,Pclass))

predict(Model,Linet)

Pclass=2
Sex="male"
Age=53

Jesse=data.frame(c(Sex,Age,Pclass))
predict(Model,Jesse)



