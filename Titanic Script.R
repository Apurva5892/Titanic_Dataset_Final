getwd()
a<-read.csv("titanic_train.csv")
a
View(a)       # In View 'V' is always capital
str(a)
class(a)

# TRYING TO UNDERSTAND THAT HOW MAN PEOPLE SURVIVED IN PROPORTION OF GENDER

table(a$Survived , a$Sex)

# IN PROP.TABLE WHERE IT IS 1 IT % ROW WISE
prop.table(table(a$Survived , a$Sex),1)

#  IN PROP.TABLE WHERE IT IS 1 IT % COLUMN WISE
prop.table(table(f$Survived , f$Sex),2)




# READING THE TEST_TITANIC , ADDING SURVIVAL COLUMN BASED ON ESTIMATES FROM PROP.TABLE

test <- read.csv("titanic_test.csv")

test

# ADDING A COLUMN SURVIVED IN TEST DATA SET
test$Survived <- 0

# PUTTING SURVIVED FOR ALL THE FEMALE .. AS OBSERVED MOSTLY WOMEN & CHILDREN WERE SAVED

test$Survived[test$Sex == "female"] <- 1

test$Survived

names(test)

# TAKING ONLY PASSENGER_ID & SURVIVED COLUMN
test1<-test[c(1,12)]
test1
write.csv(test1,"test_submit1.csv")

# CREATING A NEW VARIABLE CHILD .. FEATURE EXTRACTION
a$Child <- 0
a$Child[test$Age <= 18] <- 1
a$Child
table(a$Child)

count(a$Child == 1)

length(a)

table(a$Sex)

# THIS GIVES YOU TOTAL NO. OF MALE ,FEMALE W.R.T CHILD & ADULT PRESENT ON SHIP
aggregate(Survived ~ Child + Sex ,data = a ,FUN = length )

# THIS GIVES YOU TOTAL NO. OF MALE,FEMALE W.R.T CHILD & ADULT WHICH SURVIVED
aggregate(Survived ~ Child + Sex ,data = a ,FUN = sum )

aggregate(Survived ~ Child + Sex ,data = a,FUN = function(x) { sum(x)/length(x) })

# CONVERTING THE FARE IN CATEGORIES IN A NEW VARIABLE FARE2
a$Fare2 <- 0
a$Fare2[a$Fare < 10] <- '1'
a$Fare2[a$Fare >= 10  & a$Fare < 20 ] <- '2'
a$Fare2[a$Fare >= 20  & a$Fare2 < 30] <- '3'
a$Fare2[a$Fare >= 30 ] <- '4'

str(a$Fare2)

as.factor(a$Fare2)

table(a$Survived,a$Fare2)
prop.table(table(a$Survived,a$Fare2),1)
prop.table(table(a$Survived,a$Fare2),2)

prop.table(table(a$Survived,a$Sex,a$Fare2,a$Pclass),2)


# ANALYSING AS PER FARE2 , SEX & PCLASS

aggregate(Survived ~ Sex + Fare2 + Pclass,data = a,FUN = length)

aggregate(Survived ~ Sex + Fare2 +  Pclass,data = a,FUN = sum)

agg<- aggregate(Survived ~ Sex + Fare2 +Pclass,data = a,FUN = function(x) {sum(x)/length(x)})
agg

class(agg)

library(dplyr)
arrange(agg,desc(Survived))

library(rpart)
fit <- rpart(Survived ~ Age + Sex + Pclass + a$SibSp +a$Parch + a$Fare +a$Embarked,data = a ,method  = "class")
plot(fit)
text(fit)

#install.packages('rattle')
#install.packages('rpart.plot')
#install.packages('RColorBrewer')
install.packages("RGtk2")
install.packages("https://cran.r-project.org/bin/windows/contrib/3.3/RGtk2_2.20.31.zip", repos=NULL)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

.libPaths()

fancyRpartPlot()

cor(a$Pclass,a$Fare)
a$Pclass

cor(a$Fare,a$Cabin)

# NOW THE ACCURACY OF TREE ISN'T GOODSO WE NEED TO DO SOME FEATURE EXTRACTION

t<-read.csv("titanic_test.csv")

# TO COMBINE TRAINNING & TESTING DATA SET WE NEED TO CREATE A VAR Survived IN TEST DATA SET

t$Survived <- NA
t$Child <- NA
t$Fare2 <- NA
at <- rbind(a,t)

colnames(a)
colnames(t)

str(at)

as.factor(at$Survived)
as.factor(at$Pclass)
as.factor(at$SibSp)
as.factor(at$Parch)
as.factor(at$Child)
as.factor(at$Fare2)


at$Name[1]
class(at$Name)
# AS THE CLASS IS FACTOR , CONVERT IT IN CHARACTER
at$Name <- as.character(at$Name)
at$Name
class(at$Name)

strsplit(at$Name,split='[,.]')
strsplit(at$Name[1], split="[,.]")
strsplit(at$Name[1], split="[,.]")[[1]]
strsplit(at$Name[1], split="[,.]")[[1]][2]

# SAPPLY TO APPLY IT ON ALL THE  NAMES

at$Title <- sapply(at$Name , FUN = function(x) {strsplit(x,split = "[,.]")[[1]][2]})

at$Title

table(at$Title)

class(at$Title)

# REMOVING SPACES IN FRONT OF THE WORD

at$Title <- sub(" ","",at$Title)


at$Title[at$Title %in% c("Mlle","Mme")] <- "Mlle"

at$Title[at$Title %in% c("Capt","Don","Major","Sir")] <- "Sir"

at$Title[at$Title %in% c("Dona","Lady","the Countess","Jonkheer")] <- "Lady"

at$Title <- as.factor(at$Title)
at$Title

# CREATING A NEW VARIABLE FAMILY SIZE = Sbisp + Parch

at$FamilySize <- at$SibSp + at$Parch +1

at$FamilySize

class(at$FamilySize)

strsplit(at$Name,split = '[.]')
strsplit(at$Name[1],'[.]')[[1]][2]

at$Surname <- sapply(at$Name,FUN = function(x) { strsplit(x,split = '[.]')[[1]][2]})

at$Surname

# sapply(at$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

at$FamilyId <- paste(as.character(at$FamilySize),at$Surname,sep = "")

at$FamilyId

class(at$FamilyId)

table(at$FamilyId)


at$FamilyId[at$FamilySize == 2] <- 'Small'

at$FamilyId

famIds <- data.frame(table(at$FamilyId))

famIds

arrange(famIds )

famIds <- famIds[famIds$Freq <= 2,]

famIds

at$FamilyId[at$FamilyId %in% famIds$Var1] <- 'Small'

at$FamilyId

at$FamilyId <- factor(at$FamilyId)

at$FamilyId

train <- at[1:891,]
test <- at[892:1309,]

train$Survived
test$Survived


fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyId,
             data=train,  method="class")
fit

plot(fit)
text(fit)
fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")

submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)

write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

library(party)

fit1 <- ctree(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyId,
      data=train)

Prediction1 <- predict(fit1, test)

submit1 <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)

write.csv(submit, file = "myfirstdtree1.csv", row.names = FALSE)

# CHECKING THE ACCURACY WITH CONDITIONAL INFERENCE TREE .. THEY MAKE THERE DECISON BASED ON
# STATISTICAL TEST RATHER THAN A PURITY MEASURE

library(party)

fit3 <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                 Embarked + Title + FamilySize + FamilyId,
               data = train, 
               controls=cforest_unbiased(ntree=2000, mtry=3))

Prediction3 <- predict(fit3, test, OOB=TRUE, type = "response")

Submit3 <- data.frame(PassengerId = test$PassengerId, Survived = Prediction3)


write.csv(Submit3,file = "Conditional_inference_Tree.csv", row.names = FALSE)

1


# NOW DO SOME MORE FEATURE ENGINEERING
# CREATE A FACTOR OF CABINS
# IMPUTE THE MISSING VALUES OF AGE
# IMPUTE THE INCORRECT VALUES OF FARE

# CONVERTING IN FACTOR

BD <- at$Cabin[at$Cabin == c('A10','A14','A16')] <- 'A'
length(BD)

bd1 <- sub(c("A5","A6","A7"),"A",at$Cabin)
bd1

bd2 <- data.frame(at$Cabin)
sapply(bd2, FUN = function(x){ sub(c("A5","A6","A7"),"A")})

bd2

bd3 <- substr(bd2, start=1, stop=2)
bd3

table(is.na(a$Age))
table(is.na(at$Age))

###################################

# IN NF WHAT CHANGED IS CABIN IS CATEGORIZED IN A ,B ,C IN BROADER SENSE
nf <- read.csv("train_test.csv")
cor(nf$Pclass,nf$Cabin)

class(nf$Cabin)

colSums(is.na(nf))

# AT SOME PLACES FARE IS ZERO U NEED TO CORRECT IT
table(nf$Fare == 0) 

nf$Fare[nf$Fare == 0]<- NA

# PUTTING VALUES IN CHILD COLUMN IN TESTING DATA SET
nf$Child[nf$Age <= 18] <- 1
nf$Child[nf$Age > 18] <- 0

colSums(is.na(nf))

nf$Fare2 <- 0
nf$Fare2[a$Fare < 10] <- '1'
nf$Fare2[a$Fare >= 10  & a$Fare < 20 ] <- '2'
nf$Fare2[a$Fare >= 20  & a$Fare2 < 30] <- '3'
nf$Fare2[a$Fare >= 30 ] <- '4'


str(nf$Cabin)

nf$Age1 <- NA
nf$Age1 = nf$Age

colSums(is.na(nf))
colnames(nf)

nf1 <- nf[c(-2,-5,-13)] # REMOVN - SURVIDED,AGE,CHILD

colnames(nf)
colnames(nf1)


nf2 <- kNN(nf1)

colnames(nf234)

nf2 <- nf2[-c(17:32)]

colnames(nf2)

# COMPARING THE DATA OF ORINGINAL AGE & IMPUTED AGE

par(mfrow=c(1,2))

# par(mfrow = c(1,2)) FUNCTION TELLS R TO DRAW TO DIFFERENT GRAPHS SIDE NBY SIDE

hist(nf2$Age1, freq=F, main='Age: Imputed Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(nf$Age, freq=F, main='Age: Original Data', 
     col='lightgreen', ylim=c(0,0.04))


nf1$Fare == nf2$Fare

nf1$Cabin == nf2$Cabin

colSums(is.na(nf2))

# nf1$Age1 <- kNN(nf1$Age1)

colSums(is.na(nf2))

nf2$Child <- NA
nf2$Survived <- NA

nf2$Child[nf2$Age1 <= 18] <- 1
nf2$Child[nf2$Age1 >18] <- 0

colnames(nf2)

colSums(is.na(nf2))

nf2$Fare2

str(nf2)

nf2 <- nf2[-1]
nf2
colnames(nf2)
str(nf2)
nf2$SibSp <- as.factor(nf2$SibSp)
nf2$Parch <- as.factor(nf2$Parch)
nf2$Pclass <- as.factor(nf2$Pclass)
nf2$Fare2 <- as.factor(nf2$Fare2)
nf2$Child <- as.factor(nf2$Child)
nf2$Survived <- as.factor(nf2$Survived)

colnames(nf2)

train1 <- nf2[1:891,]
test1 <- nf2[892:1309,]

train1$Survived = train$Survived

library(rpart)
fit5 <- rpart(Survived ~ . ,data = train1 , method = "class")
fit5
plot(fit5)

fancyRpartPlot(fit5)

colnames(train1)
# REMOVING NAMES ,SURNAMES,TICKET
train1 <- train1[-c(1,5,13)]

# REMOVING FAMILYID FROM TRAIN1

train2 <- train1[-11]

fit6 <- rpart(Survived ~ . ,data = train2 , method = "class")

plot(fit5)

fancyRpartPlot(fit5)

colnames(test1)

# REMOVING NAME,TICKET,SURNAME
test1 <- test1[-c(1,5,13)]

colnames(test1)
test2 <- test1[-11]

colnames(train1)
colnames(test1)
str(test1)
str(train1)
train1$Survived <- as.factor(train1$Survived)

test1$Survived


# HERE TYPE = CLASS .. IT CLASSIFIES BW 0 & 1
.# TYPE = RESPONSE
# TYPE = PROB   .. GIVES U THE PROBABLITY FOR EVERY 0 & 1

prediction5 <- predict(fit6,test2, OOB=TRUE, type = "class")

Submit5 <- data.frame(PassengerId = test$PassengerId,Survived = prediction5)

write.csv(Submit5 , "Submit5.csv",row.names = FALSE)


write.csv(nf2,file = "nf2.csv", row.names = FALSE)


