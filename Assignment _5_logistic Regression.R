#read the training data into a dataframe called train

train=read.csv("C:/Users/ENVY X360/Desktop/R/class 5/train.csv",header=TRUE,sep=",")

#set the pclass, passengers pseudoclass, to be ordered categorical
train$pclass =factor(train$Pclass,levels = c(3, 2, 1), ordered = TRUE)

#create a truth vector of survival results from training
S = train$Survived == 1

#read the test data into a dataframe named test
test=read.csv("C:/Users/ENVY X360/Desktop/R/class 5/test.csv",header=TRUE,sep=",")

#pclass is categorical for test data also
test$pclass =factor(test$Pclass,levels = c(3, 2, 1), ordered = TRUE)


sapply(train,function(x) sum(is.na(x)))

sapply(train, function(x) length(unique(x)))

#install.packages("Amelia")
library(Amelia)
missmap(train, main = "Missing values vs observed")

#missing values of age
data = subset(train,select=c(2,3,5,6,7,8,12,13))

#missing values of age by mean of age
data$Age[is.na(data$Age)]=mean(data$Age,na.rm=T)

is.factor(data$Sex)

is.factor(data$Embarked)

contrasts(data$Sex)

contrasts(data$Embarked)

data <- data[!is.na(data$Embarked),]
rownames(data) <- NULL

#Model fitting

train <- data[1:800,]
test <- data[801:889,]

model <- glm(Survived ~.,family=binomial(link='logit'),data=train)
summary(model)


anova(model, test="Chisq")

#install.packages("pscl")
library(pscl)
pR2(model)


fitted.results <- predict(model,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))



#install.packages("ROCR")
library(ROCR)
p <- predict(model, newdata=subset(test,select=c(2,3,4,5,6,7,8)), type="response")
pr <- prediction(p, test$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
