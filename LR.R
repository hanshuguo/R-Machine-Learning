

training.data.raw <- read.csv('Titanic.train.csv',header=T,na.strings=c(""))

str(training.data.raw)
table(training.data.raw$Survived)


sapply(training.data.raw,function(x) sum(is.na(x)))

sapply(training.data.raw, function(x) length(unique(x)))


#install.packages("Amelia")
library(Amelia)
missmap(training.data.raw, main = "Missing values vs observed")


data <- subset(training.data.raw,select=c(2,3,5,6,7,8,10,12))



data$Age[is.na(data$Age)] <- mean(data$Age,na.rm=T)

is.factor(data$Sex)
#TRUE

is.factor(data$Embarked)
#TRUE


contrasts(data$Sex)
# male
# female    0
# male      1

contrasts(data$Embarked)

# Q S
# C 0 0
# Q 1 0
# S 0 1


data <- data[!is.na(data$Embarked),]
rownames(data) <- NULL


train <- data[1:800,]
test <- data[801:nrow(data),]

dim(data)

str(train)


dim(train)

model <- glm(Survived ~.,family=binomial(link='logit'),data=train)
summary(model)

df.residual(model)




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
