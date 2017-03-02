#install.packages("Boruta")



library(Boruta)


#setwd("../LoanPrediction")

getwd()


traindata <- read.csv("./LoanPrediction/train1.csv", header = T, stringsAsFactors = F)

str(traindata)




dim(traindata)

head(traindata)


str(traindata)


names(traindata) <- gsub("_", "", names(traindata))




summary(traindata)
traindata[traindata == ""] <- NA
traindata <- traindata[complete.cases(traindata),]


convert <- c(2:6, 11:13)
convert
dim(traindata)
traindata[,convert] <- data.frame(apply(traindata[,convert], 2, as.factor))


set.seed(123)
boruta.train <- Boruta(LoanStatus~.-LoanID, data = traindata, doTrace = 2)


print(boruta.train)


plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
       at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)



final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)


getSelectedAttributes(final.boruta, withTentative = F)


boruta.df <- attStats(final.boruta)

class(boruta.df)

print(rownames(boruta.df)[boruta.df$decision=="Confirmed"])



#----------------------------Tranditional method

library(caret)
library(randomForest)
set.seed(123)

control <- rfeControl(functions=rfFuncs, method="cv", number=10)

#rfFuncs---Ancillary functions for backwards selection

colnames(traindata)

rfe.train <- rfe(traindata[,2:12], traindata[,13], sizes=1:12, rfeControl=control)
rfe.train

plot(rfe.train, type=c("g", "o"), cex = 1.0, col = 1:11)

predictors(rfe.train)
#The top 5 variables (out of 8):
#  CreditHistory, ApplicantIncome, LoanAmount, CoapplicantIncome, LoanAmountTerm
