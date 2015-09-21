# test
library(caret);

library(AppliedPredictiveModeling);

set.seed(3433);

data(AlzheimerDisease);

adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[ inTrain,]

testing = adData[-inTrain,]

ColIndex=which(substr(colnames(training),1,2)=="IL")

Imp=which(training[,1]=="Impaired")

Con=which(training[,1]=="Control")

Newdiag= matrix(0,nrow=dim(training)[1],ncol=1)

Newdiag[Imp]=0

Newdiag[Con]=1

#F1= cbind(Newdiag, training[ColIndex[-1]])

F1= training[ColIndex[-1]]

#preProcess(x=cbind(Newdiag, training[ColIndex[-1]]),thresh=0.8,method="pca")

pca7=preProcess(x= F1, method=c("center","scale"))

train1<-predict(pca7,F1)

modelFit1<-train(training$diagnosis ~ ., method="glm",data=train1)

Imp2=which(testing[,1]=="Impaired")

Con2=which(testing[,1]=="Control")

Newdiag2= matrix(0,nrow=dim(testing)[1],ncol=1)

Newdiag2[Imp2]=0

Newdiag2[Con2]=1

ColIndex2=which(substr(colnames(testing),1,2)=="IL")

F2= testing[ColIndex2]

pca72<- preProcess(x= F2,method=c("center","scale"))

test1<-predict(pca72,F2)

confusionMatrix(testing$diagnosis,predict(modelFit1,test1))

