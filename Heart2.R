library(psych)
library(plyr)
library(tidyverse)
library(ggthemes)
library(kableExtra)
library(rafalib)
library(kernlab)
library(caret)
library(rpart)
library(rpart.plot)
library(naivebayes)
library(skimr)
library(rattle)
library(randomForest)
set.seed(420)
options(digits = 3)
Heart2 <- read_csv('data/Heart2.csv')

#sex
levels(Heart2$sex) <- c("female", "male")
Heart2$sex <- factor(Heart2$sex)

#cp
levels(Heart2$cp) <- c("asymptomatic", "atypical angina", "non anginal pain", "typical angina")
Heart2$cp <- factor(Heart2$cp)

#fbs
Heart2$fbs <- factor(Heart2$fbs)

#restecg
Heart2$restecg <- factor(Heart2$restecg)

#exang
Heart2$exang <- factor(Heart2$exang)

#slope
Heart2$slope <- factor(Heart2$slope)

#thal
Heart2$thal <- factor(Heart2$thal)

#disease
levels(Heart2$disease) <- c("disease", "no disease")
Heart2$disease <- factor(Heart2$disease)
attr(Heart2, 'spec') <- NULL
nrow(Heart2)
Heart2 <- Heart2[complete.cases(Heart2), ]
nrow(Heart2)

intrain<-createDataPartition(y=Heart2$disease,p=0.7,list=FALSE)
tr<-Heart2[intrain,]
tr$disease <- factor(tr$disease, levels = levels(tr$disease), labels = make.names(levels(tr$disease)))

te<-Heart2[-intrain,]

#cross-validation
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10,
                     repeats = 3,
                     classProbs=T,
                     savePredictions = T)

#glm
glm.Fit <- caret::train(disease ~ ., data=tr,
                   method="glm",
                   trControl=ctrl,
                   family="binomial",
                   na.action=na.exclude)
glm.Fit.Model <- predict(glm.Fit, te, type="prob")

pROC::roc(te$disease,
          glm.Fit.Model$disease,
          plot=TRUE,
          print.auc=TRUE)

#rf
rf.Fit <- caret::train(disease ~ ., data=tr,
                             method="rf",
                             preProcess=c("center","scale"),
                             trControl=ctrl,
                             na.action=na.exclude
)
#apply model on testing
rf.Fit.Model <- predict(rf.Fit, te, type="prob")

pROC::roc(te$disease,
          rf.Fit.Model$disease,
          plot=TRUE,
          print.auc=TRUE)

svmLinear.Fit <- caret::train(disease ~ ., data=tr,
                              method="svmLinear",
                              preProcess=c("center","scale"),
                              trControl= ctrl,
                              na.action=na.exclude
                              )
svmLinear.Fit.Model <- predict(svmLinear.Fit, te, type="prob")

pROC::roc(te$disease,
          svmLinear.Fit.Model$disease,
          plot=TRUE,
          print.auc=TRUE)
