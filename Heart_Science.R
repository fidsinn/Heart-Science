library(psych)
library(plyr)
library(tidyverse)
library(ggthemes)
library(kableExtra)
library(rafalib)
library(kernlab)
library(caret)
library(rpart)
library(rattle)
library(pROC)

options(digits = 3)

HeartData <- read_csv('data/heartdata.csv')

#factor attributes

#age: age in years

#sex: (0 = female; 1 = male)
HeartData$sex <- factor(HeartData$sex)
levels(HeartData$sex) <- c("female", "male")

#cp: chest pain type (0: asymptomatic, 1: atypical angina, 2: non-anginal pain, 3: typical angina)
HeartData$cp <- factor(HeartData$cp)
levels(HeartData$cp) <- c("asymptomatic", 
                          "atypical angina", 
                          "non anginal pain", 
                          "typical angina")

#trestbps: resting blood pressure (in mm/Hg on admission to the hospital)

#chol: serum cholesterol in mg/dl

#fbs: fasting blood sugar > 120 mg/dl (0=no, 1=yes)
HeartData$fbs <- factor(HeartData$fbs)
levels(HeartData$fbs) <- c("<=120", 
                           ">120")

#restecg: resting electrocardiographic results (0: showing probable or definite left ventricular hypertrophy (by Estes' criteria), 1: normal, 2: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV))
HeartData$restecg <- factor(HeartData$restecg)
levels(HeartData$restecg) <- c("left vetricular hypertrophy", 
                               "normal",
                               "st-t abnormality")

#thalach: maximum heart rate achieved

#exang: exercise induced angina (0 = no, 1=yes)
HeartData$exang <- factor(HeartData$exang)
levels(HeartData$exang) <- c("no", 
                               "yes")

#oldpeak: ST depression induced by exercise relative to rest

#slope: the slope of the peak exercise ST segment (0: downsloping; 1: flat; 2: upsloping)
HeartData$slope <- factor(HeartData$slope)
levels(HeartData$slope) <- c("downsloping", 
                             "flat",
                             "upsloping")

#ca: number of major vessels (0-3) colored by flourosopy
HeartData$ca <- factor(HeartData$ca)
levels(HeartData$ca) <- c(0, 
                          1, 
                          2, 
                          3, 
                          NA)

#thal: Thalium Stress Test Result (0: null, 1:fixed defect, 2= normal, 3=reversible defect), original: (3 = normal; 6 = fixed defect; 7 = reversable defect)
HeartData$thal <- factor(HeartData$thal)
levels(HeartData$thal) <- c(NA, 
                            "fixed defect", 
                            "normal", 
                            "reversible defect")

#target diagnosis of heart disease (angiographic disease status): 0 = disease (> 50% diameter narrowing), 1 = no disease (< 50% diameter narrowing)
#delete target
HeartData$disease <- HeartData$target
HeartData$target <- NULL
#factor disease
HeartData$disease <- factor(HeartData$disease)
levels(HeartData$disease) <- c("disease", "no disease")
#delete spec
attr(HeartData, 'spec') <- NULL

# #SUMMARY
# names(HeartData)
# summary(HeartData)
# 
# #AGE
# #summary
# summary(HeartData$age)
# 
# #mean age
# mean_age <- as.numeric(HeartData %>%
#                          summarize(mean_age=mean(age)))

# #age barplot (age_dist)
# HeartData %>%
#   mutate(age_rnd=round(age, digits=-1)) %>%
#   group_by(age_rnd) %>%
#   summarize(count=n()) %>%
#   ggplot(aes(age_rnd, count, fill=age_rnd)) +
#   geom_bar(stat="identity") +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text(), legend.position = "none") +
#   xlab("age (rounded)")
# 
# ##age vs sex no/disease barplot ()
# HeartData %>%
#   group_by(age, sex) %>%
#   filter(disease=="no disease") %>%
#   ggplot(aes(age, fill=sex)) +
#   geom_bar(position="fill")
# 
# #SEX
# #sex grouping (sex_count)
# HeartData %>%
#   group_by(sex) %>%
#   summarize(Count=n()) %>%
#   knitr::kable(table.attr = "style='width:30%;'") %>%
#   kableExtra::kable_styling()
# 
# #sex and age barplot (sex_age_count)
# HeartData %>%
#   mutate(age_rnd=round(age, digits=-1)) %>%
#   group_by(age_rnd, sex) %>%
#   summarize(count=n()) %>%
#   ggplot(aes(x=age_rnd, y=count, fill=sex)) +
#   geom_bar(stat="identity", position="dodge") +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text()) +
#   xlab("age (rounded)")
# 
# #sex vs age vs disease (dotplot) (sex_age_disease)
# HeartData %>%
#   group_by(sex, age) %>%
#   ggplot(aes(x=sex, y=age, fill=disease)) +
#   geom_dotplot(binaxis = "y",
#                stackdir = "center",
#                alpha=0.7,
#                binwidth = 1.5) +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text())
# 
# #CHEST PAIN TYPE
# #chest pain type barplot (cp_dist)
# HeartData %>%
#   group_by(cp) %>%
#   summarize(count=n()) %>%
#   ggplot(aes(cp, count, fill=count)) +
#   geom_bar(stat="identity") +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text(), legend.position = "none") +
#   xlab("chest pain type") +
#   ylab("count") +
#   scale_fill_gradient(low="purple", high="red")
# 
# #chest pain no/disease (barplot) (cp_disease_dist)
# HeartData %>%
#   group_by(cp, disease) %>%
#   summarize(count=n()) %>%
#   ggplot(aes(cp, count, fill=disease)) +
#   geom_bar(stat="identity", position="dodge") +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text()) +
#   xlab("chest pain type")
# 
# #chest pain agegroup barplot (cp_age_dist)
# HeartData%>%
#   mutate(age_rnd=round(age, digits=-1)) %>%
#   group_by(cp, age_rnd) %>%
#   summarize(count=n()) %>%
#   ggplot(aes(cp, count, fill=age_rnd)) +
#   geom_bar(stat="identity", position="dodge") +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text()) +
#   xlab("chest pain type")
# 
# #TRESTBPS (resting blood pressure)
# #trestbps disease barplot (trestbps_disease_dist)
# HeartData %>%
#   mutate(trestbps_rnd=round(trestbps, digits=-1)) %>%
#   group_by(trestbps_rnd, disease) %>%
#   summarize(count=n()) %>%
#   ggplot(aes(trestbps_rnd, count, fill=disease)) +
#   geom_bar(stat="identity", position="dodge")
# 
# #sex vs trestbps vs disease (dotplot) (sex_trestbps_disease_dist)
# HeartData %>%
#   mutate(trestbps_rnd=round(trestbps, digits=-1)) %>%
#   group_by(sex, trestbps_rnd) %>%
#   ggplot(aes(x=sex, y=trestbps_rnd, fill=disease)) +
#   geom_dotplot(binaxis = "y",
#                stackdir = "center",
#                alpha=0.7,
#                binwidth = 3) +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text()) +
#   ylab("resting blood pressure")
# 
# #trestbps disease age prop barplot (trestbps_disease_age)
# HeartData %>%
#   mutate(trestbps_rnd=round(trestbps, digits=-1)) %>%
#   group_by(trestbps_rnd) %>%
#   summarize(prop_disease=mean(disease=="disease"),
#             prop_age=round(mean(age), digits = 0),
#             count=n()) %>%
#   filter(count>=5) %>%
#   ggplot(aes(trestbps_rnd, prop_disease, fill=prop_age)) +
#   geom_bar(stat="identity") +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text()) +
#   xlab("resting blood pressure") +
#   ylab("proportion disease")
# 
# #chol - Serum cholesterol
# #chol count (chol_count)
# HeartData %>%
#   mutate(chol_rnd=round(chol, digits=-1)) %>%
#   group_by(chol_rnd) %>%
#   ggplot(aes(x=chol_rnd, y = ..prop.., fill="")) +
#   geom_bar() +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text(), legend.position = "none") +
#   xlab("serum cholesterol") +
#   ylab("percentage") +
#   geom_vline(xintercept = 125) +
#   geom_vline(xintercept = 200)
# 
# #chol and sex
# HeartData %>%
#   mutate(chol_rnd=round(chol, digits=-1)) %>%
#   group_by(chol_rnd, sex) %>%
#   summarize(count=n()) %>%
#   ggplot(aes(chol_rnd, count, fill=sex)) +
#   geom_bar(stat="identity", position="dodge") +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text()) +
#   xlab("serum cholesterol")
# 
# #chol and disease
# HeartData %>%
#   mutate(chol_rnd=round(chol, digits=-1)) %>%
#   group_by(chol_rnd, disease) %>%
#   summarize(count=n()) %>%
#   ggplot(aes(chol_rnd, count, fill=disease)) +
#   geom_bar(stat="identity", position="dodge") +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text()) +
#   xlab("serum cholesterol")
# 
# #age vs chol and disease
# HeartData %>%
#   ggplot(aes(age, chol, color=disease)) +
#   geom_point()
# 
# # (disease.chol.med)
# HD.chol.median.mean <- HeartData %>%
#   group_by(disease) %>%
#   summarize(me=mean(chol), med=median(chol))
# 
# HD.chol <- HeartData %>%
#   group_by(disease) %>%
#   select(disease, chol)
# 
# ggplot(data=HD.chol, aes(disease, chol, color=disease)) +
#   geom_jitter(width = 0.4, alpha = 0.3, size=4) +
#   stat_smooth(method="lm", formula=disease~1, se=FALSE) +
#   geom_hline(data=HD.chol.median.mean, aes(yintercept = med, color=disease)) +
#   xlab("Disease") +
#   ylab("Cholesterol") +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text(), legend.position = "none")
# 
# HeartData %>%
#   group_by(disease) %>%
#   summarize(mean(chol))
# 
# #fbs - Fasting blood sugar
# HeartData %>%
#   summarize(mean(fbs==">120"))
# 
# #fbs and disease (fbs_disease)
# HeartData %>%
#   group_by(fbs, disease) %>%
#   summarize(count=n()) %>%
#   ggplot(aes(fbs, count, fill=disease)) +
#   geom_bar(stat="identity", position="dodge") +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text()) +
#   xlab("fasting blood sugar (mg/dL)")
# 
# #restecg - Resting electrocardiographic results
# #restecg sum
# HeartData %>%
#   group_by(restecg, disease) %>%
#   summarize(n())
# 
# #restecg_disease barplot (restecg_disease)
# HeartData %>%
#   ggplot(aes(x=restecg,
#              y=..count..,
#              fill=disease)) +
#   geom_bar(position="dodge") +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text())
# 
# #THALACH
# HeartData %>%
#   ggplot(aes(age, thalach, color=disease)) +
#   geom_point()
# 
# #disease_thalach (disease_thalach)
# HeartData %>%
#   ggplot(aes(x=disease, y=thalach, color=disease)) +
#   geom_jitter(width = 0.3, alpha = 0.3, size=4) +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text()) +
#   ylab("thalach")
# 
# #age vs mean thalach disease (age_mean_thalach_disease)
# HeartData %>%
#   group_by(age, disease) %>%
#   summarize(count=n(), mean_thalach=mean(thalach)) %>%
#   filter(count>1) %>%
#   ggplot(aes(x=age, y=mean_thalach, color=disease)) +
#   geom_point() +
#   geom_smooth(span = 0.5) +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text()) +
#   ylab("thalach (mean)")
# 
# #age vs mean thalach sex ()
# HeartData %>%
#   group_by(age, sex) %>%
#   summarize(count=n(), mean_thalach=mean(thalach)) %>%
#   filter(count>1) %>%
#   ggplot(aes(x=age, y=mean_thalach, color=sex)) +
#   geom_point() +
#   geom_smooth(span = 0.5) +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text()) +
#   ylab("thalach (mean)")
# 
# #exang - Exercise induced angina
# #exang disease barplot (exang_disease)
# HeartData %>%
#   group_by(exang, disease) %>%
#   ggplot(aes(exang, ..count.., fill=disease)) +
#   geom_bar(position="dodge") +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text()) +
#   xlab("exercise induced angina")
# 
# #oldpeak - ST depression induced by exercise relative to rest
# #disease oldpeak geompoint(disease_oldpeak)
# HeartData %>%
#   ggplot(aes(disease, oldpeak, color=disease)) +
#   geom_jitter(width = 0.4, alpha = 0.3, size=4) +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text(), legend.position = "none") +
#   ylab("ST depression")
# 
# #mean and median of oldpeak and disease (oldpeak_mean_median)
# HeartData %>%
#   group_by(disease) %>%
#   summarize(mean=mean(oldpeak), median=median(oldpeak)) %>%
#   knitr::kable(table.attr = "style='width:30%;'") %>%
#   kableExtra::kable_styling()
# 
# #slope - Slope of peak exercise ST segment
# #slope disease barplot (slope_disease)
# HeartData %>%
#   group_by(slope, disease) %>%
#   ggplot(aes(slope, ..count.., fill=disease)) +
#   geom_bar(position="dodge") +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text()) +
#   xlab("slope of ST segment")
# 
# #slope sex disease barplot () --> MAYBE AS FACET FOR EVERY ATTRIBUTE
# HeartData %>%
#   group_by(slope, sex) %>%
#   summarize(probability=mean(disease=="disease")) %>%
#   ggplot(aes(slope, probability, fill=sex)) +
#   geom_bar(stat="identity", position="dodge") +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text()) +
#   ylab("disease (proportion)")
# 
# #ca - Major vessels colored by flourosopy
# #ca disease (ca_disease)
# HeartData %>%
#   filter(!is.na(ca)) %>%
#   group_by(ca) %>%
#   ggplot(aes(ca, ..count.., fill=disease)) +
#   geom_bar(position="dodge") +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text()) +
#   xlab("vessels colored")
# 
# # (ca_disease_mean)
# HeartData %>%
#   filter(!is.na(ca)) %>%
#   group_by(ca) %>%
#   summarize(ca_mean=mean(disease=="disease")) %>%
#   ggplot(aes(ca, ca_mean, fill=ca_mean)) +
#   geom_bar(stat="identity") +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text(), legend.position = "none") +
#   xlab("vessels colored") +
#   ylab("proportion of disease") +
#   scale_fill_gradient(low="purple", high="red")
# 
# #thal - Thalium Stress Test Result
# 
# #DISEASE
# #disease vs age (boxplot)
# HeartData %>%
#   group_by(disease, age) %>%
#   ggplot(aes(disease, age, col=disease)) +
#   geom_violin(alpha=0.3) +
#   theme_fivethirtyeight() +
#   theme(axis.title = element_text())

#find all NA values (remove later)
sum(is.na(HeartData))
HeartData %>% filter_all(any_vars(is.na(.)))
HeartData <- HeartData[complete.cases(HeartData), ]
# compl <- as.vector(complete.cases(HeartData))
# HeartDataRM <- HeartData[compl, ]

#Modeling
#setting seed
set.seed(50866) #50866

test_index <- createDataPartition(y = HeartData$disease,
                                  p = 0.8,
                                  list = FALSE)
training <- HeartData[test_index,]
testing <- HeartData[-test_index,]

levels(training$disease) <- c("disease", "no disease")
training$disease <- factor(training$disease, levels = levels(training$disease), labels = make.names(levels(training$disease)))

levels(testing$disease) <- c("disease", "no disease")
testing$disease <- factor(testing$disease, levels = levels(testing$disease), labels = make.names(levels(testing$disease)))

# #10-FOLD CROSS VALIDATION
control <- trainControl(method = "cv", 
                        number = 10, 
                        p = .9)
control.repeat <- trainControl(method = "repeatedcv", 
                               number = 10,
                               repeats = 3,
                               classProbs=TRUE,
                               savePredictions = TRUE)

#one hot encoding (Training data)
Training.dummy <- dummyVars(" ~.", data=training)
training.onehot <- data.frame(predict(Training.dummy, newdata = training))
training.onehot$disease.no.disease <- factor(training.onehot$disease.no.disease)
levels(training.onehot$disease.no.disease) <- c("disease", "no disease")
training.onehot$disease.no.disease <- factor(training.onehot$disease.no.disease, 
                                             levels = levels(training.onehot$disease.no.disease), 
                                             labels = make.names(levels(training.onehot$disease.no.disease)))
training.onehot$disease.disease <- NULL
#one hot encoding (Testing data)
Testing.dummy <- dummyVars(" ~.", data=testing)
testing.onehot <- data.frame(predict(Testing.dummy, newdata = testing))
testing.onehot$disease.no.disease <- factor(testing.onehot$disease.no.disease)
levels(testing.onehot$disease.no.disease) <- c("disease", "no disease")
testing.onehot$disease.no.disease <- factor(testing.onehot$disease.no.disease, 
                                             levels = levels(testing.onehot$disease.no.disease), 
                                             labels = make.names(levels(testing.onehot$disease.no.disease)))
testing.onehot$disease.disease <- NULL

#Logistic regression (generalized linear model)
# (Train.glm)
Train.glm <- caret::train(disease ~ ., data=training,
                   method="glm",
                   trControl=control.repeat,
                   family="binomial",
                   na.action=na.exclude)
#apply model on testing
Model.glm.roc <- predict(Train.glm, testing, type="prob")
Model.glm <- predict(Train.glm, testing, type="raw")
#confMatrix
Conf.glm <- confusionMatrix(Model.glm, testing$disease)
Conf.glm$table
Sens.glm <- Conf.glm$byClass[c("Sensitivity")]
Spec.glm <- Conf.glm$byClass[c("Specificity")]
Acc.glm <- Conf.glm$overall[["Accuracy"]]
F1.glm <- F_meas(Model.glm, testing$disease)
Prec.glm <- Conf.glm$byClass[c("Precision")]
Prev.glm <- Conf.glm$byClass[c("Prevalence")]

#DECISION TREE
#decision tree train (Train.dec.tree)
Train.dec.tree <- caret::train(disease ~ ., data=training,
                        method="rpart",
                        trControl=control.repeat,
                        tuneGrid=expand.grid(cp=0.001),
                        # tuneLength=10,
                        na.action=na.exclude)
#apply model on testing
Model.dec.tree.roc <- predict(Train.dec.tree, testing, type="prob")
Model.dec.tree <- predict(Train.dec.tree, testing, type="raw")
#confMatrix
Conf.dec.tree <- confusionMatrix(table(Model.dec.tree, testing$disease))
Conf.dec.tree$table
Sens.dec.tree <- confusionMatrix(Model.dec.tree, testing$disease)$byClass[c("Sensitivity")]
Spec.dec.tree <- confusionMatrix(Model.dec.tree, testing$disease)$byClass[c("Specificity")]
Acc.dec.tree <- confusionMatrix(Model.dec.tree, testing$disease)$overall[["Accuracy"]]
F1.dec.tree <- F_meas(Model.dec.tree, testing$disease)
Prec.dec.tree <- Conf.dec.tree$byClass[c("Precision")]
Prev.dec.tree <- Conf.dec.tree$byClass[c("Prevalence")]

#example: 90% have disease at thal=(fixed defect, reversible defect) and cp=asymptomatic); 30% of all patients have thal=(fixed defect, reversible defect), cp=asymptomatic)
fancyRpartPlot(Train.dec.tree$finalModel, sub="")

#RANDOM FOREST
#mtry: number of predictor variables per tree
#TRAIN function
Train.random.forest <- caret::train(disease ~ ., data=training,
                             method="rf",
                             preProcess=c("center","scale"),
                             tuneLength=10,
                             trControl=control.repeat,
                             na.action=na.exclude)
#apply model on testing
Model.random.forest <- predict(Train.random.forest, testing, type="raw")
Model.random.forest.roc <- predict(Train.random.forest, testing, type="prob")
Conf.random.forest <- confusionMatrix(Model.random.forest, testing$disease)
Conf.random.forest$table
Sens.random.forest <- confusionMatrix(Model.random.forest, testing$disease)$byClass[c("Sensitivity")]
Spec.random.forest <- confusionMatrix(Model.random.forest, testing$disease)$byClass[c("Specificity")]
Acc.random.forest <- confusionMatrix(Model.random.forest, testing$disease)$overall[["Accuracy"]]
F1.random.forest <- F_meas(Model.random.forest, testing$disease)
Prec.random.forest <- Conf.random.forest$byClass[c("Precision")]
Prev.random.forest <- Conf.random.forest$byClass[c("Prevalence")]

#optimize n (best_ntree)
modellist <- list()
for (ntree in c(500, 1000, 1500, 2000, 2500)) {
  fit <- caret::train(disease~., data=training, 
               method="rf", 
               metric="Accuracy",
               preProcess=c("center","scale"),
               trControl=control.repeat,
               tuneLength=10,
               # tuneGrid=expand.grid(.mtry=c(sqrt(ncol(training)))),
               ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
results <- resamples(modellist)
summary(results)

# #result of random forest
# Result.random.forest <- data.frame(Model.random.forest, testing$disease)
# plot(Result.random.forest)

# (rf.accuracy_dotplot)
dotplot(results, ylab="nTree")

# (varImp_Train.random.forest)
varImp(Train.random.forest)

# (plot_Train.random.forest)
plot(Train.random.forest$finalModel, main="train random forest")

#run best rf function (Train.random.forest.NEW)
Train.random.forest <- caret::train(disease ~ ., data=training,
                             method="rf",
                             trControl=control.repeat,
                             ntree=2000,
                             tuneLength=10,
                             tuneGrid = data.frame(mtry = sqrt(ncol(training))),
                             #default mtry for categorical response
                             na.action=na.exclude
)
Model.random.forest <- predict(Train.random.forest, testing, type="raw")
Model.random.forest.roc <- predict(Train.random.forest, testing, type="prob")
confusionMatrix(Model.random.forest, testing$disease)#$overall["Accuracy"]
#plot trees vs errors
# rafalib::mypar()
# plot(Train.random.forest)

#SUPPORT VECTOR MACHINES
# Linear Kernel (svmLinear): C (Cost)
# Polynomial Kernel (svmPoly): degree (Polynomial Degree), scale (Scale), C (Cost)
# Radial Basis Function Kernel (svmRadial): C (cost), sigma (Sigma)
# -
# C (cost): Misclassification parameter (high C: smaller-margin hyperplane/higher cost of misclassification; low C: larger-margin separating hyperplane/low cost of misclassification)
# degree (Polynomial Degree): Dimension of the model; 3 is default
# scale (Scale)
# sigma (Sigma)

#train svmLinear (train.svmLinear)
Train.svmLinear <- train(disease ~ ., data=training,
                         method = "svmLinear",
                         trControl= control.repeat,
                         preProcess=c("center",
                                      "scale"),
                         # tuneGrid=expand.grid(C=1)
                         tuneLength=5)
#apply model on testing
Model.svmLinear <- predict(Train.svmLinear, testing, type="raw")
Model.svmLinear.roc <- predict(Train.svmLinear, testing, type="prob")
Conf.svmLinear <- confusionMatrix(Model.svmLinear, testing$disease)
Conf.svmLinear$table
Sens.svmLinear <- confusionMatrix(Model.svmLinear, testing$disease)$byClass[c("Sensitivity")]
Spec.svmLinear <- confusionMatrix(Model.svmLinear, testing$disease)$byClass[c("Specificity")]
Acc.svmLinear <- confusionMatrix(Model.svmLinear, testing$disease)$overall[["Accuracy"]]
F1.svmLinear <- F_meas(Model.svmLinear, testing$disease)
Prec.svmLinear <- Conf.svmLinear$byClass[c("Precision")]
Prev.svmLinear <- Conf.svmLinear$byClass[c("Prevalence")]

#train svmPoly (cv)
Train.svmPoly <- caret::train(disease ~ ., data = training,
                       method = "svmPoly",
                       preProcess=c("scale",
                                    "center"),
                       trControl=control.repeat,
                       # tuneGrid=expand.grid(degree=1,
                       #                      scale=1,
                       #                      C=0.25)
                       tuneLength=5)
#apply model on testing
Model.svmPoly <- predict(Train.svmPoly, testing)
Conf.svmPoly <- confusionMatrix(Model.svmPoly, testing$disease)
Conf.svmPoly$table
Sens.svmPoly <- confusionMatrix(Model.svmPoly, testing$disease)$byClass[c("Sensitivity")]
Spec.svmPoly <- confusionMatrix(Model.svmPoly, testing$disease)$byClass[c("Specificity")]
Acc.svmPoly <- confusionMatrix(Model.svmPoly, testing$disease)$overall[["Accuracy"]]
F1.svmPoly <- F_meas(Model.svmPoly, testing$disease)

#train svmRadial (cv)
Train.svmRadial <- caret::train(disease ~ ., data = training,
                         method = "svmRadial",
                         preProcess=c("scale",
                                      "center"),
                         trControl=control.repeat,
                         # tuneGrid=expand.grid(C=0.25,
                         #                      sigma=0.031)
                         tuneLength=5
)
#apply model on testing
Model.svmRadial <- predict(Train.svmRadial, testing)
Conf.svmRadial <- confusionMatrix(Model.svmRadial, testing$disease)
Conf.svmRadial$table
Sens.svmRadial <- confusionMatrix(Model.svmRadial, testing$disease)$byClass[c("Sensitivity")]
Spec.svmRadial <- confusionMatrix(Model.svmRadial, testing$disease)$byClass[c("Specificity")]
Acc.svmRadial <- confusionMatrix(Model.svmRadial, testing$disease)$overall[["Accuracy"]]
F1.svmRadial <- F_meas(Model.svmRadial, testing$disease)

# K-NEAREST NEIGHBOURS
#train knn (train.knn)
Train.knn <- caret::train(disease.no.disease~., data=training.onehot,
                   method="knn",
                   trControl=control.repeat,
                   # tuneGrid=expand.grid(k=5)
                   tuneLength=3,
                   na.action=na.exclude)
Model.knn <- predict(Train.knn, testing.onehot, type="raw")
Model.knn.roc <- predict(Train.knn, testing.onehot, type="prob")
Conf.knn <- confusionMatrix(Model.knn, testing.onehot$disease)
Conf.knn$table
Sens.knn <- confusionMatrix(Model.knn, testing.onehot$disease)$byClass[c("Sensitivity")]
Spec.knn <- confusionMatrix(Model.knn, testing.onehot$disease)$byClass[c("Specificity")]
Acc.knn <- confusionMatrix(Model.knn, testing.onehot$disease)$overall[["Accuracy"]]
F1.knn <- F_meas(Model.knn, testing.onehot$disease)
Prec.knn <- Conf.knn$byClass[c("Precision")]
Prev.knn <- Conf.knn$byClass[c("Prevalence")]

ksize <- seq(5, 9, 1)
k_maxacc <- sapply(ksize, function(ks) {
  caret::train(disease.no.disease~., data=training.onehot,
        method="knn",
        trControl=control.repeat,
        tuneGrid=expand.grid(k=ks)
  )$results$Accuracy
})
qplot(ksize, k_maxacc)

#ROC-curve (roc.curve)
par(pty="s")
pROC::roc(testing$disease,
          Model.glm.roc$disease,
          plot=TRUE,
          print.auc=TRUE,
          print.auc.y=55,
          # partial.auc=c(100, 80),
          legacy.axes=TRUE,
          percent=TRUE,
          xlab="False Positive Rate",
          ylab="True Positive Rate",
          col="#32a89e",
          lwd=3)
pROC::plot.roc(testing$disease,
               Model.dec.tree.roc$disease,
               print.auc=TRUE,
               print.auc.y=50,
               percent=TRUE,
               add=TRUE,
               col="#3267a8",
               lwd=3)
pROC::plot.roc(testing$disease,
               Model.random.forest.roc$disease,
               print.auc=TRUE,
               print.auc.y=45,
               percent=TRUE,
               add=TRUE,
               col="#3d2cab",
               lwd=3)
pROC::plot.roc(testing$disease,
               Model.svmLinear.roc$disease,
               print.auc=TRUE,
               print.auc.y=40,
               percent=TRUE,
               add=TRUE,
               col="#a625b8",
               lwd=3)
pROC::plot.roc(testing.onehot$disease.no.disease,
               Model.knn.roc$disease,
               print.auc=TRUE,
               print.auc.y=35,
               percent=TRUE,
               add=TRUE,
               col="#c91e5a",
               lwd=3)
legend("bottomright",
       legend=c("log. regression", 
                "decision tree", 
                "random forest", 
                "support vector machines"
                ,"k-nearest neighbors"
                ),
       col=c("#32a89e", 
             "#3267a8", 
             "#3d2cab", 
             "#a625b8"
             ,"#c91e5a"
             ),
       lwd=3)
par(pty="m")

#Safe to csv for tableau analysis ()
write.csv(HeartData, "data/heartdata_processed.csv")

#ROC-curve (roc.curve)
par(pty="s")
pROC::roc(testing$disease,
          Model.glm.roc$disease,
          plot=TRUE,
          print.auc=TRUE,
          print.auc.x=30,
          print.auc.y=68,
          # partial.auc=c(100, 80),
          legacy.axes=TRUE,
          percent=TRUE,
          xlab="False Positive Rate",
          ylab="True Positive Rate",
          col="#32a89e",
          lwd=3)
pROC::plot.roc(testing$disease,
               Model.dec.tree.roc$disease,
               print.auc=TRUE,
               print.auc.x=30,
               print.auc.y=62,
               percent=TRUE,
               add=TRUE,
               col="#3267a8",
               lwd=3)
pROC::plot.roc(testing$disease,
               Model.random.forest.roc$disease,
               print.auc=TRUE,
               print.auc.x=30,
               print.auc.y=56,
               percent=TRUE,
               add=TRUE,
               col="#3d2cab",
               lwd=3)
pROC::plot.roc(testing$disease,
               Model.svmLinear.roc$disease,
               print.auc=TRUE,
               print.auc.x=30,
               print.auc.y=50,
               percent=TRUE,
               add=TRUE,
               col="#a625b8",
               lwd=3)
pROC::plot.roc(testing.onehot$disease.no.disease,
               Model.knn.roc$disease,
               print.auc=TRUE,
               print.auc.x=30,
               print.auc.y=44,
               percent=TRUE,
               add=TRUE,
               col="#c91e5a",
               lwd=3)
legend("bottomright",
       legend=c("log. regression", 
                "decision tree", 
                "random forest", 
                "svm"
                ,"knn"),
       col=c("#32a89e", 
             "#3267a8", 
             "#3d2cab", 
             "#a625b8"
             ,"#c91e5a"),
       lwd=3)
par(pty="m")