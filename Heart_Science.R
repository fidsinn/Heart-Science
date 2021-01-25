library(plyr)
library(tidyverse)
library(ggthemes)
library(kableExtra)
library(caret)
library(rpart)
library(rpart.plot)
#library(wesanderson)
#library(magrittr)

options(digits = 3)

HeartData <- read_csv('data/heart.csv')

#subset of original 76 attributes - 13 features, 1 outcome
ncol(HeartData)

#Number of instances
nrow(HeartData)

#factorize attributes
#age: age in years
HeartData <- HeartData %>%
  mutate(sex=ifelse(sex==0, 'female', 'male'))
HeartData$sex <- as.factor(HeartData$sex) #sex: (0 = female; 1 = male)
#HeartData$sex %<>% factor

HeartData$cp <- as.factor(HeartData$cp) #cp: chest pain type (0: asymptomatic, 1: atypical angina, 2: non-anginal pain, 3: typical angina)
HeartData$cp <- revalue(HeartData$cp, c('0'='asymptomatic', '1'='atypical angina', '2'='non anginal pain', '3'='typical angina'))

#trestbps: resting blood pressure (in mm/Hg on admission to the hospital)

#chol: serum cholesterol in mg/dl

HeartData <- HeartData %>%
  mutate(fbs=ifelse(fbs==1, '>120', '<=120'))
HeartData$fbs <- as.factor(HeartData$fbs) #fbs: fasting blood sugar > 120 mg/dl (1=yes, 0=no)

HeartData$restecg <- as.factor(HeartData$restecg) #restecg: resting electrocardiographic results (0: showing probable or definite left ventricular hypertrophy (by Estes' criteria), 1: normal, 2: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV))
HeartData$restecg <- revalue(HeartData$restecg, c('0'='left vetricular hypertrophy', '1'='normal', '2'='st-t abnormality'))

#thalach: maximum heart rate achieved

HeartData <- HeartData %>%
  mutate(exang=ifelse(exang==0, 'no', 'yes'))
HeartData$exang <- as.factor(HeartData$exang) #exang: exercise induced angina (1 = yes; 0 = no)

#oldpeak: ST depression induced by exercise relative to rest

HeartData$slope <- as.factor(HeartData$slope) #slope: the slope of the peak exercise ST segment (0: downsloping; 1: flat; 2: upsloping)
HeartData$slope <- revalue(HeartData$slope, c('0'='downsloping', '1'='flat', '2'='upsloping'))

HeartData$ca <- as.factor(HeartData$ca)#ca: number of major vessels (0-3) colored by flourosopy
HeartData$ca <- revalue(HeartData$ca, c('4'=NA))

HeartData$thal <- as.factor(HeartData$thal) #thal: Thalium Stress Test Result (0: null, 1:fixed defect, 2= normal, 3=reversible defect), original: (3 = normal; 6 = fixed defect; 7 = reversable defect)
HeartData$thal <- revalue(HeartData$thal, c('0'=NA, '1'='fixed defect', '2'='normal', '3'='reversible defect'))

HeartData <- HeartData %>%
  mutate(target=ifelse(target==0, "disease", "no disease"))
HeartData$target <- as.factor(HeartData$target) #target diagnosis of heart disease (angiographic disease status): 0 = disease (> 50% diameter narrowing), 1 = no disease (< 50% diameter narrowing)
HeartData$disease <- HeartData$target
HeartData$target <- NULL
attr(HeartData, 'spec') <- NULL
#SUMMARY
names(HeartData)
summary(HeartData)

#AGE
#summary
summary(HeartData$age)

#mean age
mean_age <- as.numeric(HeartData %>%
  summarize(mean_age=mean(age)))

#age barplot (age_dist)
HeartData %>%
  mutate(age_rnd=round(age, digits=-1)) %>%
  group_by(age_rnd) %>%
  summarize(count=n()) %>%
  ggplot(aes(age_rnd, count, fill=age_rnd)) +
  geom_bar(stat="identity") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), legend.position = "none") +
  xlab("age (rounded)")

##age vs sex no/disease barplot ()
HeartData %>%
  group_by(age, sex) %>%
  filter(disease=="no disease") %>%
  ggplot(aes(age, fill=sex)) +
  geom_bar(position="fill")

#SEX
#sex grouping (sex_count)
HeartData %>%
  group_by(sex) %>%
  summarize(Count=n()) %>%
  knitr::kable(table.attr = "style='width:30%;'") %>% 
  kableExtra::kable_styling()

#sex and age barplot (sex_age_count)
HeartData %>%
  mutate(age_rnd=round(age, digits=-1)) %>%
  group_by(age_rnd, sex) %>%
  summarize(count=n()) %>%
  ggplot(aes(x=age_rnd, y=count, fill=sex)) +
  geom_bar(stat="identity", position="dodge") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  xlab("age (rounded)")

#sex vs age vs disease (dotplot) (sex_age_disease)
HeartData %>%
  group_by(sex, age) %>%
  ggplot(aes(x=sex, y=age, fill=disease)) +
  geom_dotplot(binaxis = "y", 
               stackdir = "center", 
               alpha=0.7, 
               binwidth = 1.5) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())

#CHEST PAIN TYPE
#chest pain type barplot (cp_dist)
HeartData %>%
  group_by(cp) %>%
  summarize(count=n()) %>%
  ggplot(aes(cp, count, fill=cp)) +
  geom_bar(stat="identity") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), legend.position = "none") +
  xlab("chest pain type")

#chest pain no/disease (barplot) (cp_disease_dist)
HeartData %>%
  group_by(cp, disease) %>%
  summarize(count=n()) %>%
  ggplot(aes(cp, count, fill=disease)) +
  geom_bar(stat="identity", position="dodge") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  xlab("chest pain type")

#chest pain agegroup barplot (cp_age_dist)
HeartData%>%
  mutate(age_rnd=round(age, digits=-1)) %>%
  group_by(cp, age_rnd) %>%
  summarize(count=n()) %>%
  ggplot(aes(cp, count, fill=age_rnd)) +
  geom_bar(stat="identity", position="dodge") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  xlab("chest pain type")

#TRESTBPS (resting blood pressure)
#trestbps disease barplot (trestbps_disease_dist)
HeartData %>%
  mutate(trestbps_rnd=round(trestbps, digits=-1)) %>%
  group_by(trestbps_rnd, disease) %>%
  summarize(count=n()) %>%
  ggplot(aes(trestbps_rnd, count, fill=disease)) +
  geom_bar(stat="identity", position="dodge")

#sex vs trestbps vs disease (dotplot) (sex_trestbps_disease_dist)
HeartData %>%
  mutate(trestbps_rnd=round(trestbps, digits=-1)) %>%
  group_by(sex, trestbps_rnd) %>%
  ggplot(aes(x=sex, y=trestbps_rnd, fill=disease)) +
  geom_dotplot(binaxis = "y", 
               stackdir = "center", 
               alpha=0.7, 
               binwidth = 3) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  ylab("resting blood pressure")

#trestbps disease age prop barplot (trestbps_disease_age)
HeartData %>%
  mutate(trestbps_rnd=round(trestbps, digits=-1)) %>%
  group_by(trestbps_rnd) %>%
  summarize(prop_disease=mean(disease=="disease"), 
            prop_age=round(mean(age), digits = 0), 
            count=n()) %>%
  filter(count>=5) %>%
  ggplot(aes(trestbps_rnd, prop_disease, fill=prop_age)) +
  geom_bar(stat="identity") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  xlab("resting blood pressure") +
  ylab("proportion disease")

#chol - Serum cholesterol
#chol
HeartData %>%
  mutate(chol_rnd=round(chol, digits=-1)) %>%
  group_by(chol_rnd) %>%
  ggplot(aes(x=chol_rnd, y = ..prop.., fill="")) +
  geom_bar() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), legend.position = "none") +
  xlab("serum cholesterol") +
  ylab("percentage") +
  geom_vline(xintercept = 125) +
  geom_vline(xintercept = 200)

#chol and sex
HeartData %>%
  mutate(chol_rnd=round(chol, digits=-1)) %>%
  group_by(chol_rnd, sex) %>%
  summarize(count=n()) %>%
  ggplot(aes(chol_rnd, count, fill=sex)) +
  geom_bar(stat="identity", position="dodge") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  xlab("serum cholesterol")

#chol and disease
HeartData %>%
  mutate(chol_rnd=round(chol, digits=-1)) %>%
  group_by(chol_rnd, disease) %>%
  summarize(count=n()) %>%
  ggplot(aes(chol_rnd, count, fill=disease)) +
  geom_bar(stat="identity", position="dodge") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  xlab("serum cholesterol")

#age vs chol and disease
HeartData %>%
  ggplot(aes(age, chol, color=disease)) +
  geom_point()

#fbs - Fasting blood sugar
HeartData %>%
  summarize(mean(fbs==">120"))

#fbs and disease (fbs_disease)
HeartData %>%
  group_by(fbs, disease) %>%
  summarize(count=n()) %>%
  ggplot(aes(fbs, count, fill=disease)) +
  geom_bar(stat="identity", position="dodge") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  xlab("fasting blood sugar (mg/dL)")

#restecg - Resting electrocardiographic results  
#restecg sum
HeartData %>%
  group_by(restecg, disease) %>%
  summarize(n())

#restecg_disease barplot (restecg_disease)
HeartData %>%
  ggplot(aes(x=restecg, 
             y=..count.., 
             fill=disease)) +
  geom_bar(position="dodge") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())

#THALACH
HeartData %>%
  ggplot(aes(age, thalach, color=disease)) +
  geom_point()

#disease_thalach (disease_thalach)
HeartData %>%
  ggplot(aes(x=disease, y=thalach, color=disease)) +
  geom_jitter(width = 0.3, alpha = 0.3, size=4) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  ylab("thalach")

#age vs mean thalach disease (age_mean_thalach_disease)
HeartData %>%
  group_by(age, disease) %>%
  summarize(count=n(), mean_thalach=mean(thalach)) %>%
  filter(count>1) %>%
  ggplot(aes(x=age, y=mean_thalach, color=disease)) +
  geom_point() +
  geom_smooth(span = 0.5) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  ylab("thalach (mean)")

#age vs mean thalach sex ()
HeartData %>%
  group_by(age, sex) %>%
  summarize(count=n(), mean_thalach=mean(thalach)) %>%
  filter(count>1) %>%
  ggplot(aes(x=age, y=mean_thalach, color=sex)) +
  geom_point() +
  geom_smooth(span = 0.5) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  ylab("thalach (mean)")

#exang - Exercise induced angina
#exang disease barplot (exang_disease)
HeartData %>%
  group_by(exang, disease) %>%
  ggplot(aes(exang, ..count.., fill=disease)) +
  geom_bar(position="dodge") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  xlab("exercise induced angina")
  
#oldpeak - ST depression induced by exercise relative to rest
#disease oldpeak geompoint(disease_oldpeak)
HeartData %>%
  ggplot(aes(disease, oldpeak, color=disease)) +
  geom_jitter(width = 0.4, alpha = 0.3, size=4) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), legend.position = "none") +
  ylab("ST depression")

#mean and median of oldpeak and disease (oldpeak_mean_median)
HeartData %>%
  group_by(disease) %>%
  summarize(mean=mean(oldpeak), median=median(oldpeak)) %>%
  knitr::kable(table.attr = "style='width:30%;'") %>% 
  kableExtra::kable_styling()

#slope - Slope of peak exercise ST segment
#slope disease barplot (slope_disease)
HeartData %>%
  group_by(slope, disease) %>%
  ggplot(aes(slope, ..count.., fill=disease)) +
  geom_bar(position="dodge") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  xlab("slope of ST segment")

#slope sex disease barplot () --> MAYBE AS FACET FOR EVERY ATTRIBUTE
HeartData %>%
  group_by(slope, sex) %>%
  summarize(probability=mean(disease=="disease")) %>%
  ggplot(aes(slope, probability, fill=sex)) +
  geom_bar(stat="identity", position="dodge") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  ylab("disease (proportion)")

#ca - Major vessels colored by flourosopy
# HeartData %>%
#   filter(!is.na(ca)) %>%
#   group_by(ca) %>%
#   ggplot(aes(ca, ..prop.., group=1, fill=disease)) +
#   geom_bar(stat="count") +
#   scale_y_continuous(labels = scales::percent_format())

#ca disease (ca_disease)
HeartData %>%
  filter(!is.na(ca)) %>%
    group_by(ca) %>%
    ggplot(aes(ca, ..count.., fill=disease)) +
    geom_bar(position="dodge") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text()) +
    xlab("vessels colored")

#thal - Thalium Stress Test Result

#DISEASE
#disease vs age (boxplot)
HeartData %>%
  group_by(disease, age) %>%
  ggplot(aes(disease, age, col=disease)) +
  geom_violin(alpha=0.7) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())

HeartData %>%
  mutate(age_rnd=round(age, digits=-1)) %>%
  ggplot(aes(age, ))

#Modeling
#setting seed
set.seed(2)

test_index <- createDataPartition(y = HeartData$disease, times = 1, p = 0.2, list = FALSE)
train_set <- HeartData[-test_index,]
test_set <- HeartData[test_index,]

dec_tree_disease <- train_set %>%
  rpart(disease ~ ., data=., model=TRUE)
rpart.plot(dec_tree_disease)

dec_tree_disease_pred <- predict(dec_tree_disease, test_set, type="class")
confusionMatrix(table(dec_tree_disease_pred, test_set$disease))
# #Example for k-fold cross validation and knn using it
# #10-FOLD CROSS VALIDATION
# control <- trainControl(method = "cv", number = 10, p = .9)
# 
# #KNN train just for investigation purposes (needs long time)
# m_knn <- train_set %>%
#   train(manhattan~longitude+latitude,
#         method="knn",
#         data=.,
#         tuneGrid = data.frame(k = seq(5))
#         #trControl=control
#   )
# m_p_hat_knn <- predict(m_knn, test_set, type = "raw")
# confusionMatrix(m_p_hat_knn, test_set$manhattan)#$overall["Accuracy"]