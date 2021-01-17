library(tidyverse)

HeartData <- read_csv("data/heart.csv")

#subset of original 76 attributes - 13 features, 1 outcome
ncol(HeartData)

#Number of instances
nrow(HeartData)

HeartData <- HeartData %>%
  mutate(sex=ifelse(sex==0, "female", "male"))

HeartData <- HeartData %>%
  mutate(target=ifelse(target==0, "disease", "no_disease"))

#factorize attributes
#age: age in years
HeartData$sex <- as.factor(HeartData$sex) #sex: (0 = female; 1 = male)
HeartData$cp <- as.factor(HeartData$cp) #cp: chest pain type (0: asymptomatic, 1: atypical angina, 2: non-anginal pain, 3: typical angina)
#trestbps: resting blood pressure (in mm/Hg on admission to the hospital)
#chol: serum cholestoral in mg/dl
HeartData$fbs <- as.factor(HeartData$fbs) #fbs: fasting blood sugar > 120 mg/dl (1=yes, 0=no)
HeartData$restecg <- as.factor(HeartData$restecg) #restecg: resting electrocardiographic results (0: showing probable or definite left ventricular hypertrophy by Estes' criteria, 1: normal, 2: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV))
#thalach: maximum heart rate achieved
HeartData$exang <- as.factor(HeartData$exang) #exang: exercise induced angina (1 = yes; 0 = no)
#oldpeak: ST depression induced by exercise relative to rest
HeartData$slope <- as.factor(HeartData$slope) #slope: the slope of the peak exercise ST segment (0: downsloping; 1: flat; 2: upsloping)
HeartData$ca <- as.factor(HeartData$ca) #ca: number of major vessels (0-3) colored by flourosopy
HeartData$thal <- as.factor(HeartData$thal) #thal: Thalium Stress Test Result (0: null, 1:fixed defect, 2= normal, 3=reversible defect), original: (3 = normal; 6 = fixed defect; 7 = reversable defect)
HeartData$target <- as.factor(HeartData$target) #target diagnosis of heart disease (angiographic disease status): 0 = disease (> 50% diameter narrowing), 1 = no disease (< 50% diameter narrowing)

HeartData %>%
  group_by(age, sex) %>%
  filter(target=="no_disease") %>%
  ggplot(aes(age, fill=sex)) +
  geom_bar(position="fill")

HeartData %>%
  group_by(sex) %>%
  summarize(n())