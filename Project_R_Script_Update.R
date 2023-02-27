#Library to one-hot-encode

library(tidyverse)

#Read in data after install onto machine

adult <- read.csv("C:/Users/danie/Downloads/adult.data",header=F)
adult <- data.frame(adult)
colnames(adult) <- c('age', 'work-class', 'final-weight', 'education',
                     'education-number', 'marital-status', 'occupation',
                     'relationship', 'race', 'sex', 'capital-gain',
                     'capital-loss', 'hours-per-week', 'native-country',
                     'salary')

#Data set is too big, so only keep around 5000 observations, randomly.
set.seed(1)
kept <- sample(nrow(adult),
                5000)
adult <- adult[kept,]

#Get rid of the "education" variable, since the "education-number" variable
#means the same thing.
newadult <- adult[,-4]

#Get rid of any rows that have "?" column values (because they are missing
#values). Only the "work-class", "occupation", and "native-country" variables
#have missing values.
newadult <- newadult[newadult$`work-class` != ' ?' & newadult$occupation != ' ?'
                     & newadult$`native-country` != ' ?',]

#"education-number" is a categorical variable in 
#numerical form, with an implicit ordering, so no need to one-hot-encode.

#One-hot-encode variable "work-class".
newadult <- mutate(newadult, `Private` =
                     ifelse(`work-class` == " Private", 1, 0))
newadult <- mutate(newadult, `Self-Emp-Not-Inc` =
                     ifelse(`work-class` == " Self-emp-not-inc", 1, 0))
newadult <- mutate(newadult, `Self-Emp-Inc` =
                     ifelse(`work-class` == " Self-emp-inc", 1, 0))
newadult <- mutate(newadult, `Federal-Gov` =
                     ifelse(`work-class` == " Federal-gov", 1, 0))
newadult <- mutate(newadult, `State-Gov` =
                     ifelse(`work-class` == " State-gov", 1, 0))
newadult <- mutate(newadult, `Local-Gov` =
                     ifelse(`work-class` == " Local-gov", 1, 0))
newadult <- mutate(newadult, `Without-Pay` =
                     ifelse(`work-class` == " Without-pay", 1, 0))
#Do not need to one-hot-encode "work-class" if the value is
#"Never-worked", as there would be no listed occupation for
#such an observation, so it was deleted already.

#One-hot-encode variable "marital-status".
newadult <- mutate(newadult, `Married-Civ-Spouse` =
                     ifelse(`marital-status` == " Married-civ-spouse", 1, 0))
newadult <- mutate(newadult, `Divorced` =
                     ifelse(`marital-status` == " Divorced", 1, 0))
newadult <- mutate(newadult, `Never-Married` =
                     ifelse(`marital-status` == " Never-married", 1, 0))
newadult <- mutate(newadult, `Separated` =
                     ifelse(`marital-status` == " Separated", 1, 0))
newadult <- mutate(newadult, `Widowed` =
                     ifelse(`marital-status` == " Widowed", 1, 0))
newadult <- mutate(newadult, `Married-Spouse-Absent` =
                     ifelse(`marital-status` == " Married-spouse-absent", 1, 0))
newadult <- mutate(newadult, `Married-AF-Spouse` =
                     ifelse(`marital-status` == " Married-AF-spouse", 1, 0))

#One-hot-encode variable "occupation".
newadult <- mutate(newadult, `Tech-Support` =
                     ifelse(`occupation` == " Tech-support", 1, 0))
newadult <- mutate(newadult, `Craft-Repair` =
                     ifelse(`occupation` == " Craft-repair", 1, 0))
newadult <- mutate(newadult, `Other-Service` =
                     ifelse(`occupation` == " Other-service", 1, 0))
newadult <- mutate(newadult, `Sales` =
                     ifelse(`occupation` == " Sales", 1, 0))
newadult <- mutate(newadult, `Exec-Managerial` =
                     ifelse(`occupation` == " Exec-managerial", 1, 0))
newadult <- mutate(newadult, `Prof-Specialty` =
                     ifelse(`occupation` == " Prof-specialty", 1, 0))
newadult <- mutate(newadult, `Handlers-Cleaners` =
                     ifelse(`occupation` == " Handlers-cleaners", 1, 0))
newadult <- mutate(newadult, `Machine-Op-Inspct` =
                     ifelse(`occupation` == " Machine-op-inspct", 1, 0))
newadult <- mutate(newadult, `Adm-Clerical` =
                     ifelse(`occupation` == " Adm-clerical", 1, 0))
newadult <- mutate(newadult, `Farming-Fishing` =
                     ifelse(`occupation` == " Farming-fishing", 1, 0))
newadult <- mutate(newadult, `Transport-Moving` =
                     ifelse(`occupation` == " Transport-moving", 1, 0))
newadult <- mutate(newadult, `Priv-House-Serv` =
                     ifelse(`occupation` == " Priv-house-serv", 1, 0))
newadult <- mutate(newadult, `Protective-Serv` =
                     ifelse(`occupation` == " Protective-serv", 1, 0))
newadult <- mutate(newadult, `Armed-Forces` =
                     ifelse(`occupation` == " Armed-Forces", 1, 0))

#One-hot-encode variable "relationship".
newadult <- mutate(newadult, `Wife` =
                     ifelse(`relationship` == " Wife", 1, 0))
newadult <- mutate(newadult, `Own-Child` =
                     ifelse(`relationship` == " Own-child", 1, 0))
newadult <- mutate(newadult, `Husband` =
                     ifelse(`relationship` == " Husband", 1, 0))
newadult <- mutate(newadult, `Not-In-Family` =
                     ifelse(`relationship` == " Not-in-family", 1, 0))
newadult <- mutate(newadult, `Other-Relative` =
                     ifelse(`relationship` == " Other-relative", 1, 0))
newadult <- mutate(newadult, `Unmarried` =
                     ifelse(`relationship` == " Unmarried", 1, 0))

#One-hot-encode variable "race".
newadult <- mutate(newadult, `White` =
                     ifelse(`race` == " White", 1, 0))
newadult <- mutate(newadult, `Asian-Pac-Islander` =
                     ifelse(`race` == " Asian-Pac-Islander", 1, 0))
newadult <- mutate(newadult, `Amer-Indian-Eskimo` =
                     ifelse(`race` == " Amer-Indian-Eskimo", 1, 0))
newadult <- mutate(newadult, `Black` =
                     ifelse(`race` == " Black", 1, 0))
newadult <- mutate(newadult, `Other` =
                     ifelse(`race` == " Other", 1, 0))

#One-hot-encode variable "native-country".
newadult <- mutate(newadult, `United-States` =
                     ifelse(`native-country` == " United-States", 1, 0))
newadult <- mutate(newadult, `Cambodia` =
                     ifelse(`native-country` == " Cambodia", 1, 0))
newadult <- mutate(newadult, `England` =
                     ifelse(`native-country` == " England", 1, 0))
newadult <- mutate(newadult, `Puerto-Rico` =
                     ifelse(`native-country` == " Puerto-Rico", 1, 0))
newadult <- mutate(newadult, `Canada` =
                     ifelse(`native-country` == " Canada", 1, 0))
newadult <- mutate(newadult, `Germany` =
                     ifelse(`native-country` == " Germany", 1, 0))
newadult <- mutate(newadult, `Outlying-US` =
                     ifelse(`native-country` == " Outlying-US(Guam-USVI-etc)", 1, 0))

newadult <- mutate(newadult, `India` =
                     ifelse(`native-country` == " India", 1, 0))
newadult <- mutate(newadult, `Japan` =
                     ifelse(`native-country` == " Japan", 1, 0))
newadult <- mutate(newadult, `Greece` =
                     ifelse(`native-country` == " Greece", 1, 0))
newadult <- mutate(newadult, `South` =
                     ifelse(`native-country` == " South", 1, 0))
newadult <- mutate(newadult, `China` =
                     ifelse(`native-country` == " China", 1, 0))
newadult <- mutate(newadult, `Cuba` =
                     ifelse(`native-country` == " Cuba", 1, 0))
newadult <- mutate(newadult, `Iran` =
                     ifelse(`native-country` == " Iran", 1, 0))
newadult <- mutate(newadult, `Honduras` =
                     ifelse(`native-country` == " Honduras", 1, 0))
newadult <- mutate(newadult, `Philippines` =
                     ifelse(`native-country` == " Philippines", 1, 0))
newadult <- mutate(newadult, `Italy` =
                     ifelse(`native-country` == " Italy", 1, 0))
newadult <- mutate(newadult, `Poland` =
                     ifelse(`native-country` == " Poland", 1, 0))
newadult <- mutate(newadult, `Jamaica` =
                     ifelse(`native-country` == " Jamaica", 1, 0))
newadult <- mutate(newadult, `Vietnam` =
                     ifelse(`native-country` == " Vietnam", 1, 0))
newadult <- mutate(newadult, `Mexico` =
                     ifelse(`native-country` == " Mexico", 1, 0))
newadult <- mutate(newadult, `Portugal` =
                     ifelse(`native-country` == " Portugal", 1, 0))
newadult <- mutate(newadult, `Ireland` =
                     ifelse(`native-country` == " Ireland", 1, 0))
newadult <- mutate(newadult, `France` =
                     ifelse(`native-country` == " France", 1, 0))
newadult <- mutate(newadult, `Dominican-Republic` =
                     ifelse(`native-country` == " Dominican-Republic", 1, 0))
newadult <- mutate(newadult, `Laos` =
                     ifelse(`native-country` == " Laos", 1, 0))
newadult <- mutate(newadult, `Ecuador` =
                     ifelse(`native-country` == " Ecuador", 1, 0))
newadult <- mutate(newadult, `Taiwan` =
                     ifelse(`native-country` == " Taiwan", 1, 0))
newadult <- mutate(newadult, `Haiti` =
                     ifelse(`native-country` == " Haiti", 1, 0))
newadult <- mutate(newadult, `Columbia` =
                     ifelse(`native-country` == " Columbia", 1, 0))
newadult <- mutate(newadult, `Hungary` =
                     ifelse(`native-country` == " Hungary", 1, 0))
newadult <- mutate(newadult, `Guatemala` =
                     ifelse(`native-country` == " Guatemala", 1, 0))
newadult <- mutate(newadult, `Nicaragua` =
                     ifelse(`native-country` == " Nicaragua", 1, 0))
newadult <- mutate(newadult, `Scotland` =
                     ifelse(`native-country` == " Scotland", 1, 0))
newadult <- mutate(newadult, `Thailand` =
                     ifelse(`native-country` == " Thailand", 1, 0))
newadult <- mutate(newadult, `Yugoslavia` =
                     ifelse(`native-country` == " Yugoslavia", 1, 0))
newadult <- mutate(newadult, `El-Salvador` =
                     ifelse(`native-country` == " El-Salvador", 1, 0))
newadult <- mutate(newadult, `Trinadad&Tobago` =
                     ifelse(`native-country` == " Trinadad&Tobago", 1, 0))
newadult <- mutate(newadult, `Peru` =
                     ifelse(`native-country` == " Peru", 1, 0))
newadult <- mutate(newadult, `Hong` =
                     ifelse(`native-country` == " Hong", 1, 0))
newadult <- mutate(newadult, `Holand-Netherlands` =
                     ifelse(`native-country` == " Holand-Netherlands", 1, 0))

#Delete variables that have been one-hot-encoded
newadult <- newadult[,-c(2,5,6,7,8,13)]

#Change "salary" to binary numeric categorical variable, 1 if salary >50k
#and 0 otherwise. Delete original "salary" variable.
newadult <- mutate(newadult, `Salary` = 
                     ifelse(`salary` == " >50K", 1, 0))
newadult <- newadult[,-8]

#Change "sex" to binary numeric categorical variable, 1 if male
#and 0 otherwise.
newadult$sex <- ifelse(newadult$sex == ' Male', 1, 0)

#Make the one-hot-encoded dummy variables, as well as
#the categorical variables, factors
factors <- c(3,4,8:ncol(newadult))
for (i in factors) {
  newadult[,i] <- as.factor(newadult[,i])
}

#The data will need to be scaled since final-weight, capital-gain, and
#capital-loss are all variables with values in the thousands. The
#scaling will be done when subdividing the data into a training set
#and a testing set.

#Making a scaled training set subdivision and scaled testing set subdivision.
set.seed(1)
train <- sample(nrow(newadult),
                0.8*nrow(newadult))
trainSalary <- newadult[train,88]
testSalary <- newadult[-train,88]
newadult.train <- newadult[train,-88]
newadult.train.num <- scale(newadult.train[,c(1,2,5,6,7)])
newadult.train.fact <- newadult.train[,-c(1,2,5,6,7)]
newadult.train <- data.frame(newadult.train.num, newadult.train.fact,
                             trainSalary)

newadult.test <- newadult[-train,-88]
centers = c(0,0,0,0,0)
scales = c(0,0,0,0,0)
for (i in 1:5) {
  centers[i] = attr(newadult.train.num, "scaled:center")[i]
  scales[i] = attr(newadult.train.num, "scaled:scale")[i]
}
newadult.test.num <- scale(newadult.test[,c(1,2,5,6,7)],
                           center = centers,
                           scale = scales)
newadult.test.fact <- newadult.test[,-c(1,2,5,6,7)]
newadult.test <- data.frame(newadult.test.num, newadult.test.fact,
                            testSalary)

#SVM will not work when the data contains predictors that are factors with only
#one unique value. The predictors for which this is the case are 'Outlying.US',
#'Greece', and 'Holand.Netherlands'. Thus, get rid of these predictors from 
#the training and testing sets.
newadult.train <- subset(newadult.train,select=-c(Outlying.US, Greece,
                                                  Holand.Netherlands))
newadult.test <- subset(newadult.test,select=-c(Outlying.US, Greece,
                                                  Holand.Netherlands))






#DOING SVM ON THE BEST MODEL BASED ON THE PLOTS - PART C AND D
quant = scale(newadult[,c(1,2,5,6,7)])
cat = newadult[,-c(1,2,5,6,7)]
fulldata = data.frame(quant,cat,newadult$Salary)
fulldata = fulldata[,-89]

fulldata = fulldata[,-c(35,53,87)]


#SVM Based on the best 
library(e1071)

svmfit = svm(Salary~., data = fulldata, kernel="polynomial", degree=2, cost=100)
summary(svmfit)

plot(svmfit, fulldata, age~final.weight)
plot(svmfit, fulldata, age~capital.gain, xlim = c(-0.1,4))
plot(svmfit, fulldata, age~capital.loss, xlim=c(-0.1,8))
plot(svmfit, fulldata, age~hours.per.week)

plot(svmfit, fulldata, final.weight~capital.gain, xlim=c(-0.1,4))
plot(svmfit, fulldata, final.weight~capital.loss)
plot(svmfit, fulldata, final.weight~hours.per.week)

plot(svmfit, fulldata, capital.gain~hours.per.week)


plot(svmfit, fulldata, capital.loss~hours.per.week)
for (i in 30:ncol(fulldata)){
  print(i)
  print(table(fulldata[,i]))
}

