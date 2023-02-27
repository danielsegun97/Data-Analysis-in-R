#Library to one-hot-encode
install.packages('tidyverse')
library(tidyverse)

#Read in data after install onto machine
adult <- read.csv("C:/Users/danie/Downloads/adult.data",header=F)
adult <- data.frame(adult)
colnames(adult) <- c('age', 'work-class', 'final-weight', 'education',
                     'education-number', 'marital-status', 'occupation',
                     'relationship', 'race', 'sex', 'capital-gain',
                     'capital-loss', 'hours-per-week', 'native-country',
                     'salary')

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
library(class);
adultSubset = newadult[1:5000, ];
summary(adultSubset);
n = nrow(adultSubset);
train = sample(n, n*.8);

# Scale the quantitative data
x.train.quant =scale(adultSubset[train, names(adultSubset) %in% c("final-weight", "capital-gain", 
                                                                  "capital-loss", "age")]);
x.train.qual = adultSubset[train,  !names(adultSubset) %in% c("Salary", "final-weight", "capital-gain", "capital-loss", "age")];
# Combines the above data frames together into a scaled dataframe
x.train = cbind(x.train.quant, x.train.qual);

# Scale the quantitative data based on x.train.quant
x.test.quant = scale(adultSubset[-train,  names(adultSubset) %in% c("final-weight", "capital-gain", 
                                                                    "capital-loss", "age")]
                     ,center=attr(x.train.quant, "scaled:center")
                     ,scale=attr(x.train.quant, "scaled:scale"))
x.test.qual = adultSubset[-train,  !names(adultSubset) %in% c("Salary", "final-weight", "capital-gain", "capital-loss", "age")];

# Combines the above data frames together into a scaled dataframe
x.test = cbind(x.test.quant, x.test.qual);

y.train = adultSubset[train, c("Salary")];
y.test = adultSubset[-train, c("Salary")];

# Using KNN #
set.seed(1);
for(K in c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)) {
  knn.pred = knn(train = x.train,
                 test = x.test,
                 cl = y.train,
                 k=K);
  print(mean(knn.pred != y.test));
}

# Using KNN With Cross Validation " 
x.train.quant =scale(adultSubset[train, names(adultSubset) %in% c("final-weight", "capital-gain", 
                                                                  "capital-loss", "age")]);
x.train.qual = adultSubset[train,  !names(adultSubset) %in% c("final-weight", "capital-gain", "capital-loss", "age")];
# Combines the above data frames together into a scaled dataframe
x.train = cbind(x.train.quant, x.train.qual);

# Scale the quantitative data based on x.train.quant
x.test.quant = scale(adultSubset[-train,  names(adultSubset) %in% c("final-weight", "capital-gain", 
                                                                    "capital-loss", "age")]
                     ,center=attr(x.train.quant, "scaled:center")
                     ,scale=attr(x.train.quant, "scaled:scale"))
x.test.qual = adultSubset[-train,  !names(adultSubset) %in% c("final-weight", "capital-gain", "capital-loss", "age")];

# Combines the above data frames together into a scaled dataframe
x.test = cbind(x.test.quant, x.test.qual);

y.train = adultSubset[train, c("Salary")];
y.test = adultSubset[-train, c("Salary")];
testing = cbind(x.test, y.test);
training = cbind(x.train, y.train);
install.packages("caret");
library(caret)
ctrl <- trainControl(method="cv",number = 5) #number: how many fold
fit.knn <- train(Salary ~ ., data = training,
                 method = "knn",
                 trControl = ctrl,
                 tuneGrid = expand.grid(k = c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)),
                 preProcess = c("center","scale"))
fit.knn;
# From this data it appears K=7 is the optimal value with an accuracy of 0.9105730 = 91.06%

