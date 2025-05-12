################################################################################
#
# HTELFinal.R
#
################################################################################
# External Functions
################################################################################
library(smallstuff)
library(data.table)
library(ISLR)
library(MASS)        
library(sur)
library(AER)
library(glmnet)
library(boot)
library(leaps)
################################################################################
# Internal Functions
################################################################################
source("smallstuff2.R")
################################################################################
# Save the environment 
################################################################################
parSave=par(no.readonly = TRUE)
#par(parSave)
################################################################################
# Processing 
################################################################################
#1
#loading dataset
data("USSeatBelts")
?USSeatBelts
dim(USSeatBelts)

#converting dataset into data table
setDT(USSeatBelts)[]

#looking at summary of dataset
summary(USSeatBelts)

#the variable seatbelt has missing values, we will find the percentage of missing
#data in the dataset:
mean(!complete.cases(USSeatBelts)) * 100
#because over a 5th of our dataset has some form of missing value, we will impute
#missing values using regression

#visualization of missing cases
pie_var = c(27.3, 72.7)
pie(pie_var, labels = c('Missing', 'Intact'), col=c('orange', 'blue'),
    main = "Percentage of Missing Values in Our Data")

#copying data in case we mess up
seatbelt2 = copy(USSeatBelts)

seatbelt2[is.na(seatbelt)]
#there don't seem to be multiple NAs in the same rows
lmodr = lm(seatbelt~state+year+miles+speed65+speed70+
             drinkage+alcohol+income+age, seatbelt2)
seatbelt2[is.na(seatbelt), seatbelt:=round2(predict(lmodr, USSeatBelts[is.na(seatbelt)]),2)]
seatbelt2$seatbelt
summary(seatbelt2)
mean(!complete.cases(seatbelt2)) *100
#no more missing values

USSeatBelts = seatbelt2

#getting rid of variables we are not interested in
#we will get rid of the variables 'state', 'speed70', 'drinkage', and 'income':
(vars = c('state', 'speed70', 'drinkage', 'income'))

USSeatBelts[, (vars):=NULL][]

#looking at classes of variables
USSeatBelts[, lapply(.SD, class)]
#making sure factor variables all go the same way
summary(USSeatBelts)

#all factors go from no to yes, but for the variable enforce we will 
#combine the levels 'primary' and 'secondary' into 'yes', making it
#dichotomous
levels(USSeatBelts$enforce) = list(no = 'no',
                                   yes = c('secondary', 'primary'))

#one other thing is that the variable 'year' is listed as a factor variable. we
#will change it to numeric instead
USSeatBelts[, year:= as.numeric(year)][]

#we are going to create a classification model for the dichotomous variable
#'enforce'
#predictors are 'year', 'miles', 'seatbelt', 'speed65', 'drinkage', 
#'alcohol', 'age', and 'fatalities'

#2
#splitting into training and testing set, split 70% train and 30% test
set.seed(47L)
tr = sample(765, 536)
SeatbeltTrain = USSeatBelts[tr]
SeatbeltTest = USSeatBelts[-tr]

#3
summary(USSeatBelts)
#the dataset is called 'Effects of Mandatory Seat Belt Laws in the US', and has
#data from 1983-1997 from all 50 US states + District of Columbia of traffic
#fatalities and seat belt usage

#we found this dataset interesting because seat belts have become such a staple
#of driving safety

#the original dataset has 765 observations and 12 variables:
#state- factor of US state
#year- factor of year
#miles- traffic miles per year (millions)
#fatalities- number of fatalities per million miles
#seatbelt- seat belt usage rate, self reported
#speed65- factor indicating whether or not there is a 65 mph speed limit
#speed70- factor indicating whether or not there is a 70+ mph speed limit
#drinkage- factor indicating whether or not there is a minimum drinking age of
#21 years old
#alcohol- factor indicating whether or not there is a maximum of 0.08 bac
#income- median per captia income (US dollar)
#age- mean age
#enforce- factor indicating seat belt law enforcement

#4
#we want to create a classification model on whether or not there is law
#enforcement on seatbelts

#logistic regression with full model
gmodfull = glm(enforce~., binomial, SeatbeltTrain)
summary(gmodfull)
#some predictors are not significant, like fatalities and speed65yes

#using best subset selection to try for best model
model.matrix(gmodfull, SeatbeltTrain)
subs=regsubsets(enforce~., SeatbeltTrain,nvmax=6)
(subsum = summary(subs))

#plotting BIC value and finding best d
plot(subsum$bic,xlab="Number of Variables",ylab="BIC",type='b', 
     main = "Best Number of Variables")
(m=which.min(subsum$bic))
#best d appears to be 6
#x(6.010671), y(-466.9049)
points(x=6.010671, y=-466.9049, pch='x', col='blue', cex=2)

#best 6 variables
coef(subs,6)
#our best 4 variables appear to be year, miles, seatbelt, speed65, alcohol 
#and age

#logistic model with these 6
gmodsub = glm(enforce~year+miles+seatbelt+speed65+alcohol+age, 
              binomial, SeatbeltTrain)
summary(gmodsub)
#everything is significant, except speed65, so we will make new model 
#without it
gmodsub = update(gmodsub, ~.-speed65, SeatbeltTrain)
summary(gmodsub)
#now everything is significant

#now going to create qda lda and knn models with these 6 best predictors
#to see what model can be best

#lda model
(ldamod = lda(enforce~year+miles+seatbelt+speed65+alcohol+age, SeatbeltTrain))
#according to this model, 39.9% of the time, there were no seatbelt laws
#being enforced, while 60.1% of the time, there were seatbelt laws being
#enforced.

#barplot on ldamod counts
barplot(ldamod$counts, ylab='Counts', xlab= 'Are Seatbelts Enforced?',
        main = 'Counts of LDA Model', col = c('orange', 'blue'))

#qda model
(qdamod = qda(enforce~year+miles+seatbelt+speed65+alcohol+age, SeatbeltTrain))
#got same prior probabilities as lda model, meaning once again that 
#39.9% of the time, there were no seatbelt laws being enforced, while 
#60.1% of the time, there were seatbelt laws being enforced.

#KNN model
kmod = KNN(enforce~year+miles+seatbelt+speed65+alcohol+age, SeatbeltTrain)

#dummy classifier
SeatbeltTrain[,summary(enforce)/.N]
#Predict enforcement of seat belts
SeatbeltTest[,mean(enforce!="yes")]
#error rate for dummy model is 34.5%, so all of our other models are better

#10-fold cross validation on training models
#gmodfull
set.seed(47L)
CVerror(gmodfull,10)  
#full logistic model has a CV error rate of 12.3

#gmodsub
set.seed(47L)
CVerror(gmodsub,10)  
#selected best subset logistic model has a CV error rate of 12.1

#lda
set.seed(47L)
CVerror(ldamod,10)  
#lda model has a CV error rate of 12.1

#qda
set.seed(47L)
CVerror(qdamod,10)  
#qda model has a CV error rate of 11.6

#10 fold CV from k 1-150
knnerr=NULL
for (i in 1:150) {
  set.seed(47L); knnerr[i]=CVerror(kmod,10,K=i)
}
knnerr
(m=min(knnerr))
which(knnerr==m)
#the best k seems to be k=14

plot(knnerr,type='b', main = "CV Error KNN")

CV_er = c(12.3, 12.1, 12.1, 11.6, 29.1)
barplot(CV_er, names.arg = c('Full', 'Sub', 'LDA', 'QDA', 'KNN'),
        xlab = 'Models', ylab = '% Error', main = 'Cross Validation Error Rates of 
        Machine Learning Models',
        col = c('blue3', 'blue4', 'darkorange', 'deepskyblue3', 'darkslateblue'))

#our qda model has the best CV error rate out of all of our models. 
#however, these are all based on training data. We will now find testing 
#testing error rates of our models, as well as sensitivity, specificity,
#and false neg/positive

#gmodfull
(lergmodfull = logistErrorRate(gmodfull,SeatbeltTest))
lergmodfull$errorRate
#Predicted error rate 11.8%
lergmodfull$result[2,2]/lergmodfull$result[3,2]  #sensitivity=88.7%
lergmodfull$result[1,1]/lergmodfull$result[3,1]  #Specificity=87.3%
lergmodfull$result[1,2]/lergmodfull$result[3,2] #false neg rate = 11.33%

#gmodsub
(lergmodsub = logistErrorRate(gmodsub,SeatbeltTest))
lergmodsub$errorRate
#Predicted error rate 11.4%
lergmodsub$result[2,2]/lergmodsub$result[3,2]  #sensitivity=88.7%
lergmodsub$result[1,1]/lergmodsub$result[3,1]  #Specificity=88.6%
lergmodsub$result[1,2]/lergmodsub$result[3,2] #false neg rate = 11.3%

#lda model
predlda=predict(ldamod,SeatbeltTest)
mean(predlda$class!=SeatbeltTest$enforce)*100
#Predicted error rate 10.0%
addmargins(table(Fitted=predlda$class,Response=SeatbeltTest$enforce))
136/150   #sensitivity=90.7%
70/79     #specificity=88.6%
14/150    #false neg=9.3%
9/79     #false pos=11.4%
136/145   #precision=93.8%
(78 + 136)/229  #accuracy=93.4%

#qda model
predqda=predict(qdamod,SeatbeltTest)
mean(predqda$class!=SeatbeltTest$enforce)*100
#Predicted error rate 12.7%
addmargins(table(fitted=predqda$class,response=SeatbeltTest$enforce))
130/150   #sensitivity=86.7%
70/79     #specificity=88.6%
20/130    #false neg=15.4%
9/79     #false pos=11.4%

#KNN
predk=predict(kmod,SeatbeltTest,K=14)
mean(predk$class!=SeatbeltTest$enforce)*100
#Predicted error rate 32.3%, awful error rate compared to others so did not
#bother with getting sensitivity etc

#visualization of testing error
test_error = c(11.8, 11.4, 10, 12.7, 32.3)
barplot(test_error, names.arg = c('Full', 'Sub', 'LDA', 'QDA', 'KNN'),
        xlab = 'Models', ylab = '% Error', main = 'Testing Error Rates of Machine Learning Models',
        col = c('blue3', 'blue4', 'darkorange', 'deepskyblue3', 'darkslateblue'))
#because it has the lowest testing error rate at 10.0%, we will select our
#lda model

#model in context: our model has a sensitivity of 90.7%, meaning that it 
#correctly identified the presence of seat belt law enforcement 90.7% of the
#time. It has a specificity of 88.6%, meaning that it correctly identified 
#the lack of seat belt law enforcement 88.6% of the time. Our model's false
#positive rate of 11.4% means that it incorrectly predicted the presence of
#seat belt laws 11.4% of the time, while its false negative rate of 9.3%
#means that it incorrectly predicted the lack of seat belt law enforcement
#9.3% of the time. The precision of 93.8% indicates that out of all the
#data, the model was able to correctly identify the lack of seat belt laws
#93.8% of the time. The overall accuracy of the model is 93.4%, meaning that
#93.4% of the total predictions were correct















