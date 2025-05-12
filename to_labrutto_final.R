################################################################################
#
# to_labrutto_final.R
#
################################################################################
# External Functions
################################################################################
library(dslabs)
library(smallstuff)
options(rgl.useNULL = TRUE)
library(rgl)
library(matlib)
library(Matrix)
library(faraway)
library(broom)
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
data(us_contagious_diseases)
?us_contagious_diseases
summary(us_contagious_diseases)
dim(us_contagious_diseases)
head(us_contagious_diseases)

#use measles
data = us_contagious_diseases[which(us_contagious_diseases$disease=="Measles"),
                              names(us_contagious_diseases) %in% c('disease', 'state', 
                                                                   'year','weeks_reporting',
                                                                   'count', 'population')]
head(data)
dim(data)

#reindexing data so that it starts at index 1
rownames(data) <- 1:nrow(data)
head(data)

#going to divide population by 100,000 to scale it down and make it more comparable
#to the other variables

data$population = data$population/100000
head(data)

#our dataset has contagious disease data for US states. For our project, we are only
#going to look at the disease Measles. It was chosen due to our shared interest in
#its content and we accessed it through the dslabs package, and each row is a 
#state in specific year

#variables:
#disease- a factor of disease names. we will use Measles only
#state- a factor containing state names
#year
#weeks_reporting- the number of weeks that counts were reported that year
#count- total number of reported disease cases
#population- state population that is interpolated for non-census years

#2
#checking for errors/null data
sum(is.na(data))

#these are rows with null values
(nullvalues = which(is.na(data)==TRUE))
length(nullvalues) / nrow(data)
#the number of null rows makes up 1.67% of our total data


#making a visualization of null vs not null data
null_area = (length(nullvalues) / nrow(data)) * 100
not_null = 100 - null_area
pie_data = c(null_area, not_null)
labels = c("Null Data", "Existing Data")
pie(pie_data, labels, col = c("yellow", "purple"), 
    main = "Null vs. Existing Data in Dataset")

#dropping na values
data = na.omit(data)
dim(data)
sum(is.na(data))

#in summary: we checked for null values, evaluated what percent they made up of 
#our total dataset, then omitted them. Our data now has 0 null values

#5
#creating linear models of our data
#1st linear model-our response variable is going to be the count variable, 
#which is the number of reported cases. our predictor variables will be year, 
#weeks_reporting, and population

lmod1 = lm(count~year+weeks_reporting+population, data)
summary(lmod1)
coef(lmod1)

#model: count = 448236.8 - 227.4year + 4.7weeks_reporting + 89.0population + eps
#model in context: for each one year passing, there is a decrease of 227.4 total
#Measles cases, if all other predictors are held constant. for each one week 
#increase in weeks reporting counts, there is an increase of 4.7 reported 
#Measles cases in the total count, when all other variables are held constant. 
#for each increase in population, there is an increase of 89.0 total Measles 
#cases, when all other predictors are held constant
#lmod1 r-squared value: 24.57%
glance(lmod1)$p.value
#lmod1 p value: 4.788233e-230 


#2nd linear model- our response variable is going to be the count variable, 
#which is the number of reported cases.our predictor variables will be  
#weeks_reporting, and population

lmod2 = update(lmod1, ~.-year)
summary(lmod2)
coef(lmod2)
#model: count = -3816.2 + 170.4weeks_reporting + 57.9population + eps
#model in context: for each one week increase in weeks reporting counts, there is 
#an increase of 170.4 reported Measles cases in the total count, when all other 
#variables are held constant. for each increase in population, there is an 
#increase of 57.9 total reported Measles cases, when all other predictors are 
#held constant.
#lmod2 r-squared value: 12.9%
glance(lmod2)$p.value
#lmod2 p value: 4.763595e-114 


#3rd linear model- our response variable is going to be the count variable, 
#which is the number of reported cases.our predictor variable will be  
#weeks_reporting

lmod3 = update(lmod2, ~.-population)
summary(lmod3)
coef(lmod3)
#model: count = -2100.5 + 183.6weeks_reporting + eps
#lmod3 r-squared value: 8.1%
#model in context: for each one week increase in weeks reporting counts, there is 
#an increase of 183.6 total reported Measles cases in the total count, when all 
#other variables are held constant.
glance(lmod3)$p.value
#lmod3 p value: 8.284193e-71 

#4
#checking for identifiability/linear independence
X = model.matrix(lmod1)
head(X)
#checking for linear independence
rankMatrix(X)[[1]]
ncol(X)
#our main linear model is identifiable, its rank is equal to number of columns

X2 = model.matrix(lmod2)
rankMatrix(X2)[[1]]
ncol(X2)
#same for lmod2

X3 = model.matrix(lmod3)
rankMatrix(X3)[[1]]
ncol(X3)

#6
#hypothesis testing- we will perform a t test on each parameter to see if they
#are zero or not (if they are zero, they have no effect on the model). we are 
#trying to see if it is effective to use lmod1, or the linear model with all 
#three parameters

#stating the hypothesis: 
#H0- beta1 year = 0
#H1- beta1 year != 0

#H0- beta2 weeks_reporting = 0
#H1- beta2 weeks_reporting != 0

#H0- beta3 population = 0
#H1- beta3 population != 0

summary(lmod1)
#according to the summary of our model, there is enough evidence that the 
#coefficients year and population do not equal 0 (p < 2.2e^-16 for both), so we
#reject the null hypothesis. however, the coefficient weeks_reporting does not
#have enough evidence that it does not equal 0 (p = 0.683), so we retain the null
#hypothesis that it is likely weeks_reporting equals 0

#our original models all include weeks_reporting, so we created a 'new' model
#that removed weeks_reporting and checked its r-squared score:
lmod1new = update(lmod1, ~.-weeks_reporting, data)
summary(lmod1new)
coef(lmod1new)

#r-squared of 24.59%
summary(lmod1)
#r-squared of 24.57%
glance(lmod1new)$p.value
#p value of lmod1new: 1.859321e-231 
Xnew = model.matrix(lmod1new)
head(X)
#checking for linear independence
rankMatrix(X)[[1]]
ncol(X)


#we decided it would not hurt the r-squared score to remove weeks_reporting,
#so we will proceed with this new linear model 1 adjustment

#lmod1new model: count = 452927.8 - 229.7year + 89.5population + eps
#model in context: for each one year passing, there is a decrease of 229.7 total
#Measles cases, if all other predictors are held constant. for each increase in 
#population, there is an increase of 89.5 total Measles cases, when all other 
#predictors are held constant

#visualization of r-squared values
values = c(24.6, 12.9, 8.1)
barplot(values, col = c("red", "blue", "green"), 
        names = c("Linear Model 1", "Linear Model 2", "Linear Model 3"),
        main = "R-Squared Values of Linear Models", 
        ylab = "% of Variance Explained by Model", ylim = c(5,30), xpd = FALSE)

#7
#scatterplots of count vs each lm variable
plot(count~weeks_reporting, data=data, xlab="Number of Weeks Reporting",
     ylab="Number of Cases", main="Count vs Weeks Reporting", col="red", 
     pch=19)

plot(count~population, data=data, xlab="State Population (Thousands)",
     ylab="Number of Cases", main="Count vs Population", col="blue", 
     pch=19)

plot(count~year, data=data, xlab="Year",
     ylab="Number of Cases", main="Count vs Year", col="green", 
     pch=19)

#residual plots, will use standardized residuals due to all having high leverage
#points
plot(rstandard(lmod1new)~fitted(lmod1new),xlab="Fitted values", 
     ylab="Standardized Residuals", main="Linear Model 1 Standardized Residual Plot")
#huge problem: funnel shape
which(rstandard(lmod1new)>10)

plot(rstandard(lmod2)~fitted(lmod2),xlab="Fitted values", 
     ylab="Standardized Residuals", main="Linear Model 2 Standardized Residual Plot")
#also oddly shaped

plot(rstandard(lmod3)~fitted(lmod3),xlab="Fitted values", 
     ylab="Standardized Residuals", main="Linear Model 3 Standardized Residual Plot")
#huge problem: funnel shape

#all of our linear models seem to have funnel shapes

par(mfrow=c(2,2))
plot(lmod1new)
#for lmod1, going to investigate points 2861, 2864, and 2414


plot(lmod2)
#also 2427

plot(lmod3)

#points 2861, 2864, 2414, and 2427 seem odd so we will investigate them

#cooks distance
cook=cooks.distance(lmod1new)
(idx1=order(cook,decreasing=T)[1:3])
#all very very small

lmodtest1 = update(lmod1new, subset=-idx1)
summary(lmod1new)
summary(lmodtest1)
#barely changes anything

cook2=cooks.distance(lmod2)
(idx2=order(cook2,decreasing=T)[1:3])
lmodtest2 = update(lmod2, subset=-idx2)
summary(lmod2)
summary(lmodtest2)
#barely changes anything

cook3=cooks.distance(lmod3)
(idx3=order(cook3,decreasing=T)[1:3])
lmodtest3 = update(lmod3, subset=-idx3)
summary(lmod3)
summary(lmodtest3)
#r-squared went from 8.1% to 8.4%, still not big change at all

#no big changes, leaving linear models as they are
par(parSave)

#10
#creating variables for leverages for each lm, and find high leverages
(n = nrow(data))
(p1 = length(coef(lmod1new)))
(p2 = length(coef(lmod2)))
(p3 = length(coef(lmod3)))

hatlmod1 = hatvalues(lmod1new)
length(hatlmod1[hatlmod1> 2*p1/n])

hatlmod2 = hatvalues(lmod2)
length(hatlmod2[hatlmod2> 2*p2/n])

hatlmod3 = hatvalues(lmod3)
length(hatlmod3[hatlmod3> 2*p3/n])

#checking for high leverage observations through half normal plots
#first lm
halfnorm(hatlmod1,labs=rownames(data), main="Linear Model 1 Half Normal Plot")
qqlineHalf(hatlmod1)
#a ton of high leverage points

#second lm
halfnorm(hatlmod2,labs=rownames(data), main="Linear Model 2 Half Normal Plot")
qqlineHalf(hatlmod2)
#also a ton of high leverage points

#third lm
halfnorm(hatlmod3,labs=rownames(data), main="Linear Model 3 Half Normal Plot")
qqlineHalf(hatlmod3)

#finding outliers for each lm
rstu = rstudent(lmod1new)
(out1 = length(rstu[rstu> 3]))

rstu2 = rstudent(lmod2)
(out2 = length(rstu2[rstu2> 3]))

rstu3 = rstudent(lmod3)
(out3 = length(rstu3[rstu3> 3]))

#interesting, the more variables are added to the lm, the less outliers it has

#plotting number of outliers per model

barplot(c(out1, out2, out3), col=c("red", "blue", "green"), ylim = c(60,100),
        names= c("Linear Model 1", "Linear Model 2", "Linear Model 3"), main=
          "Number of Outliers for Each Linear Model", ylab = "Number of Outliers",
        xpd = FALSE)

#12
#checking models for colinearity using variance inflation factors
vif(lmod1new)
vif(lmod2)
vif(lmod3)

barplot(vif(lmod1new), ylim = c(0,10), xlab = "Linear Model 1 Coefficients", 
        col = c("pink", "purple", "aquamarine"), ylab = "VIF Values", 
        main = "Variance Inflation Factors for Linear Model 1")
abline(h= c(5, 10), lty= c(3,2), col=c("orange", "red"), lwd=2)

barplot(vif(lmod2), ylim = c(0,10), xlab = "Linear Model 2 Coefficients", 
        col = c("pink", "purple"), ylab = "VIF Values", 
        main = "Variance Inflation Factors for Linear Model 2")
abline(h= c(5, 10), lty= c(3,2), col=c("orange", "red"), lwd=2)












