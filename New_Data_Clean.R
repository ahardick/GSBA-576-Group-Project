#######################################
######## GSBA 576 Goup Project ########
##### Sudent Perfomance in Exams ######
########## Goup Members: ##############
##### Abdul, Hardick, Hidalgo & Vo ####
#######################################


#Importing dataset from CSV file uploaded to GSBA-576-Group Project Repository
#naming the import as df
df<-read.csv("https://raw.githubusercontent.com/ahardick/GSBA-576-Group-Project/master/vgsales.csv")


#conducting some preliminary exploratory analysis

#checking the dimensions of the dataframe as if it were a matrix object
dim(df)

#checking the head() function shows the first six observations
head(df)

#generating the summary statistics
summary(df)  

#Checking the class for each variable
# Do we need to change any of the values? 
class(df$Rank)
class(df$Name)
class(df$Platform)
class(df$Year)
class(df$Genre)
class(df$Publisher)
class(df$NA_Sales)
class(df$EU_Sales)
class(df$JP_Sales)
class(df$Other_Sales)
class(df$Global_Sales)

# Create a new DF of each of the numeric values
new_df <- data.frame(df$NA_Sales, df$EU_Sales,df$JP_Sales,df$Other_Sales,df$Global_Sales, head=TRUE)
names(new_df) &lt;- c("Sales", "EU_Sales", "JP_Sales" , "Other")                      
head(new_df)                          
#Creating histogram plots for each of the variables
hist(df$Rank)
hist(df$Name)
hist(df$Platform)
hist(df$Year)
hist(df$Genre)
hist(df$Publisher)
hist(df$NA_Sales)
hist(df$EU_Sales)
hist(df$JP_Sales)
hist(df$Other_Sales)
hist(df$Global_Sales)

#Generating the pairs of scatter plots for all variables
pairs(new_df)

#Generating the entire variance-covariance matrix
cov(new_df)




#############################
#######DATA CLEANING ########
#############################



##CONDUCT SOME EXPLORATORY ANALYSIS##
View(df)  ##View data in spreadsheet format
head(df)  ##display first 6 rows of data
tail(df)  ##display last 6 rows of data
summary(df)  ##generates summary statistics for all variables
plot(df$NA_Sales)   ##plot of NA Sales
hist(df$NA_Sales)  ##generates a histogram of NA Sales
summary(df$NA_Sales) ##summarizes NA_Sales
#Plotting Titles Sold  by platform
plot(factor(df$Genre))
#Plotting number of ttles sold by by year. Looks like there are some NA Values we can delete
plot(factor(df$Year))

#Finding unique Values of all to find any N/A's, looks like Year Has the only NA values
unique(df3$Platform)
unique(df3$Year)
unique(df3$Genre)
unique(df3$Name)
unique(df3$Publisher)


#Deleting rows that have NA values, saving to a new DF
df2<-subset(df, Year!='N/A')
view(df2)
plot(factor(df2$Year))
# Subsetting the data into only 2008
df3<-subset(df2, Year==2008)
plot(factor(df3$Year))

view(df3)


plot(factor(df3$Genre))
# Create a dummy variable for action games. To be used is regression & Classification 
df3$Action <- ifelse(df3$Genre == "Action", 1, 0)  


# Create New Columns, subtracting Specific columns from global sales

df3$Less_NA_Sales_<- df3$Global_Sales-df3$NA_Sales
df3$Less_EU_Sales_<- df3$Global_Sales-df3$EU_Sales
df3$Less_JP_Sales_<- df3$Global_Sales-df3$JP_Sales
df3$Less_Other_Sales_<- df3$Global_Sales-df3$Other_Sales

View(df3)
##################################
####MULTIPLE LINEAR REGRESSION####
##########W/ DIAGNOSTICS##########
##################################



##DESCRIPTIVE SUMMARY STATISTICS##
cov(df[,7:11]) #generates the variance-covariance matrix from column variables 7-11
cor(df[,7:11]) #generates the correlation matrix for column variables 7-11

##GENERATING SOME BASIC VISUALIZATIONS
hist(df3$Global_Sales, prob = TRUE, breaks = 100)#generates a histogram for the Sales variable
hist(df3$EU_Sales, prob = TRUE, breaks = 100) 
hist(df3$JP_Sales, prob = TRUE, breaks = 100) 
hist(df3$NA_Sales, prob = TRUE, breaks = 100) 
hist(df3$Other_Sales, prob = TRUE, breaks = 100)



#add calibrated normal density curve to histogram
curve(dnorm(x, mean = mean(df3$Global_Sales), sd = sd(df3$Global_Sales)), col = "darkblue", lwd = 2, add = TRUE)

#generates a nonparametric density estimate of the distribution of the Sales variable
plot(density(df3$Global_Sales)) 
#generate pairs of scatter plots from the variables in columns 7-11
pairs(df3[,7:11]) 

#generate scatter plot of Sales(X) vs. AdSpend(Y)
plot(df3$Global_Sales~df3$Less_NA_Sales) 

############################
###LINEAR MODEL BUILDING ###
############################

##BUILDING A LINEAR MODEL TO QUANTIFY THE RELATIONSHIP BETWEEN SALES AND ADSPEND##

# How can we build a model to compare Global sales to other States? 


M1<-lm(Global_Sales~Less_NA_Sales, df3)  
M2<-lm(Global_Sales~Less_EU_Sales, df3)
M3<-lm(Global_Sales~Less_JP_Sales, df3)
M4<-lm(Global_Sales~Less_Other_Sales, df3)
       
##MODEL DIAGNOSTICS##
summary(M1) #produces the summary output of the model 1
confint(M1) #returns upper and lower bounds from the 95% confidence interval for each model parameter

summary(M2) #produces the summary output of the model 2 
confint(M2) #returns upper and lower bounds from the 95% confidence interval for each model parameter

summary(M3) #produces the summary output of the model 3
confint(M3) #returns upper and lower bounds from the 95% confidence interval for each model parameter

summary(M4) #produces the summary output of the model 
confint(M4) #returns upper and lower bounds from the 95% confidence interval for each model parameter



##VISUALIZING OUR RESULTS

plot(df3$Global_Sales~df3$Less_NA_Sales)
#add regression line to plot
abline(M1$coefficients[1], M1$coefficients[2], col='blue', lwd=2)


plot(df3$Global_Sales~df3$Less_EU_Sales)
#add regression line to plot
abline(M2$coefficients[1], M2$coefficients[2], col='blue', lwd=2)


plot(df3$Global_Sales~df3$Less_JP_Sales)
#add regression line to plot
abline(M3$coefficients[1], M3$coefficients[2], col='blue', lwd=2)

plot(df3$Global_Sales~df3$Less_Other_Sales)
#add regression line to plot
abline(M4$coefficients[1], M4$coefficients[2], col='blue', lwd=2) #add regression line to plot



##RESIDUAL ANALYSIS##
plot(M1$residuals)
abline(0,0,col='black')
hist(M1$residuals, breaks = 50)
summary(M1$residuals)


plot(M2$residuals)
abline(0,0,col='black')
hist(M2$residuals, breaks = 50)
summary(M2$residuals)


plot(M3$residuals)
abline(0,0,col='black')
hist(M3$residuals, breaks = 50)
summary(M3$residuals)


plot(M4$residuals)
abline(0,0,col='black')
hist(M4$residuals, breaks = 50)
summary(M4$residuals)


##QUESTION: Can we predict the success of global Sales? based on the sales in a given country? ##



## Not sure if we need to keep this below

library(tseries) #loads "tseries" library - need to first install "tseries" package
#conducts a hypothesis test for normality called the Jarque-Bera test
jarque.bera.test(M1$residuals) #null hypothesis: data is distribution is normal

jarque.bera.test(M2$residuals)

jarque.bera.test(M3$residuals)

jarque.bera.test(M4$residuals)
##############################################
####MULTIPLE LINEAR REGRESSION: AN EXAMPLE####
#####WITH PREDICTION, VALIDATION/TESTING######
###########AND REGULARIZATION#################
##############################################


#QUESTION: IF WE CONTROL FOR TARGETED AD SPENDING (ADSPEND), IS THE PROMOTION ACTUALLY EFFECTIVE?

##BUILD A MULTIVARIATE MODEL TO PREDICT SALES CONTROLLING FOR BOTH ADSPEND AND PROMO##
M3<-lm(Sales~AdSpend+Promo, df) #model: Sales = B_0+B_1(AdSpend)+B_2(Promo)+e
summary(M3)  #returns summary output for model M3

M4<-lm(Sales~AdSpend+Promo+Impressions, df) #adds Impressions to the model M3
summary(M4)  #returns summary output for model M4

#QUESTION: ARE THERE SEASONALITY EFFECTS RELATED TO A PARTICULAR MONTH/DAY OF THE WEEK?

M5<-lm(Sales~AdSpend+Month, df) #build a regression using dummy (binary) variables controlling for Month
summary(M5) #summary output from model M5

M6<-lm(Sales~AdSpend+Day, df) #build a regression using dummy (binary) variables controlling for Day
summary(M6) #summary output for model M6

M7<-lm(Sales~ . -Date, df) #regresses sales on ALL other varaibles EXCEPT for the Date variable
summary(M7)

##MANUALLY CREATE DUMMY VARIABLE FOR SATURDAY EFFECT USING SUBSETTING []##
df$Sat[df$Day=='Saturday']<-1 #if it's Saturday, code the Sat variable 1
df$Sat[df$Day!='Saturday']<-0 #if it's not Saturday, code the Sat variable 0
View(df)

##BUILD MODEL CAPTURING SATURDAY EFFECT ONLY IN ADDITION TO ADSPEND##
M8<-lm(Sales~AdSpend+Sat, df) #model: Sales=B_0+B_1(AdSpend)+B_2(Sat)+e
summary(M8) #returns summary output
cbind( M8$coefficients, confint(M8)) #binds the point estimates and a interval in a table

##SOME RESIDUAL ANALYSIS##
plot(M8$residuals) #plots residuals
abline(0,0,col='black', lwd = 4) #adds a straight line across X-axis

hist(M8$residuals, prob = TRUE) #plots histogram of residuals
#adds calibrated normal curve to residual histogram
curve(dnorm(x, mean = mean(M8$residuals), sd = sd(M8$residuals)), col = "darkblue", lwd = 2, add=TRUE)
summary(M8$residuals) #summary descriptive stats for residuals

library(tseries) #loads "tseries" library - need to first install "tseries" package
#conducts a hypothesis test for normality called the Jarque-Bera test
jarque.bera.test(M8$residuals) #null hypothesis: data is distribution is normal

##################################################
##PREDICTION USING A TRAINING/TESTING PARTITIONS##
##################################################

Training<-subset(df, df$Month!='December') #generates training data
Testing<-subset(df, df$Month=='December') #generates testing data

#CHECK DIMENSIONS OF DATA PARTITION
dim(Training)
dim(Testing)
View(Testing)

#RE-BUILD MODEL M8 WITH ONLY THE TRAINING DATA PARTITION
M9<-lm(Sales~AdSpend+Sat, Training)
summary(M9)

##CALCULATE ROOT MEAN SQUARE PREDICTION ERROR ON TEST DATA: THE IN-SAMPLE ERROR MEASURE
RMSE_IN<-sqrt(sum((M9$fitted.values-Training$Sales)^2)/length(Training$Sales))
RMSE_IN #report root mean squared error (E_out) using the out-of-sample testing data

#EVALUATE M9 ON THE TEST PARTITION TO COMPUTE THE OUT-OF-SAMPLE PREDICTIONS
predictions<-predict(M9, Testing)
View(predictions) # view predictions for December

##CALCULATE ROOT MEAN SQUARE PREDICTION ERROR ON TEST DATA: THE OUT-OF-SAMPLE ERROR MEASURE
RMSE_OUT<-sqrt(sum((predictions-Testing$Sales)^2)/length(Testing$Sales))
RMSE_OUT #report root mean squared error (E_out) using the out-of-sample testing data

############################
#####PARTITIONING DATA######
###WITH THE CARET PACKAGE###
############################
install.packages('caret') #installs the caret package
library(caret)  #calls the caret library to use createDataPartition()

##now we will split the data into training and test sets
##creates a vector of rows to randomly sample p=70% from the raw data for traning
set.seed(123) #locks seed for random partitioning
inTrain <- createDataPartition(y=df$Sales, p=.70, list = FALSE) 

##stores these rows in the training set
Training<-df[inTrain,]  

##stores all rows not in the training set in the test/validation set
Testing<-df[-inTrain,]  

##look at the fruits of your labor!
dim(Training)
dim(Testing)
View(Training)
View(Testing)

#############################################
##AN ALTERNATIVE TO PARTITIONING WITH CARET##
#############################################

#re-download fresh dataset if needed
df <-read.csv("https://raw.githubusercontent.com/slevkoff/CLASS_DATA/master/SALES.csv", header=T)

#fraction of sample to be used for training
p<-.7

#number of observations (rows) in the dataframe
obs_count<-dim(df)[1]

#number of observations to be selected for the training partition
#the floor() function rounds down to the nearest integer
training_size <- floor(p * obs_count)

#set the seed to make your partition reproducible
set.seed(123)
#create a vector with the shuffled row numbers of the original dataset
train_ind <- sample(obs_count, size = training_size)

Training <- df[train_ind, ] #pulls random rows for training
Testing <- df[-train_ind, ] #pulls random rows for testing

dim(Training)
dim(Testing)



############################################
##THE UNREGULARIZED PSEUDOINVERSE SOLUTION##
############################################

#LET'S CONSIDER ESTIMATING THE FOLLOWING VERSION of M9 WITH TRAINING DATA:
#SALES=B0+B1*ADSPEND+B2*SAT+u

VecOfOnes<-rep(1,times=dim(Training)[1]) ##generates a vector of ones that has same length as rows in dataset

##adds column of ones to the input data and changes the class from data.frame to matrix
X<-cbind(as.matrix(VecOfOnes), as.matrix(unlist(Training[,7])),as.matrix(unlist(Training[,9])))  ##creates the X matrix and adds a column of 1's for the intercept term

y<-as.matrix(Training[,5]) ##changes the class of the output variable to a matrix class


##NOTES: the solve() function is how R computes an inverse matrix
##the t() function is used to take the transpose of a matrix
##the expression %*% is uses to multiply two matrix objects together

PseudoInverse<-solve(t(X)%*%X)%*%t(X)  
Beta<-PseudoInverse%*%y  ##recovers the optimal parameter vector
rownames(Beta)<-c('Intercept','AdSpend','Sat')  #adds row names to the Beta vector
t(Beta) ##reports the Beta vector (as a row vector)

##constructing the X matrix using the test data
VecOfOnes<-rep(1,times=dim(Testing)[1])
X_test<-cbind(as.matrix(VecOfOnes), as.matrix(unlist(Testing[,7])),as.matrix(unlist(Testing[,9])))

pred_in<-X%*%Beta #generates fitted values (ie:  in-sample predictions)
pred_out<-X_test%*%Beta #generates predictions on testing data (ie: out-of-sample predictions)

RMSE_IN_NR<-sqrt(sum((pred_in-Training$Sales)^2)/length(pred_in))  #computes in-sample error
RMSE_OUT_NR<-sqrt(sum((pred_out-Testing$Sales)^2)/length(pred_out)) #computes out-of-sample error

RMSE_IN_NR
RMSE_OUT_NR


###############################
##IMPLEMENTING REGULARIZATION##
####USING THE RIDGE PENALTY####
###############################

lambda<-seq(0, 1,.001) #generates a grid of lambdas between 0 and 1
Beta_RIDGE<-matrix(NA, 3, length(lambda)) #an empty matrix for storing Betas for each lambda
I<-diag(dim(X)[2]) #builds the (k+1)x(k+1) identity matrix
#generates RIDGE regression estimates for each value of the regularization parameter, lambda
for (i in 1:length(lambda)){
  
  PseudoInverse_RIDGE<-solve(t(X)%*%X-lambda[i]*I)%*%t(X)  #pseudo-inverse solution for the RIDGE regression
  Beta_RIDGE[,i]<-PseudoInverse_RIDGE%*%y  ##recovers the optimal parameter vector
}

#crate empty matrices to store errors for each regularized value in lambda
RMSE_IN<-matrix(NA,1,length(lambda))
RMSE_OUT<-matrix(NA,1,length(lambda))

for (i in 1:length(lambda)){
  pred_in<-X%*%Beta_RIDGE[,i]
  pred_out<-X_test%*%Beta_RIDGE[,i]
  RMSE_IN[i]<-sqrt(sum((pred_in-Training$Sales)^2)/length(pred_in))  #computes in-sample error
  RMSE_OUT[i]<-sqrt(sum((pred_out-Testing$Sales)^2)/length(pred_out)) #computes out-of-sample error
}
rownames(Beta_RIDGE)<-c('Intercept.','AdSpend','Sat')
Beta_RIDGE #report coefficient estimates for each value of the regularization parameter, lambda
#RMSE_IN #repors in-sample error for each value of the regularization parameter, lambda
#RMSE_OUT #reports out-of-sample error for each value of the regularization parameter, lambda
RMSE_OUT<-t(RMSE_OUT) #converts RMSE_OUT to column vector to use cbind in the next step

#CREATE TABLE WITH LAMBDA VALUES IN FIRST COLUMN
#AND ASSOCIATED E_OUT IN THE SECOND COLUMN
ValTable<-cbind(as.matrix(lambda), RMSE_OUT)

#PLOT THE E_OUT VALUES AGAINST THE DEGREE OF REGULARIZATION
plot(ValTable[,1], ValTable[,2], type='l', ylab='E_OUT', xlab='lambda', main='REGULARIZATION RESULTS')

min(RMSE_OUT) #LOWEST ESTIMATED E_OUT WITH REGULARIZATION
max(RMSE_OUT) #HIGHEST E_OUT IN THE ABSENCE OF ANY REGULARIZATION MATCHES RMSE_OUT_NR


