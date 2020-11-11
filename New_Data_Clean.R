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
new_df <- data.frame(df$NA_Sales, df$EU_Sales,df$JP_Sales,df$Other_Sales,df$Global_Sales, head=FALSE)
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

#Creating a sbsetted dataset, taking out NA values and 
df2<-subset(df, Year!='N/A')
View(df2)
plot(factor(df2$Year))
# Subset the Data to only 2008
df3<-subset(df2, Year==2008)
plot(factor(df3$Year))
View(df3)

#Finding unique Values of all to find any N/A's, looks like Year Has the only NA values
unique(df3$Platform)
unique(df3$Year)
unique(df3$Genre)
unique(df3$Name)
unique(df3$Publisher)



# Create a dummy variable for action games. To be used is regression & Classification 
df3$Action <- ifelse(df3$Genre == "Action", 1, 0)  


# Create New Columns, subtracting Specific columns from global sales

df3$Less_NA_Sales_<- df3$Global_Sales-df3$NA_Sales
df3$Less_EU_Sales_<- df3$Global_Sales-df3$EU_Sales
df3$Less_JP_Sales_<- df3$Global_Sales-df3$JP_Sales
df3$Less_Other_Sales_<- df3$Global_Sales-df3$Other_Sales


# Create a dummy variable for action games. To be used is regression & Classification 
df3$Action <- ifelse(df3$Genre == "Action", 1, 0)  
df3$Action <- ifelse(df3$Genre == "Action", 1, 0)



View(df3)
str(df3)

############################
###LINEAR MODEL BUILDING ###
############################

##BUILDING A LINEAR MODEL TO QUANTIFY THE RELATIONSHIP BETWEEN SALES AND ADSPEND##



M1<-lm(NA_Sales~JP_Sales, df3)  
M2<-lm(Less_EU_Sales_~EU_Sales, df3)
M3<-lm(Less_JP_Sales_~JP_Sales, df3)
M4<-lm(Global_Sales~Less_Other_Sales_, df3)
       
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

plot(df3$Less_NA_Sales_~df3$NA_Sales)
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
##########################################################################
####                      Partitioning data                          #####
##########################################################################







#####################
####RANDOM FOREST####
#####################

library(caret) #data partioning library and other machine learning tools
library(rpart) #CART library
library(e1071) #svm library
library(randomForest) #

#Inspect the Data
str(df3)

#Change Characters to factors
df3$Platform<-as.factor(df3$Platform)
df3$Genre<-as.factor(df3$Genre)
df3$Publisher<-as.factor(df3$Publisher)
df3$Action<-as.factor(df3$Action)
df3$Name<-as.factor(df3$Name)
df3$Year<-as.integer(df3$Year)
df3$Rank<-as.factor(df3$Rank)

#####Get Rid of unnecessary Var
df4<-subset(df3, select = -c(Year,Rank))
df5<-subset(df4, select = -c(Name))
df6<-subset(df5, select = -c(Publisher))
df7<-subset(df6, select = -c(Platform))
df8<-subset(df7, select = -c(Action))



#final check before model
str(df8)
#Set up Training and Testing Data

p<-.7
obs_count<-dim(df8)[1]
training_size <- floor(p * obs_count)
training_size
set.seed(123)
train_ind <- sample(obs_count, size = training_size)

Training <- df8[train_ind, ] 
View(Training)
Testing <- df8[-train_ind, ]
View(Testing)



#caret package implementation with 10-fold cross validation
train_control <- trainControl(method="cv", number=10, savePredictions = TRUE)
RF1 <- train(NA_Sales ~ ., method="rf", trControl=train_control, preProcess=c("center", "scale"), tuneLength=2, data=Training)
print(RF1)
confusionMatrix(predict(RF1, Testing), Testing$NA_Sales, positive='1')

#random forest package implementation
RF2 <- randomForest(NA_Sales ~., Training)
print(RF2)
confusionMatrix(predict(RF2, Testing), Testing$NA_Sales, positive='1')



