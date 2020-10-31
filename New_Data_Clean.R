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

?data.frame
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
