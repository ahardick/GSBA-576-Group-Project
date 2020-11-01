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

df3<-subset(df2, Year==2008)
plot(factor(df3$Year))

?barplot
view(df3)
?hist

plot(factor(df3$Genre))
# Create a dummy variable for action games. To be used is regression & Classification 
df3$Action <- ifelse(df3$Genre == "Action", 1, 0)  
view(df3)
