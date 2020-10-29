#################################
##ANOTHER DATA CLEANING EXAMPLE##
######INTRODUCING FOR LOOPS######
#################################

df_raw<-read.table('https://raw.githubusercontent.com/slevkoff/CLASS_DATA/master/CLEANING.txt')
dim(df_raw)
View(df_raw)

#CREATES AN EMPTY 1x4 VECTOR OF ZEROS FOR COUNTING
#NAs IN EACH COLUMN
NA_COUNT_LONG<-matrix(0,1,dim(df_raw)[2])
NA_COUNT_LONG #SEE THE CONTENTS OF THIS VARIABLE

#WE COULD DO IT THE LONG WAY
NA_COUNT_LONG[1]<-sum(is.na(df_raw[,1]))
NA_COUNT_LONG[2]<-sum(is.na(df_raw[,2]))
NA_COUNT_LONG[3]<-sum(is.na(df_raw[,3]))
NA_COUNT_LONG[4]<-sum(is.na(df_raw[,4]))

NA_COUNT_LONG #RETURN COUNT OF NAs IN EACH COLUMN

#############################################
#OR WE COULD AUTOMATE IT USING A (FOR) LOOP:#
#############################################
#Sets up empty matrix
NA_COUNT_AUTO<-matrix(0,1,dim(df_raw)[2])
NA_COUNT_AUTO #SEE THE CONTENTS OF THIS VARIABLE

for (i in 1:dim(df_raw)[2]) {
  NA_COUNT_AUTO[i]<-sum(is.na(df_raw[,i]))
}

NA_COUNT_AUTO #RETURN COUNT OF NAs IN EACH COLUMN

#DELETION METHOD IN ONE STEP
df_clean<-na.omit(df_raw)

#CHECK FOR RELATIONSHIPS
cor(df_clean)

#LOOK AT DISTRIBUTIONS THE LONG WAY
hist(df_clean$V1)
hist(df_clean$V2)
hist(df_clean$V3)
hist(df_clean$V4)

#############################################
#OR WE COULD AUTOMATE IT USING A (FOR) LOOP:#
#############################################

for (i in 1:dim(df_clean)[2]) {
  hist(df[,i]) #DISPLAYS HISTOGRAM FOR COLUMN i
}

for (i in 1:dim(df_clean)) {
  
}
