## R Language for Prediction of San Francisco Crime Classfication 
## Importing all the libraries which support the various features

library(plyr)  ## Tools for Splitting, Applying and Combining Data
library(dplyr) ## A Grammar of Data Manipulation
library(ggmap)  ## Spatial Visualization with ggplot2
library(ggplot2) ## Spatial Visualization with ggplot2
library(readr) ## Read Rectangular Text Data
library (e1071) ## Misc Functions of the Department of Statistics, Probability
library(sqldf) ## Perform SQL Selects on R Data Frames
library(caret) ## Classification and Regression Training
library(klaR) ## Classification and visualization

print(" Loading the Data sets ")
## Load the data from desired path
naive_train <- read.csv(file="E:/R/San_fran_classification/train.csv",head=TRUE,sep=",")
naive_test <- read.csv(file="E:/R/San_fran_classification/test.csv",head=TRUE,sep=",")

print(" Datasets are loaded and creating required temporary datasets for preprocessing")
naive_train_temp <- naive_train
naive_train_temp$Descript <- NULL # Make it all value NULL to remove the column

print(" Adding new columns to the data")
# Add the required temporary new features to data frame naive_train_temp
date = strptime(naive_train_temp$Dates,"%Y-%m-%d %H:%M:%S")  # Extract the date from the data
naive_train_temp["Year"] <- paste("A", format(date,"%Y")) # Add new coulumn year by separating
naive_train_temp["Month"] <- paste("M", format(date,"%m"))
naive_train_temp["Date"] <-  paste("D",format(date,"%d"))
naive_train_temp["Hour"] <-  format(date,"%H")
naive_train_temp["HourC"] <-  paste("D",format(date,"%H"))
naive_train_temp["DayCat"] <-  paste("DD",substr(format(date,"%d"),1,1))
print(" Classifying hour")
## Here we classify the time into five different categories which are PRE-JOB, MORNING, AFTERNOON, EVENING, NIGHT
## By using the following function we classify the hour
classify_hour <- function(x) { 
  
  if(x >= 0 & x < 5) y <- "NIGHT"
  if(x >= 5 & x < 9) y <- "PRE-JOB"
  if(x >= 9 & x < 13) y <- "MORNING"
  if(x >= 13 & x < 18) y <- "AFTERNOON"
  if(x >= 18 & x <= 24) y <- "EVENING"
  
  return(y)
}
print("Preprocessing...classifying hour function...")

## This line will add a column to the dataset saying category of the hour as described above

naive_train_temp["HourCat"] <- sapply(naive_train_temp$H,classify_hour)

print("Hour is classified and column is added")

##Let's choose the crime which occured most of the times

data_temp_Max <- sqldf('select Category,PdDistrict,HourCat,M,DD, count(*) nro from naive_train_temp group by Category,PdDistrict,HourCat,Y,M,D order by 2')
final_data_train <- ddply(data_temp_Max,~HourCat+PdDistrict+M+DD,function(x){x[which.max(x$nro),]})
# nro will count how many times the crime and is no need to be in dataset and remove it
final_data_train$nro <- NULL


### Test Data preparation
naive_test_temp <- naive_test
naive_test_temp <- round_any(naive_test_temp$X, 0.005)
naive_test_temp$Y_round <- round_any(naive_test_temp$Y, 0.005)
naive_test_temp$X_Y <- as.factor(paste(naive_test_temp$X_round, naive_test_temp$Y_round, sep = " "))

date = strptime(naive_test_temp$Dates,"%Y-%m-%d %H:%M:%S")
naive_test_temp["Y"] <- paste("A", format(date,"%Y"))
naive_test_temp["M"] <- paste("M", format(date,"%m"))
naive_test_temp["D"] <-  paste("D",format(date,"%d"))
naive_test_temp["H"] <-  format(date,"%H")

naive_test_temp["DD"] <-  paste("DD",substr(format(date,"%d"),1,1))

print(" Test data preparation")
naive_test_temp["HourCat"] <- sapply(naive_test_temp$H,classify_hour)

print(" Test data hours classified")
final_data_test <- sqldf('select distinct PdDistrict,HourCat,M,DD from naive_test_temp ')

print("Dataset for testing the model prepared")

## Testing the Model

split=0.80
Index_train <- createDataPartition(final_data_train$Category, p=split, list=FALSE) ## splitting the data
temp_train <- final_data_train[ Index_train,]
temp_test <- final_data_train[-Index_train,]

print(" Data is splitted and initiated the model")
model <-  naiveBayes(Category ~ ., data = temp_train) ## Model training initiated and done

# Make predictions 
x_temp_test <- temp_test[,2:5]
y_temp_test <- temp_test[,1]
predictions <- predict(model, x_temp_test)

print(" Predicitons are made")

## Measure the performance by using confusion matrix.

confusion_matrix<-confusionMatrix(predictions, y_temp_test)

print(" Confusion matrix created ")

## Visualisation using ggplot used online

#library(ggplot2)
#library(ggmap)
#map <- 
 # readRDS("../input/sf_map_copyright_openstreetmap_contributors.rds")
#
#ggmap(map, extent='device', legend="topleft") +
 # geom_point(aes(x=X, y=Y, colour=Category), data=mapdata ) +  
#  ggtitle('Violent Crime in San Francisco')

#ggmap(map, extent='device') +
  #geom_point(aes(x=X, y=Y, colour=Category), data=mapdata ) +
 # scale_colour_discrete(guide='none') +
  #facet_wrap(~Category) +
  #ggtitle('Violent Crime in San Francisco')

#ggplot(data=mapdata, aes(x=DayOfWeek)) +
 # geom_bar(colour="black", fill="skyblue") +
 #ylab('Count')

#ggplot(data=mapdata, aes(x=DayOfWeek)) +
#  geom_bar(colour="black", fill="skyblue") +
 # ylab('Count') +
#  facet_wrap(~Category, scales='free')
#ggplot(data=mapdata, aes(x=Hour)) +
#  geom_bar(colour="black", fill="skyblue") +
#  ylab('Count') 
#ggplot(data=mapdata, aes(x=Hour)) +
#  geom_bar(colour="black", fill="skyblue") +
 # ylab('Count') +
 # facet_wrap(~Category, scales='free')
