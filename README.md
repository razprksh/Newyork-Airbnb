# Newyork-Airbnb
 #
##Installing packages
#
install.packages("biglm")
library("biglm")
install.packages("ff")
library("ff")
install.packages("ffbase")
library("ffbase")
library(datasets)
install.packages("Metrics")
library("Metrics")
install.packages('dplyr')
library(dplyr)
install.packages("magrittr")
library(magrittr)
library(ggplot2)
install.packages("graphics")
library(graphics)
#
##Read in the file Newyork.csv
#
Newyork_Airbnb <- read.csv (file.choose())
dim (Newyork_Airbnb)
str(Newyork_Airbnb)
##To print max in console
options(max.print=999999)

## Creating a dataframe for Newyork_Airbnb
New.data <- data.frame(Newyork_Airbnb)

## Changing last_review unto date format
as.Date(New.data$last_review, "%Y-%m-%d")


#
##
### Cleaning the data
##
#
## Step 1
## Checking missing data
New.data[!complete.cases(New.data),]
##F inding missing values
is.na(New.data3)
## count missing values
sum(is.na(New.data))

## Step2
## To Keep only the rows without missing values 
New.data1 <- New.data[complete.cases(New.data),]
head(New.data1)

# Step3
## Dropping the zeros
New.data2<- as.data.frame (New.data1[apply(New.data1!=0, 1, all),])


# Step4
# Changing categorial data into numericals using lapply 
New.data3<- as.data.frame(lapply(New.data2, as.numeric))
head(New.data3)

## step5
New.data4 <-data.frame(na.omit(New.data3))
names(New.data4)


#
## Spliting into training and testing data
#
##Lets use 70% for training, 20% for validation and 10% for testing
total.rows <-nrow(New.data4)
total.rows
train.size <- floor(.7*total.rows)
validation.size<-floor(.2*total.rows)

train.rows <-sample (1:total.rows,train.size)

Newyork.training <- New.data4 [train.rows, ]

Newyork.remaining <-New.data4[-train.rows, ]

remaining.rows <-nrow(New.data4)
validate.rows <-sample (1:remaining.rows, validation.size)

Newyork.validation <- Newyork.remaining[validate.rows, ]
Newyork.testing<-Newyork.remaining[-validate.rows, ]

set.seed(1000)

##
Fit linear regression model
Newyork.model <-lm( price ~ neighbourhood_group+neighbourhood+room_type+minimum_nights+number_of_reviews+last_review +reviews_per_month+calculated_host_listings_count+availability_365, data=Newyork.training)
summary(Newyork.model)


## Fit linear regression model on the testing data
Newyork.model.testing <-lm( price ~ neighbourhood_group+neighbourhood+room_type+minimum_nights+number_of_reviews+last_review +reviews_per_month+calculated_host_listings_count+availability_365, data=Newyork.testing)
summary(Newyork.model.testing)


