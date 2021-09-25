# ---
  title: "Data Mining, KNN"
author: Ilse Akkerman - Author, Alwin Schra - Reviewer
date: "`r format(Sys.time(), '%d %B, %Y')`"
output:
  html_notebook:
  toc: yes
toc_depth: 2
# ---
  
install.packages("readr")
install.packages("class")
install.packages("magrittr")
install.packages("caret")
install.packages("e1071")

library(readr)
library(class)
library(magrittr)
library(caret)
library(lattice)
library(e1071)

# ---
## Business Understanding
# For the donation of blood it is necessary to check the person giving the blood on infective diseases. 
#It is in the greatest interest of the person receiving blood that he or she will not be infective with a disease. 
#The person receiving blood is weak and most of the time already fighting a disease or recovering from a operation. 
#Therefore it is necessary for the blood bank to test each person giving blood on infective diseases.  
  

## Data Understanding
#  Choose a suitable dataset from [this](https://github.com/HAN-M3DM-Data-Mining/assignments/tree/master/datasets) folder and train  your own kNN model. Follow all the steps from the CRISP-DM model.
# the data set is the HCV data set
# the data exists of a patient number, a colum if they are blood donor or not, age, sex and different blood values
  
url <- "https://raw.githubusercontent.com/HAN-M3DM-Data-Mining/assignments/master/datasets/KNN-hcvdat0.csv"
rawDF <- read_csv(url)
View(rawDF)
str(rawDF)


## Data Preparation

# The first row of data does not contribute to the end result so er van delete that one
cleanDF <- rawDF[,-1]
head(cleanDF)
View(cleanDF)
# there are also NA values which have to be removed
cleanDF2 <- drop_na(cleanDF)
head(cleanDF2)
View(cleanDF2)
cleanDF3 <- na.omit(cleanDF2)
head(cleanDF3)
View(cleanDF3)

cntDiag <- table(cleanDF3$Category)
propDiag <- round(prop.table(cntDiag)*100, digits = 1)

cntDiag
propDiag

cleanDF3$Category <- cleanDF3$Category %>% factor
cleanDF3$Category <- fct_collapse(cleanDF3$Category, donor = "0=Blood Donor", hepatitis = c("0s=suspect Blood Donor", "1=Hepatitis", "2=Fibrosis", "3=Cirrhosis"))
levels(cleanDF3$Category)
head(cleanDF3, 10)
summary(cleanDF3[c("CREA", "GGT", "PROT")])

normalize <- function(x) { 
  return ((x - min(x)) / (max(x) - min(x))) 
}

testSet1 <- c(1:5)
testSet2 <- c(1:5) * 10

cat("testSet1:", testSet1, "\n")
cat("testSet2:", testSet2, "\n")

cat("Normalized testSet1:", normalize(testSet1), "\n")
cat("Normalized testSet2:", normalize(testSet2))

nCols <- dim(cleanDF3)[2]
NorDF <- cleanDF3[4:13]
View(NorDF)
cleanDF3_no <- sapply(1:10,
                     function(x) {
                       normalize(NorDF[,x])
                     }) %>% as.data.frame()
summary(cleanDF3_no[c("CREA", "GGT", "PROT")])


count(cleanDF3)
trainDF_feat <- cleanDF3[1:294, ]
testDF_feat <- cleanDF3[295:588, ]
trainDF_labels <- cleanDF3[1:294, 1]
testDF_labels <- cleanDF3[295:588, 1]
cl <- trainDF_labels[,1, drop=TRUE]
dim(trainDF_feat)
dim(testDF_feat)
length(cl)
summary(cleanDF3)
sum(is.na(cleanDF3))

## Mis.nan()## Modeling
cleanDF_test_predi <- knn(train = as.matrix(trainDF_feat), test = as.matrix(testDF_feat), cl = as.matrix(trainDF_labels), k = 21)
head(cleanDF_test_predi)
confusionMatrix(cleanDF_test_predi, testDF_labels[[1]], positive = NULL, dnn = c("Prediction", "True"))


## Evaluation and Deployment
# there is an error in the code in line 101, i cant figure out where the mistake is made and internet isnt of much help. the error i am getting is:
# Error in knn(train = as.matrix(trainDF_feat), test = as.matrix(testDF_feat),  : 
#NA/NaN/Inf in foreign function call (arg 6)
#In addition: Warning messages:
#  1: In knn(train = as.matrix(trainDF_feat), test = as.matrix(testDF_feat),  :
#             NAs introduced by coercion
#            2: In knn(train = as.matrix(trainDF_feat), test = as.matrix(testDF_feat),  :
#                        NAs introduced by coercion
# Maybe Alwin can figure out the error and find a way to fix it. 

# Because of the error i can not see the accuracy of the model. For the sake of the receiver of the blood the test should be really accurate so that there will be no transmitting diseases given to them.
# Therefore an accuracy of 99% or higher will be acceptable
# Next to that you rather have a false positive than a false negative. With a false positive there is no risk for the receiving party of getting an invective disease that may kill them without proper treatment.

                      

reviewer adds suggestions for improving the model

