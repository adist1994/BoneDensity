rm(list=ls())
library(readr)
library(MASS)
library(kernlab)
library(mvtnorm)
library(Matrix)
library(optimx)

# Read data
bone <- read_csv("data/spnbmd.csv")
bone <- as.data.frame(bone)

# Create training/testing data without discarding subjects with single observation

train <- bone

q <- table(bone$idnum)
q <- row.names(q)[as.numeric(table(bone$idnum))>1]
test <- bone[bone$idnum %in% q,] # test data with after discarding



# Traning and testing after discarding---------------------------

q <- table(bone$idnum)
q <- row.names(q)[as.numeric(table(bone$idnum))>1]
train <- bone[bone$idnum %in% q,]
test <-  bone[bone$idnum %in% q,]

bone$spnbmd <- log(bone$spnbmd)
bone$age <- (bone$age - mean(bone$age))/sd(bone$age)
bone$spnbmd <- (bone$spnbmd - mean(bone$spnbmd))/sd(bone$spnbmd)

train <- bone
test <- bone


# Standardize training and testing data

train$age <- (train$age - mean(train$age))/sd(train$age)
train$spnbmd <- (train$spnbmd - mean(train$spnbmd))/sd(train$spnbmd)

test$age <- (test$age - mean(train$age))/sd(train$age)
test$spnbmd <- (test$spnbmd - mean(train$spnbmd))/sd(train$spnbmd)


# Split training data by class labels

train <- split(train, train$sex) # for classifying sex

train <- split(train, train$ethnic) # for classifying race

