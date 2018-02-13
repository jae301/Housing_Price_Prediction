rm(list=ls())
######





##install.packages("plyr")
library(corrplot)
library(plyr)
library(psych)
##EDA
library(ISLR)
#graphics.off()
#par("mar")
#par(mar=c(1,1,1,1))

setwd("~/Desktop/Presentation_502")

train<-read.csv("Train_data.csv", stringsAsFactors = T)
test<-read.csv("test_house.csv", stringsAsFactors = T)


#practice_train<-read.csv("Train_data.csv", stringsAsFactors = T)
#practice_train$MiscVal
summary(train)
sum(is.na(train))#how many total missing values are there
colSums(is.na(train))#every column and their NAs in training data


#makes catergoircal variables
factor_formula <- sapply(train, is.factor)
training_factor_vars<- train[, factor_formula]
colSums(is.na(training_factor_vars))##categorical variables missing variables


#replaces levels with NA with new label
########ALLEY
train$Alley<-factor(ifelse(is.na(train$Alley), 'NoAlley', paste(train$Alley)), levels = c(levels(train$Alley), 'NoAlley'))
colSums(is.na(train))##categorical variables missing variables
levels(train$Alley)

test$Alley<-factor(ifelse(is.na(test$Alley), 'NoAlley', paste(test$Alley)), levels = c(levels(test$Alley), 'NoAlley'))
colSums(is.na(test))##categorical variables missing variables
levels(test$Alley)


####BsmtQual
levels(train$BsmtQual)#"Ex" "Fa" "Gd" "TA"
train$BsmtQual<-factor(ifelse(is.na(train$BsmtQual), 'NoBsmt', paste(train$BsmtQual)), levels = c(levels(train$BsmtQual), 'NoBsmt'))
colSums(is.na(train))##categorical variables missing variables
levels(train$BsmtQual)


levels(test$BsmtQual)#"Ex" "Fa" "Gd" "TA"
test$BsmtQual<-factor(ifelse(is.na(test$BsmtQual), 'NoBsmt', paste(test$BsmtQual)), levels = c(levels(test$BsmtQual), 'NoBsmt'))
colSums(is.na(test))##categorical variables missing variables
levels(test$BsmtQual)

####BsmtCond
levels(train$BsmtCond)# "Fa" "Gd" "Po" "TA"
train$BsmtCond<-factor(ifelse(is.na(train$BsmtCond), 'NoBsmt', paste(train$BsmtCond)), levels = c(levels(train$BsmtCond), 'NoBsmt'))
colSums(is.na(train))##categorical variables missing variables
levels(train$BsmtCond)

levels(test$BsmtCond)# "Fa" "Gd" "Po" "TA"
test$BsmtCond<-factor(ifelse(is.na(test$BsmtCond), 'NoBsmt', paste(test$BsmtCond)), levels = c(levels(test$BsmtCond), 'NoBsmt'))
colSums(is.na(test))##categorical variables missing variables
levels(test$BsmtCond)


####BsmtExposure
levels(train$BsmtExposure)# "Av" "Gd" "Mn" "No"
train$BsmtExposure<-factor(ifelse(is.na(train$BsmtExposure), 'NoBsmt', paste(train$BsmtExposure)), levels = c(levels(train$BsmtExposure), 'NoBsmt'))
colSums(is.na(train))##categorical variables missing variables
levels(train$BsmtExposure)


levels(test$BsmtExposure)# "Av" "Gd" "Mn" "No"
test$BsmtExposure<-factor(ifelse(is.na(test$BsmtExposure), 'NoBsmt', paste(test$BsmtExposure)), levels = c(levels(test$BsmtExposure), 'NoBsmt'))
colSums(is.na(test))##categorical variables missing variables
levels(test$BsmtExposure)


###BsmtFinType1
levels(train$BsmtFinType1)# "ALQ" "BLQ" "GLQ" "LwQ" "Rec" "Unf"
train$BsmtFinType1<-factor(ifelse(is.na(train$BsmtFinType1), 'NoBsmt', paste(train$BsmtFinType1)), levels = c(levels(train$BsmtFinType1), 'NoBsmt'))
colSums(is.na(train))##categorical variables missing variables
levels(train$BsmtFinType1)


levels(test$BsmtFinType1)# "ALQ" "BLQ" "GLQ" "LwQ" "Rec" "Unf"
test$BsmtFinType1<-factor(ifelse(is.na(test$BsmtFinType1), 'NoBsmt', paste(test$BsmtFinType1)), levels = c(levels(test$BsmtFinType1), 'NoBsmt'))
colSums(is.na(test))##categorical variables missing variables
levels(test$BsmtFinType1)



###BsmtFinType2
levels(train$BsmtFinType2)#  "ALQ" "BLQ" "GLQ" "LwQ" "Rec" "Unf"
train$BsmtFinType2<-factor(ifelse(is.na(train$BsmtFinType2), 'NoBsmt', paste(train$BsmtFinType2)), levels = c(levels(train$BsmtFinType2), 'NoBsmt'))
colSums(is.na(train))##categorical variables missing variables
levels(train$BsmtFinType2)


levels(test$BsmtFinType2)#  "ALQ" "BLQ" "GLQ" "LwQ" "Rec" "Unf"
test$BsmtFinType2<-factor(ifelse(is.na(test$BsmtFinType2), 'NoBsmt', paste(test$BsmtFinType2)), levels = c(levels(test$BsmtFinType2), 'NoBsmt'))
colSums(is.na(test))##categorical variables missing variables
levels(test$BsmtFinType2)


##FireplaceQu
levels(train$FireplaceQu)#"Ex" "Fa" "Gd" "Po" "TA"
train$FireplaceQu<-factor(ifelse(is.na(train$FireplaceQu), 'NoFire', paste(train$FireplaceQu)), levels = c(levels(train$FireplaceQu), 'NoFire'))
colSums(is.na(train))##categorical variables missing variables
levels(train$FireplaceQu)


levels(test$FireplaceQu)#"Ex" "Fa" "Gd" "Po" "TA"
test$FireplaceQu<-factor(ifelse(is.na(test$FireplaceQu), 'NoFire', paste(test$FireplaceQu)), levels = c(levels(test$FireplaceQu), 'NoFire'))
colSums(is.na(test))##categorical variables missing variables
levels(test$FireplaceQu)




####GarageType
levels(train$GarageType)#""2Types"  "Attchd"  "Basment" "BuiltIn" "CarPort" "Detchd" 
train$GarageType<-factor(ifelse(is.na(train$GarageType), 'NoGarage', paste(train$GarageType)), levels = c(levels(train$GarageType), 'NoGarage'))
colSums(is.na(train))##categorical variables missing variables
levels(train$GarageType)


levels(test$GarageType)#""2Types"  "Attchd"  "Basment" "BuiltIn" "CarPort" "Detchd" 
test$GarageType<-factor(ifelse(is.na(test$GarageType), 'NoGarage', paste(test$GarageType)), levels = c(levels(test$GarageType), 'NoGarage'))
colSums(is.na(test))##categorical variables missing variables
levels(test$GarageType)



####GarageFinish
levels(train$GarageFinish)#"Fin"      "RFn"      "Unf"
train$GarageFinish<-factor(ifelse(is.na(train$GarageFinish), 'NoGarage', paste(train$GarageFinish)), levels = c(levels(train$GarageFinish), 'NoGarage'))
colSums(is.na(train))##categorical variables missing variables
levels(train$GarageFinish)


levels(test$GarageFinish)#"Fin"      "RFn"      "Unf"
test$GarageFinish<-factor(ifelse(is.na(test$GarageFinish), 'NoGarage', paste(test$GarageFinish)), levels = c(levels(test$GarageFinish), 'NoGarage'))
colSums(is.na(test))##categorical variables missing variables
levels(test$GarageFinish)


####GarageQual
#######LEVELS ARE DIFFERENT NEED TO CHANGE so they have same levels 
levels(train$GarageQual)#"Ex"       "Fa"       "Gd"       "Po"       "TA"       
train$GarageQual<-factor(ifelse(is.na(train$GarageQual), 'NoGarage', paste(train$GarageQual)), levels = c(levels(train$GarageQual), 'NoGarage'))
colSums(is.na(train))##categorical variables missing variables
levels(train$GarageQual)




levels(test$GarageQual)#"Ex"       "Fa"       "Gd"       "Po"       "TA"       
test$GarageQual<-factor(ifelse(is.na(test$GarageQual), 'NoGarage', paste(test$GarageQual)), levels = c(levels(test$GarageQual), 'NoGarage'))
colSums(is.na(test))##categorical variables missing variables
levels(test$GarageQual)


####GarageCond
levels(train$GarageCond)#""Ex"       "Fa"       "Gd"       "Po"       "TA"      
train$GarageCond<-factor(ifelse(is.na(train$GarageCond), 'NoGarage', paste(train$GarageCond)), levels = c(levels(train$GarageCond), 'NoGarage'))
colSums(is.na(train))##categorical variables missing variables
levels(train$GarageCond)


levels(test$GarageCond)#""Ex"       "Fa"       "Gd"       "Po"       "TA"      
test$GarageCond<-factor(ifelse(is.na(test$GarageCond), 'NoGarage', paste(test$GarageCond)), levels = c(levels(test$GarageCond), 'NoGarage'))
colSums(is.na(test))##categorical variables missing variables
levels(test$GarageCond)



####PoolQC
#######LEVELS ARE DIFFERENT NEED TO CHANGE so they have same levels 

levels(train$PoolQC)#""Ex" "Fa" "Gd"
train$PoolQC<-factor(ifelse(is.na(train$PoolQC), 'NoPool', paste(train$PoolQC)), levels = c(levels(train$PoolQC), 'NoPool'))
colSums(is.na(train))##categorical variables missing variables
levels(train$PoolQC)




levels(test$PoolQC)#""Ex" "Fa" "Gd"
test$PoolQC<-factor(ifelse(is.na(test$PoolQC), 'NoPool', paste(test$PoolQC)), levels = c(levels(test$PoolQC), 'NoPool'))
colSums(is.na(test))##categorical variables missing variables
levels(test$PoolQC)


####Fence
levels(train$Fence)#""Ex" "Fa" "Gd"
train$Fence<-factor(ifelse(is.na(train$Fence), 'NoFence', paste(train$Fence)), levels = c(levels(train$Fence), 'NoFence'))
colSums(is.na(train))##categorical variables missing variables
levels(train$Fence)



levels(test$Fence)#""Ex" "Fa" "Gd"
test$Fence<-factor(ifelse(is.na(test$Fence), 'NoFence', paste(test$Fence)), levels = c(levels(test$Fence), 'NoFence'))
colSums(is.na(test))##categorical variables missing variables
levels(test$Fence)


####MiscFeature
#######LEVELS ARE DIFFERENT NEED TO CHANGE so they have same levels 

levels(train$MiscFeature)#""Ex" "Fa" "Gd"
train$MiscFeature<-factor(ifelse(is.na(train$MiscFeature), 'None', paste(train$MiscFeature)), levels = c(levels(train$MiscFeature), 'None'))
colSums(is.na(train))##categorical variables missing variables
levels(train$MiscFeature)

levels(test$MiscFeature)#""Ex" "Fa" "Gd"
test$MiscFeature<-factor(ifelse(is.na(test$MiscFeature), 'None', paste(test$MiscFeature)), levels = c(levels(test$MiscFeature), 'None'))
colSums(is.na(test))##categorical variables missing variables
levels(test$MiscFeature)


#levels(train$MasVnrType) Did have missing values, delete NA

#####take out rest of missing variables
colSums(is.na(train))
train<-na.omit(train)
test<-na.omit(test)
colSums(is.na(train))




##makes continuous variables
nums <- sapply(train, is.numeric)
training_continuous<-data.frame(train[,nums])######[,#] selects given column. When do [,nums] it selects all numerical columns
colSums(is.na(training_continuous))##continuous variables which are missing values
##LotFrontage 259
#MasVnrArea 8
#GarageYrBlt 81
#training_continuous_cleaned <- na.omit(training_continuous)
corrplot(cor(training_continuous), method="number", number.cex = .6)
















hist(log(train$SalePrice))


#hist(train$MSSubClass)
multi.hist(training_continuous[,1:16])
multi.hist(training_continuous[,17:32])
multi.hist(training_continuous[,33:38])


###CONTINUOUS VARIABELS WHICH SHOULD BE CATEGORICAL
hist(train$LowQualFinSF)#whether or not we should categorize this as 0 high qual 1 low qual
train$BsmtFullBath<-as.factor(train$BsmtFullBath)
levels(train$BsmtFullBath)

train$OverallCond<-as.factor(train$OverallCond)
levels(train$OverallCond)
train$OverallQual<-as.factor(train$OverallQual)
levels(train$OverallQual)
train$BsmtHalfBath<-as.factor(train$BsmtHalfBath)
levels(train$BsmtHalfBath)
train$FullBath<-as.factor(train$FullBath)
levels(train$FullBath)######NEEDS TO BE CHANGED BC DIFFERENT LEVELS
train$BedroomAbvGr<-as.factor(train$BedroomAbvGr)
levels(train$BedroomAbvGr)

train$KitchenAbvGr<-as.factor(train$KitchenAbvGr)
levels(train$KitchenAbvGr)####NEEDS TO BE CHANGED BC DIFFERENT LEVELS

train$TotRmsAbvGrd<-as.factor(train$TotRmsAbvGrd)
levels(train$TotRmsAbvGrd)

train$Fireplaces<-as.factor(train$Fireplaces)
levels(train$Fireplaces)#####HAS DIFFERENT LEVELS THANB TRAIN

train$GarageCars<-as.factor(train$GarageCars)
levels(train$GarageCars)

train$MoSold<-as.factor(train$MoSold)
levels(train$MoSold)

train$HalfBath<-as.factor(train$HalfBath)
levels(train$HalfBath)





test$BsmtFullBath<-as.factor(test$BsmtFullBath)
levels(test$BsmtFullBath)
test$OverallCond<-as.factor(test$OverallCond)
levels(test$OverallCond)#####HAS DIFFERENT LEVELS THANB TRAIN
test$OverallQual<-as.factor(test$OverallQual)
levels(test$OverallQual)
test$BsmtHalfBath<-as.factor(test$BsmtHalfBath)
levels(test$BsmtHalfBath)
test$FullBath<-as.factor(test$FullBath)
levels(test$FullBath)
test$BedroomAbvGr<-as.factor(test$BedroomAbvGr)
levels(test$BedroomAbvGr)
test$KitchenAbvGr<-as.factor(test$KitchenAbvGr)
levels(test$KitchenAbvGr)
test$TotRmsAbvGrd<-as.factor(test$TotRmsAbvGrd)
levels(test$TotRmsAbvGrd)#####HAS DIFFERENT LEVELS THANB TRAIN
test$Fireplaces<-as.factor(test$Fireplaces)
levels(test$Fireplaces)
test$GarageCars<-as.factor(test$GarageCars)
levels(test$GarageCars)#####HAS DIFFERENT LEVELS THANB TRAIN
test$MoSold<-as.factor(test$MoSold)
levels(test$MoSold)
test$HalfBath<-as.factor(test$HalfBath)
levels(test$HalfBath)







###### FACTORS WITH DIFFERENT LEVELS ON TRAIN AND TEST.....SO HAVE TO MAKE SURE THE LEVELS ARE SAME THROUGHOUT


plot(train$MiscFeature)#based on this, could group 3 categories and compare to none, that way would work with the test dat
levels(test$MiscFeature)

levels(train$MiscFeature)<-c("HasMiscFeature", "HasMiscFeature","HasMiscFeature","HasMiscFeature" ,"None")
levels(test$MiscFeature)<-c("HasMiscFeature", "HasMiscFeature","HasMiscFeature" ,"None")





plot(train$PoolQC)#based on this, could group 3 categories and compare to none, that way would work with the test dat
test$PoolQC

levels(train$PoolQC)<-c("HasPool", "HasPool","HasPool","NoPool")
levels(test$PoolQC)<-c("HasPool", "HasPool","NoPool")




plot(train$GarageQual)#based on this, could group 3 categories and compare to none, that way would work with the test dat
plot(test$GarageQual)
levels(train$GarageQual)<-c("ExcellentFairGood", "ExcellentFairGood","ExcellentFairGood", "Poor", "Typical", "NoGarage")
levels(test$GarageQual)<-c("ExcellentFairGood","ExcellentFairGood", "Poor", "Typical", "NoGarage")




plot(train$FullBath)
plot(test$FullBath)
levels(train$FullBath)<-c("0", "1","2", "3or4")
levels(test$FullBath)<-c("0", "1","2", "3or4", "3or4")




plot(train$KitchenAbvGr)
plot(test$KitchenAbvGr)
levels(train$KitchenAbvGr)<-c("1","2or3", "2or3")
levels(test$KitchenAbvGr)<-c("1","2or3")




plot(train$Fireplaces)
plot(test$Fireplaces)
levels(train$Fireplaces)<-c("0","1", "2", "3or4")
levels(test$Fireplaces)<-c("0","1", "2", "3or4", "3or4")







plot(train$OverallCond)
plot(test$OverallCond)
levels(train$OverallCond)<-c("1or2","3", "4", "5", "6", "7", "8", "9")
levels(test$OverallCond)<-c("1or2", "1or2","3", "4", "5", "6", "7", "8", "9")






plot(train$TotRmsAbvGrd)
plot(test$TotRmsAbvGrd)
levels(train$TotRmsAbvGrd)<-c("3", "4", "5", "6", "7", "8", "9", "10", "11", "12+")
levels(test$TotRmsAbvGrd)<-c("3", "4", "5", "6", "7", "8", "9", "10", "11", "12+", "12+")





plot(train$GarageCars)
plot(test$GarageCars)
levels(train$GarageCars)<-c("1","2","3","4or5")
levels(test$GarageCars)<-c("1","2","3","4or5", "4or5")






#find way to make this a 1 if a number and 0 if not
train$MiscVal[train$MiscVal > 0] <- 1
train$PoolArea[train$PoolArea > 0] <- 1
train$ScreenPorch[train$ScreenPorch > 0] <- 1
train$X3SsnPorch[train$X3SsnPorch > 0] <- 1
train$EnclosedPorch[train$EnclosedPorch > 0] <- 1
train$BsmtFinSF2[train$BsmtFinSF2 > 0] <- 1
train$LowQualFinSF[train$LowQualFinSF > 0] <- 1
train$BsmtFinSF1[train$BsmtFinSF1 > 0] <- 1


train$MiscVal<-as.factor(train$MiscVal)
train$PoolArea<-as.factor(train$PoolArea)
train$ScreenPorch<-as.factor(train$ScreenPorch)
train$X3SsnPorch<-as.factor(train$X3SsnPorch)
train$EnclosedPorch<-as.factor(train$EnclosedPorch)
train$BsmtFinSF2<-as.factor(train$BsmtFinSF2)
train$BsmtFinSF1<-as.factor(train$BsmtFinSF1)
train$LowQualFinSF<-as.factor(train$LowQualFinSF)








test$MiscVal[test$MiscVal > 0] <- 1
test$PoolArea[test$PoolArea > 0] <- 1
test$ScreenPorch[test$ScreenPorch > 0] <- 1
test$X3SsnPorch[test$X3SsnPorch > 0] <- 1
test$EnclosedPorch[test$EnclosedPorch > 0] <- 1
test$BsmtFinSF2[test$BsmtFinSF2 > 0] <- 1
test$LowQualFinSF[test$LowQualFinSF > 0] <- 1
test$BsmtFinSF1[test$BsmtFinSF1 > 0] <- 1


test$MiscVal<-as.factor(test$MiscVal)
test$PoolArea<-as.factor(test$PoolArea)
test$ScreenPorch<-as.factor(test$ScreenPorch)
test$X3SsnPorch<-as.factor(test$X3SsnPorch)
test$EnclosedPorch<-as.factor(test$EnclosedPorch)
test$BsmtFinSF2<-as.factor(test$BsmtFinSF2)
test$BsmtFinSF1<-as.factor(test$BsmtFinSF1)
test$LowQualFinSF<-as.factor(test$LowQualFinSF)





levels(train$MiscVal)
levels(train$PoolArea)
levels(train$ScreenPorch)
levels(train$X3SsnPorch)
levels(train$EnclosedPorch)
levels(train$BsmtFinSF2)
levels(train$BsmtFinSF1)
levels(train$LowQualFinSF)



levels(test$MiscVal)
levels(test$PoolArea)
levels(test$ScreenPorch)
levels(test$X3SsnPorch)
levels(test$EnclosedPorch)
levels(test$BsmtFinSF2)
levels(test$BsmtFinSF1)
levels(test$LowQualFinSF)







##this allows us to see what are the continuous variables now
nums2 <- sapply(train, is.numeric)
training_continuous_new<-data.frame(train[,nums2])######[,#] selects given column. When do [,nums] it selects all numerical columns
colSums(is.na(training_continuous_new))##continuous variables which are missing values with the new set of categorical and continuous variables
colSums(is.na(training_continuous))##continuous variables which are missing values with original set of categorical and continuous variables


####New corrplot matrix with the correct continuous variables
new_correlation_plot<-corrplot(cor(training_continuous_new), method="number")

##plots of all continuous variables
multi.hist(training_continuous_new)





###trainsformations?
#histogram()


plot(train$GrLivArea, train$SalePrice)#nice lienar relationship





##continuous variable year built changed to catergory which if greater than 1970 its a 1, if less than or equal to 1970 its 0
train$YearBuilt[train$YearBuilt > 1970] <- 1
train$YearBuilt[train$YearBuilt > 1] <- 0
train$YearBuilt<-as.factor(train$YearBuilt)
train$YearBuilt
#plot(train$SalePrice, train$YearBuilt)

test$YearBuilt[test$YearBuilt > 1970] <- 1
test$YearBuilt[test$YearBuilt > 1] <- 0
test$YearBuilt<-as.factor(test$YearBuilt)
test$YearBuilt










train$YearRemodAdd[train$YearRemodAdd > 1970] <- 1
train$YearRemodAdd[train$YearRemodAdd > 1] <- 0
train$YearRemodAdd<-as.factor(train$YearRemodAdd)
train$YearRemodAdd

test$YearRemodAdd[test$YearRemodAdd > 1970] <- 1
test$YearRemodAdd[test$YearRemodAdd > 1] <- 0
test$YearRemodAdd<-as.factor(test$YearRemodAdd)
test$YearRemodAdd








range(train$YrSold)
train$YrSold[train$YrSold > 2008] <- 1
train$YrSold[train$YrSold > 1] <- 0
train$YrSold<-as.factor(train$YrSold)
train$YrSold


range(test$YrSold)
test$YrSold[test$YrSold > 2008] <- 1
test$YrSold[test$YrSold > 1] <- 0
test$YrSold<-as.factor(test$YrSold)
test$YrSold






##this allows us to see what are the continuous variables now
nums3 <- sapply(train, is.numeric)
training_continuous_new2<-data.frame(train[,nums3])######[,#] selects given column. When do [,nums] it selects all numerical columns
colSums(is.na(training_continuous_new2))##continuous variables which are missing values with the new set of categorical and continuous variables
colSums(is.na(training_continuous_new2))##continuous variables which are missing values with original set of categorical and continuous variables

new_correlation_plot<-corrplot(cor(training_continuous_new2), method="number")
multi.hist(training_continuous_new2)








############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################
##############################################################################TRANSFORMATIONS

hist(log(train$MSSubClass))
hist(log(train$LotFrontage))
hist(log(train$LotArea))
hist(log(train$MasVnrArea))
hist(log(train$BsmtUnfSF))
hist(log(train$TotalBsmtSF))
range(train$LotArea)


#train$MSSubClass<-log(train$MSSubClass)
#train$LotFrontage<-log(train$LotFrontage)
#train$LotArea<-log(train$LotArea)
#train$MasVnrArea<-log(train$MasVnrArea)
#train$BsmtUnfSF<-log(train$BsmtUnfSF)
#train$TotalBsmtSF<-log(train$TotalBsmtSF)

#test$MSSubClass<-log(test$MSSubClass)
#test$LotFrontage<-log(test$LotFrontage)
#test$LotArea<-log(test$LotArea)
#test$MasVnrArea<-log(test$MasVnrArea)


#library("outliers")
#outlier_tf<-outlier(training_continuous_new2$MSSubClass,logical=TRUE)
#find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
#data_new = training_continuous_new2[-find_outlier,]
#plot(data_new)
#multi.hist(data_new)


remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}


train$MSSubClass<- remove_outliers(train$MSSubClass)
train$LotFrontage <- remove_outliers(train$LotFrontage)
train$LotArea <- remove_outliers(train$LotArea)
train$MasVnrArea <- remove_outliers(train$MasVnrArea)
train$BsmtUnfSF <- remove_outliers(train$BsmtUnfSF)
train$TotalBsmtSF <- remove_outliers(train$TotalBsmtSF)
boxplot(train$MSSubClass)
boxplot(train$LotFrontage)
boxplot(train$LotArea)

test$MSSubClass<- remove_outliers(test$MSSubClass)
test$LotFrontage <- remove_outliers(test$LotFrontage)
test$LotArea <- remove_outliers(test$LotArea)
test$MasVnrArea <- remove_outliers(test$MasVnrArea)
test$BsmtUnfSF <- remove_outliers(test$BsmtUnfSF)
test$TotalBsmtSF <- remove_outliers(test$TotalBsmtSF)






plot(train$RoofMatl)
plot(test$RoofMatl)
levels(train$RoofMatl)<-c("Other","CompShg","ModOrSev", "Other", "Other", "Other", "Other", "Other", "Other")
levels(test$RoofMatl)<-c("Other","CompShg","ModOrSev", "Other", "Other", "Other", "Other", "Other", "Other")







######basic model steps
colSums(is.na(train))##any missining variables?
train<-na.omit(train)
train$SaleType
House_Data_train<-train
House_Data_test<-test



#######This tells us tehre is factor/categorical variable which is using only 1 of the categories
#dat <- na.omit(House_Data)
## extract factor columns and drop redundant levels
#fctr <- lapply(dat[sapply(dat, is.factor)], droplevels)
## count levels
#sapply(fctr, nlevels)
#levels(train$Utilities)



##########SINCE UTILITIES only uses 1 of the categories, it cant be used to model. It wouldnt provide any information. THUS DELETE
House_Data_train$Utilities <- NULL
House_Data_test$Utilities <- NULL

colSums(is.na(train))##any missining variables?
colSums(is.na(test))##any missining variables?





#train_data_for_fitting<-sample(c(TRUE,FALSE), nrow(House_Data_train),rep=TRUE)
#test<-(!train_data_for_fitting)
#traindata<-House_Data_train[train_data_for_fitting,]
#testdata<-House_Data_train[test,]






#x<-model.matrix(SalePrice~.-Id,House_Data)[,-1]
#y<-House_Data$SalePrice

#traindata<-sample(1:nrow(x), nrow(x)/2)
#test<-(-traindata)
#y.test=y[test]


#fit_stepwise<- lm(y[traindata]~x[traindata,])
#step<-stepAIC(fit_stepwise, direction="both")#works becasue n>p
#step$anova # display results
#coef(step)
#contrasts (House_Data$BsmtFullBath)







#lm_fit<-lm(SalePrice~.-Id,data=House_Data)




##TOO BIG FOR THIS so focused on continuous vars
#For computational reasons, best subset selection cannot be applied with very large p.
#regfit_best_subset_cont<-regsubsets(SalePrice~.-Id,data=training_continuous_new2[train_data_for_fitting_continuous,],nvmax=19)
#test.mat=model.matrix(Salary~.,data=Hitters[test,])


#######Stepwies with the full data

#regfit_fwd<-regsubsets(SalePrice~.-Id,data=House_Data ,nvmax=19, method ="forward")
#summary(regfit_fwd)


###develop stepwise model both method on FULL TRAINING DATA-- THIS IS JUST TO MAKE SURE THE MODELING WORKS> NOT THE 
library(MASS)
#fit_stepwise<- lm(SalePrice~.-Id, data = House_Data_train)
#step<-stepAIC(fit_stepwise, direction="both")#works becasue n>p
#step$anova # display results
#coef(step)

#summary(step)

#Step_prediction<-predict(step, House_Data_test)




##########
########## took the training data set, then split it and used it on here
#fit_stepwise2<- lm(SalePrice~.-Id, data = traindata)
#step2<-stepAIC(fit_stepwise2, direction="both")#works becasue n>p
#step2$anova # display results
#coef(step2)

#summary(step2)

#Step_prediction<-predict(step2, testdata)

#RMSE_regtree<-sqrt(mean((Step_prediction-testdata$SalePrice)^2))
#RMSE_regtree
#MSE_step<-(RMSE_regtree)^2
#MSE_step








library(pls)

x<-model.matrix(SalePrice~., House_Data_train)[,-1]
y<-House_Data_train$SalePrice
set.seed(1)
train<-sample(1:nrow(x), nrow(x)/2)
test<-(-train)
y.test=y[test]

pls.fit=plsr(SalePrice~., data=House_Data_train, subset=train, validation="CV")
summary(pls.fit)
pls.pred=predict(pls.fit,x[test,],ncomp=20)

mean((pls.pred-y.test)^2)
sqrt(mean((pls.pred-y.test)^2))


test_avg<-mean(y.test)
test_r2<-1-mean((y.test - pls.pred)^2) /mean((y.test - test_avg)^2)
test_r2#R^2 test  0.83214


