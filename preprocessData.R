#install.packages("eeptools")
library(eeptools)
#install.packages("mice")
library(mice)

#impute columns apo 2:6 kai sta questions antikathistw ta emty values me to mean tous opote ta questions einai numerical
train = read.csv("train2016.csv",na.strings=c("",".","NA"))
test = read.csv("test2016.csv",na.strings=c("",".","NA"))


#Remove outliers in YOB attribute
train$YOB[which(train$YOB>2001)]=NA
train$YOB[which(train$YOB<1931)]=NA

test$YOB[which(test$YOB>2001)]=NA
test$YOB[which(test$YOB<1931)]=NA


#replace empty values of questions with the mean in both train and test set
for (i in 8:108) {
  train[, i] = as.numeric(train[, i]) - 1
  train[, i][is.na(train[, i])] = mean(train[, i], na.rm=TRUE)
}


for (i in 7:107) {
  test[, i] = as.numeric(test[, i]) - 1
  test[, i][is.na(test[, i])] = mean(test[, i], na.rm=TRUE)
}

train$YOB = makenum(as.character(train$YOB))
test$YOB = makenum(as.character(test$YOB))

#Tranform income into an ordered factor 
incomeLevels = c("under $25,000", "$25,001 - $50,000", "$50,000 - $74,999", "$75,000 - $100,000", "$100,001 - $150,000", "over $150,000")

train$Income = factor(train$Income,levels=incomeLevels,ordered=TRUE)
test$Income = factor(test$Income,levels=incomeLevels,ordered=TRUE)

#Tranform education into an ordered factor 
educationLevels = c("Current K-12", "High School Diploma", "Current Undergraduate", 
                    "Associate's Degree", "Bachelor's Degree", "Master's Degree", 
                    "Doctoral Degree")

train$EducationLevel =factor(train$EducationLevel,levels=educationLevels,ordered=TRUE)
test$EducationLevel = factor(test$EducationLevel, levels=educationLevels, ordered=TRUE)

dependent=train$Party

#bind both train and test set to impute silmutaeously for columns 2:6
imputationColumns =  c("YOB", "Gender", "Income", "HouseholdStatus", "EducationLevel")
all = rbind(train[-c(7)], test)
simple = all[imputationColumns]

imputed=complete(mice(simple,m=10))

#Recreate again train and test dataset from the imputed dataframe
train$YOB = imputed[1:nrow(train),]$YOB
train$Gender = imputed[1:nrow(train),]$Gender
train$Income = imputed[1:nrow(train),]$Income
train$HouseholdStatus = imputed[1:nrow(train),]$HouseholdStatus
train$EducationLevel = imputed[1:nrow(train),]$EducationLevel


test$YOB = imputed[(nrow(train) + 1):nrow(all),]$YOB
test$Gender = imputed[(nrow(train) + 1):nrow(all),]$Gender
test$Income = imputed[(nrow(train) + 1):nrow(all),]$Income
test$HouseholdStatus = imputed[(nrow(train) + 1):nrow(all),]$HouseholdStatus
test$EducationLevel = imputed[(nrow(train) + 1):nrow(all),]$EducationLevel


write.csv(train, "trainnew.csv", row.names=FALSE)
train = read.csv("trainnew.csv")
write.csv(test, "testnew.csv", row.names=FALSE)
test = read.csv("testnew.csv")
