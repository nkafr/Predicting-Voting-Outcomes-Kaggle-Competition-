#install.packages(gbm)
library(gbm)

train$Party=ifelse(train$Party=="Democrat",0, 1)
GBM = gbm(Party ~ YOB+ Income+ EducationLevel+ HouseholdStatus+ Q109244+ Q115611+ Q98197 +Q113181 +Q98869 +Q101163 + Q99480 +Q105840 +Q120379 + Q116881+ +Q106272+ Q120472 + Q115899 + Q102089 +Q110740 + Q119851 +Q121699 +Q115195 + Q106042 +Q118232 + Q100680 + Q118892 +Q107869, data=train,distribution="bernoulli",n.trees=5000,shrinkage=0.0056,verbose=FALSE,cv.folds=16,interaction.depth=1)

gbmresponse=predict(GBM,newdata=test, type="response")
threshold = 0.5

PredTestLabels = as.factor(ifelse(gbmresponse<threshold, "Democrat", "Republican"))
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions= PredTestLabels)
write.csv(MySubmission, "SubmissionSimpleLog.csv", row.names=FALSE)
