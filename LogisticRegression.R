#Run logistic regression using the most important features (in terms of p-values))
logit = glm(Party ~YOB+ Income+ EducationLevel+ HouseholdStatus+ Q109244+ Q115611+ Q98197 +Q113181 +Q98869 +Q101163 + Q99480 +Q105840 +Q120379 + Q116881+ +Q106272+ Q120472 + Q115899 + Q102089 +Q110740 + Q119851 +Q121699 +Q115195 + Q106042 +Q118232 + Q100680 + Q118892 +Q107869,family="binomial",data=train)

PredTest = predict(logit, newdata=test, type="response")
threshold = 0.5

PredTestLabels = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions= PredTestLabels)
write.csv(MySubmission, "SubmissionSimpleLog.csv", row.names=FALSE)