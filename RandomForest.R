#install.packages("randomForest")
library(randomForest)
#install.packages("caret")
library(caret)

#tune random forest to find the best parameters (better approach)
rf_model<-train(Party ~YOB+ Income+ EducationLevel+ HouseholdStatus+ Q109244+ Q115611+ Q98197 +Q113181 +Q98869 +Q101163 + Q99480 +Q105840 +Q120379 + Q116881+ +Q106272+ Q120472 + Q115899 
                + Q102089 +Q110740 + Q119851 +Q121699 +Q115195 + Q106042 +Q118232 + Q100680 + Q118892 +Q107869, data=train ,method="rf",trControl=trainControl(method="cv",number=5),
                prox=TRUE,allowParallel=TRUE)

print(rf_model)
print(rf_model$finalModel)

#alternatively use tune.rf 
tune.rf <- tuneRF(train[,-7],train[,7], improve=0.05, stepFactor=0.5)

#Execute random forest algorith using the most important features (in terms of p-values))
forest = randomForest(Party ~YOB+ Income+ EducationLevel+ HouseholdStatus+ Q109244+ Q115611+ Q98197 +Q113181 +Q98869 +Q101163 + Q99480 +Q105840 +Q120379 + Q116881+ +Q106272+ Q120472 + Q115899 + Q102089 +Q110740 + Q119851 +Q121699 +Q115195 + Q106042 +Q118232 + Q100680 + Q118892 +Q107869 , ntree=800, nodesize=30, data=train)


#A  metric we can look at is related to "impurity", which measures how homogenous each bucket or leaf of the tree is. 
#In each tree in the forest, whenever we select a variable and perform a split, the impurity is decreased. 
#Therefore, one way to measure the importance of a variable is to average the reduction in impurity, 
#taken over all the times that variable is selected for splitting in all of the trees in the forest

varImpPlot(forest) #or forest$importance[order(-forest$importance), , drop = FALSE] or importance(forest)
#forest$importance conatains the same of varImpPlot(forest) but unordered


#Another metric that we can look at is the number of times, aggregated over all of the trees in the random forest model, 
#that a certain variable is selected for a split
vu = varUsed(forest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(forest$forest$xlevels[vusorted$ix]))


PredTestLabels= predict(forest, newdata=test)

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions= PredTestLabels)
write.csv(MySubmission, "SubmissionSimpleLog.csv", row.names=FALSE)