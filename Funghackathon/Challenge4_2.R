


x<-table.rcwm[,c(1:5,7:33,36:88,91:96,99)]
x<-na.omit(x)
x<-as.data.frame(x)

xDummy <- dummyVars("~.",data=x, fullRank=F)
x <- as.data.frame(predict(xDummy,x))
print(names(x))

prop.table(table(x$BANGLADESH))
tempOutcome <- x$BANGLADESH

outcomeName <-"BANGLADESH"
predictorsNames <- names(x)[names(x) != outcomeName]
names(getModelInfo())
x$BANGLADESH <- ifelse(x$BANGLADESH==1,'yes','nope')
getModelInfo()$gbm$type

# split data into training and testing chunks
set.seed(1234)
splitIndex <- createDataPartition(x[,outcomeName], p = .75, list = FALSE, times = 1)
trainDF <- x[ splitIndex,]
testDF  <- x[-splitIndex,]
objControl <- trainControl(method='cv', number=3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)

# run model
objModel <- train(trainDF[,predictorsNames], as.factor(trainDF[,outcomeName]), 
                  method='gbm', 
                  trControl=objControl,  
                  metric = "ROC",
                  preProc = c("center", "scale"))


# find out variable importance
summary(objModel)

# find out model details
objModel

#####GLM
# pick model gbm and find out what type of model it is
getModelInfo()$glmnet$type

# save the outcome for the glmnet model
x$BANGLADESH  <- tempOutcome


# create caret trainControl object to control the number of cross-validations performed
objControl <- trainControl(method='cv', number=3, returnResamp='none')

# run model
objModel <- train(trainDF[,predictorsNames], trainDF[,outcomeName], method='glmnet', family = "binomial", alpha = 1)


# get predictions on your testing data
predictions <- predict(object=objModel, testDF[,predictorsNames])

library(pROC)
auc <- roc(testDF[,outcomeName], predictions)
print(auc$auc)

postResample(pred=predictions, obs=testDF[,outcomeName])

# find out variable importance
summary(objModel)
plot(varImp(objModel,scale=F))

# find out model details
objModel

# display variable importance on a +/- scale 
vimp <- varImp(objModel, scale=F)
results <- data.frame(row.names(vimp$importance),vimp$importance$Overall)
results$VariableName <- rownames(vimp)
colnames(results) <- c('VariableName','Weight')
results <- results[order(results$Weight),]
results <- results[(results$Weight != 0),]

par(mar=c(5,15,4,2)) # increase y-axis margin. 
xx <- barplot(results$Weight, width = 0.85, 
              main = paste("Variable Importance -",outcomeName), horiz = T, 
              xlab = "< (-) importance >  < neutral >  < importance (+) >", axes = FALSE, 
              col = ifelse((results$Weight > 0), 'blue', 'red')) 
axis(2, at=xx, labels=results$VariableName, tick=FALSE, las=2, line=-0.3, cex.axis=0.6)  

