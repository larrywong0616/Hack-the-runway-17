
#CHINA

fit<- glm(CHINA ~ Min_wage+price+eaFOB+LandingCosts, family = binomial(link = "logit"),data=table.rcwm)
summary(fit) # display results
confint(fit) # 95% CI for the coefficients
exp(coef(fit)) # exponentiated coefficients
exp(confint(fit)) # 95% CI for exponentiated coefficients
predict(fit, type="response") # predicted values
residuals(fit, type="deviance") # residuals

###Lasso, Ridge, Elastic regression
x<-table.rcwm[,c(1:5,7:33,36:88,91:96)]


names(table.rcwm)
y<-table.rcwm[,99]
dim(table.rcwm)[1]
train_rows <- sample(1:(dim(table.rcwm)[1]), .8*dim(table.rcwm)[1])
x.train <- as.matrix(x[train_rows, ])
x.test <- as.matrix(select(x[-train_rows, ]))
y.train <- as.matrix(y[train_rows])
y.test <- as.matrix(y[-train_rows])

fit2<-glmnet(as.matrix(mtcars[-1]), mtcars[,1], 
             lambda=cv.glmnet(as.matrix(mtcars[-1]), mtcars[,1])$lambda.1se)
coef(fit2)
as.data.frame(predict(x.train,y.train))  

fit.lasso <- glmnet(x.train, as.vector(y.train), family="binomial", alpha=1)
fit.ridge <- glmnet(x.train, y.train, family="gaussian", alpha=0)
fit.elnet <- glmnet(x.train, y.train, family="gaussian", alpha=.5)




#Caret
x<-table.rcwm[,c(1:5,7:33,36:88,91:96)]
prop.table(table(x$BANGLADESH))
outcomeName <-"BANGLADESH"
predictorsNames <- names(x)[names(x) != outcomeName]
names(getModelInfo())

x$BANGLDAESH <- ifelse(x$BANGLDAESH==1,'yes','nope')

getModelInfo()$gbm$type
set.seed(1234)
train_rows <- sample(1:(dim(table.rcwm)[1]), .8*dim(table.rcwm)[1])
x.train <- as.data.frame(x[train_rows, ])
x.test <- as.data.frame(x[-train_rows, ])
y.train <- as.data.frame(y[train_rows])
y.test <- as.data.frame(y[-train_rows])
objControl <- trainControl(method='cv', number=3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)

# run model
objModel <- train(x.train[,predictorsNames], as.factor(y.train[,'B']), 
                  method='gbm', 
                  trControl=objControl,  
                  metric = "ROC",
                  preProc = c("center", "scale"))


# find out variable importance
summary(objModel)

# find out model details
objModel
