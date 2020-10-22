library(caret)
library(rattle)
library(rpart)
library(RColorBrewer)
library(rpart.plot)
library(randomForest)
library(corrplot)
library(gbm)                                      

var_valid<- read.csv('pml-testing.csv', header=T)
var_train<- read.csv('pml-training.csv', header=T)

training<- var_train[, colSums(is.na(var_train)) == 0]
validating<- var_valid[, colSums(is.na(var_valid)) == 0]

sub_training1 <- training[, -c(1:5)]
sub_validating1 <- validating[, -c(1:5)]

set.seed(123)
Train <- createDataPartition(sub_training1$classe, p = 0.7, list = FALSE)

sub_training2 <- sub_training1[Train, ]
sub_testing2 <- sub_training1[-Train, ]

nearzero <- nearZeroVar(sub_training2)
sub_testing3  <- sub_testing2[, nearzero]
sub_training3 <- sub_training2[, -nearzero]

dim(sub_testing3)
dim(sub_training3)
cor_mat <- cor(sub_training3[, -53])
corrplot(cor_mat, order = "AOE", method = "square", type = "lower", 
         tl.cex = 0.5, tl.col = rgb(1, 0, 1))



modFit_T <- rpart(classe~.,sub_training2)
# Prediction using Decision tree
predict_T <- predict(modFit_T, sub_testing2, type="class")
confusionMatrix(myTesting$classe, predict_T)




modFRF <- randomForest(as.factor(classe) ~ ., data=sub_training2, method="class")
predRF <- predict(modFRF, sub_testing2, type = "class")

cmrf<-confusionMatrix(predRF, factor(sub_testing2$classe))
plot(cmrf$table, col = cmrf$byClass, main = paste("Random Forest Confusion Matrix: Accuracy =", round(cmrf$overall['Accuracy'], 8)))

decisionTreeMod1 <- rpart(classe ~ ., data=training1, method="class")
fancyRpartPlot(decisionTreeMod1)