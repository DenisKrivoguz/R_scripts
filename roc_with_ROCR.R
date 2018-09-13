library(ROCR)

#Load dataset
x <- read.csv("D:/GIS/Shapes/Temp/LSI_and_LS_test.csv", sep = ",")
x1 <- data.frame(x$category, x$prediction)
x2 <- round(x1, digits = 0)

#Change names of columns
colnames(x2) <- c("prediction", "category")

#
predict_rocr <- prediction(x2$prediction, x2$category)
perf_rocr <- performance(predict_rocr, "tpr", "fpr")

#Plotting ROC curve
plot(perf_rocr, col = "green", print.auc=TRUE) 
abline(0, 1, col = "grey")
legend("bottomright", legend = "AUC = 0.81")



auc_rocr <- performance(predict_rocr, "auc")

auc <- auc_rocr@y.values
