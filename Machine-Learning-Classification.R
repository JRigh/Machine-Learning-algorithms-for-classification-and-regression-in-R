#---------------------------------
# Machine Learning algorithms in R
# Classification algorithms
#---------------------------------

library(GGally)

data(iris)

# 0. Visualization
ggpairs(iris, ggplot2::aes(colour = Species, alpha = 0.4))

# 1. splitting the dataset into training and test sets
set.seed(2023)
ind <- sample(2, nrow(iris),replace=TRUE,prob=c(0.7,0.3))
training <- iris[ind==1,]
testing <- iris[ind==2,]

# 2. save entire dataset, training and testing datasets
# save a copy of the dataset in .csv
write.csv(iris, 
          "path/iris.csv",
          row.names = FALSE)
write.csv(training, 
          "path/iris_training.csv",
          row.names = FALSE)
write.csv(testing, 
          "path/iris_testing.csv",
          row.names = FALSE)

#-----------------------------------------------------------
# 1.1 Naive Bayes classifier (multiple class classification)
#-----------------------------------------------------------

library(klaR) 

# 1. Build a Naive Bayes Classifier
set.seed(2023)
nb_model <- NaiveBayes(Species ~ ., data=training) # train Naïve Bayes model
pred_nb <- predict(nb_model, testing) # apply Naïve Bayes model on test set

# 2. Create a coonfusion Matrix
tab <- table(pred_nb$class, testing$Species)
result_nb <- caret::confusionMatrix(tab)
result_nb

sensitivity_nb <- round(result_rf$byClass[, 1], 4)
specificity_nb <- round(result_rf$byClass[, 2], 4)

# 3. Plot density of each feature using nb_model
opar = par(mfrow=c(2, 2), mar=c(4,0,0,0))
plot(nb_model, main="")  
par(opar)

# 4. Plot the Confusion Matrix

library(ggplot2)

test$pred_nb <- pred_nb$class
ggplot(testing, aes(Species, pred_nb, color = Species)) +
  geom_jitter(width = 0.2, height = 0.1, size=2) +
  annotate('text', label = paste("Specificity (Setosa) =", specificity_nb[1], ", Sensitivity (Setosa) =", sensitivity_nb[1]
                                 ), x = 2, y = 0.75, size = 3) + 
  annotate('text', label = paste("Specificity (Versicolor) =", specificity_nb[2], ", Sensitivity (Versicolor) =", sensitivity_nb[2]
                                 ), x = 2, y = 0.65, size = 3) +
  annotate('text', label = paste("Specificity (Virginica) =", specificity_nb[3], ", Sensitivity (Virginica) =", sensitivity_nb[3]
                                 ), x = 2, y = 0.55, size = 3) +
  labs(title = 'Confusion Matrix - Naive Bayes Classifier',
       subtitle = 'Predicted vs. Observed from Iris dataset',
       y="Predicted", x="Observed") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"))

#--------------------------------------------------
# 1.2 Random Forest (multiple class classification)
#--------------------------------------------------

library(randomForest)

# 1. Build a Random Forest learning tree Classifier
set.seed(2023)
rf_model <- randomForest(Species~., data=training, ntree=100,proximity=TRUE)
table(predict(rf_model),training$Species)
pred_rf <- predict(rf_model, testing)

# 2. Create a Confusion Matrix
tab <- table(pred_rf, testing$Species)
result_rf <- caret::confusionMatrix(tab)
result_rf

result_rf <- caret::confusionMatrix(pred_rf, testing$Species)
sensitivity_rf <- round(result_rf$byClass[, 1], 4)
specificity_rf <- round(result_rf$byClass[, 2], 4)

# 3. Plot the Confusion Matrix

testing$pred_rf <- pred_rf
ggplot(testing, aes(Species, pred_rf, color = Species)) +
  geom_jitter(width = 0.2, height = 0.1, size=2) +
  annotate('text', label = paste("Specificity (Setosa) =", specificity_rf[1], ", Sensitivity (Setosa) =", sensitivity_rf[1]
  ), x = 2, y = 0.75, size = 3) + 
  annotate('text', label = paste("Specificity (Versicolor) =", specificity_rf[2], ", Sensitivity (Versicolor) =", sensitivity_rf[2]
  ), x = 2, y = 0.65, size = 3) +
  annotate('text', label = paste("Specificity (Virginica) =", specificity_rf[3], ", Sensitivity (Virginica) =", sensitivity_rf[3]
  ), x = 2, y = 0.55, size = 3) +
  labs(title = 'Confusion Matrix - Random Forest',
       subtitle = 'Predicted vs. Observed from Iris dataset',
       y="Predicted", x="Observed") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"))


#--------------------------------------------------------------------
# 1.3 Multinomial Logistic regression (multiple class classification)
#--------------------------------------------------------------------

library(stats4) 
library(splines) 
library(VGAM) 

# 1. Build a Multinomial Logistic Regression Classifier
set.seed(2023)
mlr_model <- vglm(Species ~ ., family=multinomial, training)
summary(mlr_model)
pred_mlr<- predict(mlr_model, testing, type = "response")
predictions <- apply(pred_mlr, 1, which.max)

# 2. Create a Confusion Matrix
tab <- table(predictions, test$Species)
predictions[which(predictions=="1")] <- levels(iris$Species)[1]
predictions[which(predictions=="2")] <- levels(iris$Species)[2]
predictions[which(predictions=="3")] <- levels(iris$Species)[3]

result_mlr <- caret::confusionMatrix(tab)
result_mlr

# 3. Plot the Confusion Matrix
testing$pred_mlr <- predictions
ggplot(testing, aes(Species, pred_mlr, color = Species)) +
  geom_jitter(width = 0.2, height = 0.1, size=2) +
  labs(title = 'Confusion Matrix - Multinomial logistic regression',
       subtitle = 'Predicted vs. Observed from Iris dataset',
       y="Predicted", x="Observed") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"))

#------------------------------------------------------------
# 1.4 Support vector machines (multiple class classification)
#------------------------------------------------------------

library(e1071)

# 1. Build a Support Vector Machines Classifier
set.seed(2023)
svm_model <- svm(Species ~ ., data=training,
               kernel="radial") #linear/polynomial/sigmoid
table(predict(svm_model),training$Species)
pred_svm <- predict(svm_model, testing)

# 2. Create a Confusion Matrix
tab <- table(pred_svm, testing$Species)
result_svm <- caret::confusionMatrix(tab)
result_svm

result_svm <- caret::confusionMatrix(pred_svm, test$Species)
sensitivity_svm <- round(result_svm$byClass[, 1], 4)
specificity_svm <- round(result_svm$byClass[, 2], 4)

# 3. Plot the Confusion Matrix
test$pred_svm <- pred_svm
ggplot(testing, aes(Species, pred_svm, color = Species)) +
  geom_jitter(width = 0.2, height = 0.1, size=2) +
  annotate('text', label = paste("Specificity (Setosa) =", specificity_svm[1], ", Sensitivity (Setosa) =", sensitivity_svm[1]
  ), x = 2, y = 0.75, size = 3) + 
  annotate('text', label = paste("Specificity (Versicolor) =", specificity_svm[2], ", Sensitivity (Versicolor) =", sensitivity_svm[2]
  ), x = 2, y = 0.65, size = 3) +
  annotate('text', label = paste("Specificity (Virginica) =", specificity_svm[3], ", Sensitivity (Virginica) =", sensitivity_svm[3]
  ), x = 2, y = 0.55, size = 3) +
  labs(title = 'Confusion Matrix - Support Vector Machines',
       subtitle = 'Predicted vs. Observed from Iris dataset',
       y="Predicted", x="Observed") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"))

#---------------------------------------------------
# 1.5 Neural Network (multiple class classification)
#---------------------------------------------------

library(tidyverse)
library(neuralnet)

# 1. Build a Neural Network Classifier
set.seed(2023)
iris$setosa <- iris$Species=="setosa"
iris$virginica <- iris$Species == "virginica"
iris$versicolor <- iris$Species == "versicolor"

nn_model <- neuralnet(setosa+versicolor+virginica ~ 
                        Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                      data=training, hidden=c(10,10), rep = 5, err.fct = "ce", 
                      linear.output = F, lifesign = "minimal", stepmax = 1000000,
                      threshold = 0.001)

# 2. Create a Confusion Matrix
pred_nn <- predict(nn_mod, testing, type="class")
predictions <- apply(pred_nn, 1, which.max)
tab <- table(predictions, testing$Species)
predictions[which(predictions=="1")] <- levels(iris$Species)[1]
predictions[which(predictions=="2")] <- levels(iris$Species)[2]
predictions[which(predictions=="3")] <- levels(iris$Species)[3]

# 3. Plot the Confusion Matrix
testing$pred_nn <- predictions
ggplot(testing, aes(Species, pred_nn, color = Species)) +
  geom_jitter(width = 0.2, height = 0.1, size=2) +
  labs(title = 'Confusion Matrix - Neural Network',
       subtitle = 'Predicted vs. Observed from Iris dataset',
       y="Predicted", x="Observed") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"))


#----
# end
#----
