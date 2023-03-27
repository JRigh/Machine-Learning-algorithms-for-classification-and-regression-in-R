#---------------------------------
# Machine Learning algorithms in R
# Classification algorithms
#---------------------------------

setwd("C:/Users/julia/OneDrive/Desktop/github/9. Machine_learning_toolbox_R")

library(xtable)
data(iris)

#-----------------
# 0. Visualization
#-----------------

library(GGally)

# 1. multiple plots 
ggpairs(iris, ggplot2::aes(colour = Species, alpha = 0.4)) +
  labs(title = 'Summary of distributions',
       subtitle = 'Complete Iris dataset',
       y="", x="") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))


library(tidyverse)
library(cowplot)

# 2.1 Create initial scatterplot
p <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species))+
  geom_point() + 
  labs(title = 'Scatterplot with marginal densities',
       subtitle = 'Sepal.Length x Sepal.Width from Iris dataset',
       y="Sepal.Width", x="Sepal.Length") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# 2.2 Create marginal densities
xdens <- axis_canvas(p, axis = "x") +
  geom_density(data = iris, aes(x = Sepal.Length, fill = Species),
               alpha = 0.4, size = 0.2)
ydens <- axis_canvas(p, axis = "y", coord_flip = TRUE)+
  geom_density(data = iris, aes(x = Sepal.Width, fill = Species),
               alpha = 0.4, size = 0.2) + coord_flip()

p1 <- insert_xaxis_grob(p, xdens, grid::unit(.2, "null"), position = "top")
p2 <- insert_yaxis_grob(p1, ydens, grid::unit(.2, "null"), position = "right")

# 2.3 Create complete plot
ggdraw(p2)

# Multiple Scatterplots with densities

library(gridExtra)

# first plot -------------------------------------------------------------------
# 2.4 Create initial scatterplot
p <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species))+
  geom_point() + 
  labs(title = 'Scatterplot with marginal densities',
       subtitle = 'Sepal.Length x Sepal.Width from Iris dataset',
       y="Sepal.Width", x="Sepal.Length") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# 2.5 Create marginal densities
xdens <- axis_canvas(p, axis = "x") +
  geom_density(data = iris, aes(x = Sepal.Length, fill = Species),
               alpha = 0.4, size = 0.2)
ydens <- axis_canvas(p, axis = "y", coord_flip = TRUE)+
  geom_density(data = iris, aes(x = Sepal.Width, fill = Species),
               alpha = 0.4, size = 0.2) + coord_flip()

p1 <- insert_xaxis_grob(p, xdens, grid::unit(.2, "null"), position = "top")
p2 <- insert_yaxis_grob(p1, ydens, grid::unit(.2, "null"), position = "right")

# 2.6 Create complete plot
plot1 <- ggdraw(p2)

# second plot ------------------------------------------------------------------
# 2.7 Create initial scatterplot
p <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species))+
  geom_point() + 
  labs(title = 'Scatterplot with marginal densities',
       subtitle = 'Sepal.Length x Petal.Length from Iris dataset',
       y="Sepal.Width", x="Sepal.Length") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# 2.8 Create marginal densities
xdens <- axis_canvas(p, axis = "x") +
  geom_density(data = iris, aes(x = Sepal.Length, fill = Species),
               alpha = 0.4, size = 0.2)
ydens <- axis_canvas(p, axis = "y", coord_flip = TRUE)+
  geom_density(data = iris, aes(x = Petal.Length, fill = Species),
               alpha = 0.4, size = 0.2) + coord_flip()

p1 <- insert_xaxis_grob(p, xdens, grid::unit(.2, "null"), position = "top")
p2 <- insert_yaxis_grob(p1, ydens, grid::unit(.2, "null"), position = "right")

# 2.9 Create complete plot
plot2 <- ggdraw(p2)

# third plot -------------------------------------------------------------------
# 2.10 Create initial scatterplot
p <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Width, color = Species))+
  geom_point() + 
  labs(title = 'Scatterplot with marginal densities',
       subtitle = 'Sepal.Length x Petal.Width from Iris dataset',
       y="Sepal.Width", x="Sepal.Length") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# 2.11 Create marginal densities
xdens <- axis_canvas(p, axis = "x") +
  geom_density(data = iris, aes(x = Sepal.Length, fill = Species),
               alpha = 0.4, size = 0.2)
ydens <- axis_canvas(p, axis = "y", coord_flip = TRUE)+
  geom_density(data = iris, aes(x = Petal.Width, fill = Species),
               alpha = 0.4, size = 0.2) + coord_flip()

p1 <- insert_xaxis_grob(p, xdens, grid::unit(.2, "null"), position = "top")
p2 <- insert_yaxis_grob(p1, ydens, grid::unit(.2, "null"), position = "right")

# 2.12 Create complete plot
plot3 <- ggdraw(p2)

# fourth plot ------------------------------------------------------------------
# 2.13 Create initial scatterplot
p <- ggplot(iris, aes(x = Sepal.Width, y = Petal.Width, color = Species))+
  geom_point() + 
  labs(title = 'Scatterplot with marginal densities',
       subtitle = 'Sepal.Width x Petal.Width from Iris dataset',
       y="Sepal.Width", x="Sepal.Length") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# 2.14 Create marginal densities
xdens <- axis_canvas(p, axis = "x") +
  geom_density(data = iris, aes(x = Sepal.Width, fill = Species),
               alpha = 0.4, size = 0.2)
ydens <- axis_canvas(p, axis = "y", coord_flip = TRUE)+
  geom_density(data = iris, aes(x = Petal.Width, fill = Species),
               alpha = 0.4, size = 0.2) + coord_flip()

p1 <- insert_xaxis_grob(p, xdens, grid::unit(.2, "null"), position = "top")
p2 <- insert_yaxis_grob(p1, ydens, grid::unit(.2, "null"), position = "right")

# 2.15 Create complete plot
plot4 <- ggdraw(p2)

# 2.16 final plot -------------------------------------------------------------------
final.plot <- grid.arrange(plot1, plot2, plot3, plot4, nrow = 2)

#-------------------------------------------------------------------------------

# 1. splitting the dataset into training and test sets
set.seed(2023)
ind <- sample(2, nrow(iris),replace=TRUE,prob=c(0.7,0.3))
training <- iris[ind==1,]
testing <- iris[ind==2,]

# 2. save entire dataset, training and testing datasets
# save a copy of the dataset in .csv
write.csv(iris, 
          "C:/Users/julia/OneDrive/Desktop/github/9. Machine_learning_toolbox_R/iris.csv",
          row.names = FALSE)
write.csv(training, 
          "C:/Users/julia/OneDrive/Desktop/github/9. Machine_learning_toolbox_R/iris_training.csv",
          row.names = FALSE)
write.csv(testing, 
          "C:/Users/julia/OneDrive/Desktop/github/9. Machine_learning_toolbox_R/iris_testing.csv",
          row.names = FALSE)

#-----------------------------------------------------------
# 1.1 Naive Bayes classifier (multiple class classification)
#-----------------------------------------------------------

library(klaR) 

# 1. Build a Naive Bayes Classifier
set.seed(2023)
nb_model <- NaiveBayes(Species ~ ., data=training) # train Naïve Bayes model
pred_nb <- predict(nb_model, testing) # apply Naïve Bayes model on test set
pred_nb_training <- predict(nb_model, training) # apply Naïve Bayes model on train set

# 2. Create accuracy metrics table and coonfusion Matrix
tab_training <- table(pred_nb_training$class, training$Species)
tab_testing <- table(pred_nb$class, testing$Species)
result_nb_training <- caret::confusionMatrix(tab_training)
result_nb_testing <- caret::confusionMatrix(tab_testing)
result_nb_testing

# metrics on the training set
sensitivity_nb_training <- round(result_nb_training$byClass[, 1]*100, 2)
specificity_nb_training <- round(result_nb_training$byClass[, 2]*100, 2)
accuracy_nb_training <- round(result_nb_training$overall[1]*100, 2)
precision_nb_training <- round(mean(result_nb_training$byClass[,'Pos Pred Value'] *100), 2)
recall_nb_training <- round(mean(result_nb_training$byClass[, 'Sensitivity']*100), 2)
f1_score_nb_training <- round(2 * ((precision_nb_training * recall_nb_training) / (precision_nb_training + recall_nb_training)),2)
  
# metrics on the testing set
sensitivity_nb_testing <- round(result_nb_testing$byClass[, 1]*100, 2)
specificity_nb_testing <- round(result_nb_testing$byClass[, 2]*100, 2)
accuracy_nb_testing <- round(result_nb_testing$overall[1]*100, 2)
precision_nb_testing <- round(mean(result_nb_testing$byClass[,'Pos Pred Value'] *100), 2)
recall_nb_testing <-  round(mean(result_nb_testing$byClass[, 'Sensitivity']*100), 2)
f1_score_nb_testing <- round(2 * ((precision_nb_testing * recall_nb_testing) / (precision_nb_testing + recall_nb_testing)),2)


table_metrics <- matrix(c(accuracy_nb_training, accuracy_nb_testing,
                          precision_nb_training, precision_nb_testing,
                          recall_nb_training, recall_nb_testing, 
                          f1_score_nb_training, f1_score_nb_testing), byrow = TRUE, ncol = 2)
colnames(table_metrics) <-  c('Training', 'Testing')
rownames(table_metrics) <- c('accuracy', 'precision', 'recall', 'f1-score')
table_metrics

# export the results in LaTex document
print(xtable(table_metrics, type = "latex", digits=2), file = "tables.tex")
  

# 3.1 plot the confusion matrix
testing$pred_nb <- pred_nb$class
ggplot(testing, aes(Species, pred_nb, color = Species)) +
  geom_jitter(width = 0.2, height = 0.1, size=2) +
  annotate('text', label = paste("Specificity (Setosa) =", specificity_nb[1], ", Sensitivity (Setosa) =", sensitivity_nb[1]
  ), x = 2, y = 0.75, size = 3) + 
  annotate('text', label = paste("Specificity (Versicolor) =", specificity_nb[2], ", Sensitivity (Versicolor) =", sensitivity_nb[2]
  ), x = 2, y = 0.65, size = 3) +
  annotate('text', label = paste("Specificity (Virginica) =", specificity_nb[3], ", Sensitivity (Virginica) =", sensitivity_nb[3]
  ), x = 2, y = 0.55, size = 3) +
  labs(title = 'Confusion Matrix - Naive Bayes Classifier',
       subtitle = paste('Predicted vs. Observed from Iris dataset. Accuracy: ', accuracy_nb),
       y="Predicted", x="Observed") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# alternative
# 3.2 3lot the Confusion Matrix
cm_nb <- caret::confusionMatrix(factor(pred_nb$class), factor(testing$Species), dnn = c("Predicted", "Observed"))
plt <- as.data.frame(cm_nb$table)
plt$Predicted <- factor(plt$Predicted, levels=rev(levels(plt$Predicted)))

ggplot(plt, aes(Predicted, Observed, fill= Freq)) +
  geom_tile() + 
  geom_text(aes(label=Freq), size = 5) +
  scale_fill_gradient(low="white", high="darkgreen") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c(levels(factor(testing$Species)))) +
  scale_y_discrete(labels=rev(c(levels(factor(testing$Species))))) +
  labs(title = 'Confusion Matrix - Naive Bayes Classifier',
       subtitle = paste('Predicted vs. Observed from Iris testing dataset. Accuracy: ', accuracy_nb, '%'),
       y="Predicted", x="Observed") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

#--------------------------------------------------
# 1.2 Random Forest (multiple class classification)
#--------------------------------------------------

library(randomForest)

# 1. Build a Random Forest learning tree Classifier
set.seed(2023)
rf_model <- randomForest(Species~., data=training, ntree=100,proximity=TRUE) # train RF model
pred_rf <- predict(rf_model, testing) # apply RF model on test set
pred_rf_training <- predict(rf_model, training) # apply RF model on train set

# 2. Create accuracy metrics table and coonfusion Matrix
tab_training <- table(pred_rf_training, training$Species)
tab_testing <- table(pred_rf, testing$Species)
result_rf_training <- caret::confusionMatrix(tab_training)
result_rf_testing <- caret::confusionMatrix(tab_testing)
result_rf_testing

# metrics on the training set
sensitivity_rf_training <- round(result_rf_training$byClass[, 1]*100, 2)
specificity_rf_training <- round(result_rf_training$byClass[, 2]*100, 2)
accuracy_rf_training <- round(result_rf_training$overall[1]*100, 2)
precision_rf_training <- round(mean(result_rf_training$byClass[,'Pos Pred Value'] *100), 2)
recall_rf_training <- round(mean(result_rf_training$byClass[, 'Sensitivity']*100), 2)
f1_score_rf_training <- round(2 * ((precision_rf_training * recall_rf_training) / (precision_rf_training + recall_rf_training)),2)

# metrics on the testing set
sensitivity_rf_testing <- round(result_rf_testing$byClass[, 1]*100, 2)
specificity_rf_testing <- round(result_rf_testing$byClass[, 2]*100, 2)
accuracy_rf_testing <- round(result_rf_testing$overall[1]*100, 2)
precision_rf_testing <- round(mean(result_rf_testing$byClass[,'Pos Pred Value'] *100), 2)
recall_rf_testing <-  round(mean(result_rf_testing$byClass[, 'Sensitivity']*100), 2)
f1_score_rf_testing <- round(2 * ((precision_rf_testing * recall_rf_testing) / (precision_rf_testing + recall_rf_testing)),2)


table_metrics <- matrix(c(accuracy_rf_training, accuracy_rf_testing,
                          precision_rf_training, precision_rf_testing,
                          recall_rf_training, recall_rf_testing, 
                          f1_score_rf_training, f1_score_rf_testing), byrow = TRUE, ncol = 2)
colnames(table_metrics) <-  c('Training', 'Testing')
rownames(table_metrics) <- c('accuracy', 'precision', 'recall', 'f1-score')
table_metrics

# export the results in LaTex document
print(xtable(table_metrics, type = "latex", digits=2), file = "tables.tex")

# 3.1 plot the confusion matrix
table(predict(rf_model),training$Species)
pred_rf <- predict(rf_model, testing)
tab <- table(pred_rf, testing$Species)
result_rf <- caret::confusionMatrix(tab)
result_rf
testing$pred_rf <- pred_rf
ggplot(testing, aes(Species, pred_rf, color = Species)) +
  geom_jitter(width = 0.2, height = 0.1, size=2) +
  labs(title = 'Confusion Matrix - Random forest Classifier',
       subtitle = paste('Predicted vs. Observed from Iris testing dataset. Accuracy: ', accuracy_rf_testing),
       y="Predicted", x="Observed") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# alternative
# 3.2 3lot the Confusion Matrix
cm_rf <- caret::confusionMatrix(factor(pred_rf), factor(testing$Species), dnn = c("Predicted", "Observed"))
plt <- as.data.frame(cm_rf$table)
plt$Predicted <- factor(plt$Predicted, levels=rev(levels(plt$Predicted)))

ggplot(plt, aes(Predicted, Observed, fill= Freq)) +
  geom_tile() + 
  geom_text(aes(label=Freq), size = 5) +
  scale_fill_gradient(low="white", high="darkgreen") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c(levels(factor(testing$Species)))) +
  scale_y_discrete(labels=rev(c(levels(factor(testing$Species))))) +
  labs(title = 'Confusion Matrix - Random Forest Classifier',
       subtitle = paste('Predicted vs. Observed from Iris testing dataset. Accuracy: ', accuracy_rf_testing, '%'),
       y="Predicted", x="Observed") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

#--------------------------------------------------------------------
# 1.3 Multinomial Logistic regression (multiple class classification)
#--------------------------------------------------------------------

library(stats4) 
library(splines) 
library(VGAM) 

# 1. Build a Multinomial Logistic Regression Classifier
set.seed(2023)
mlr_model <- vglm(Species ~ ., family=multinomial, training)
pred_mlr_training<- predict(mlr_model, training, type = "response")
pred_mlr_testing<- predict(mlr_model, testing, type = "response")
predictions <- apply(pred_mlr_testing, 1, which.max)
predictions_training <- apply(pred_mlr_training, 1, which.max)

# 2. Create accuracy metrics table and coonfusion Matrix
predictions[which(predictions=="1")] <- levels(iris$Species)[1]
predictions[which(predictions=="2")] <- levels(iris$Species)[2]
predictions[which(predictions=="3")] <- levels(iris$Species)[3]

predictions_training[which(predictions_training=="1")] <- levels(iris$Species)[1]
predictions_training[which(predictions_training=="2")] <- levels(iris$Species)[2]
predictions_training[which(predictions_training=="3")] <- levels(iris$Species)[3]


tab_training <- table(predictions_training, training$Species)
tab_testing <- table(predictions, testing$Species)
result_mlr_training <- caret::confusionMatrix(tab_training)
result_mlr_testing <- caret::confusionMatrix(tab_testing)
result_mlr_testing

# metrics on the training set
sensitivity_mlr_training <- round(result_mlr_training$byClass[, 1]*100, 2)
specificity_mlr_training <- round(result_mlr_training$byClass[, 2]*100, 2)
accuracy_mlr_training <- round(result_mlr_training$overall[1]*100, 2)
precision_mlr_training <- round(mean(result_mlr_training$byClass[,'Pos Pred Value'] *100), 2)
recall_mlr_training <- round(mean(result_mlr_training$byClass[, 'Sensitivity']*100), 2)
f1_score_mlr_training <- round(2 * ((precision_mlr_training * recall_mlr_training) / (precision_mlr_training + recall_mlr_training)),2)

# metrics on the testing set
sensitivity_mlr_testing <- round(result_mlr_testing$byClass[, 1]*100, 2)
specificity_mlr_testing <- round(result_mlr_testing$byClass[, 2]*100, 2)
accuracy_mlr_testing <- round(result_mlr_testing$overall[1]*100, 2)
precision_mlr_testing <- round(mean(result_mlr_testing$byClass[,'Pos Pred Value'] *100), 2)
recall_mlr_testing <-  round(mean(result_mlr_testing$byClass[, 'Sensitivity']*100), 2)
f1_score_mlr_testing <- round(2 * ((precision_mlr_testing * recall_mlr_testing) / (precision_mlr_testing + recall_mlr_testing)),2)


table_metrics <- matrix(c(accuracy_mlr_training, accuracy_mlr_testing,
                          precision_mlr_training, precision_mlr_testing,
                          recall_mlr_training, recall_mlr_testing, 
                          f1_score_mlr_training, f1_score_mlr_testing), byrow = TRUE, ncol = 2)
colnames(table_metrics) <-  c('Training', 'Testing')
rownames(table_metrics) <- c('accuracy', 'precision', 'recall', 'f1-score')
table_metrics

# export the results in LaTex document
print(xtable(table_metrics, type = "latex", digits=2), file = "tables.tex")

# 3.1 plot the Confusion Matrix
testing$pred_mlr <- predictions
ggplot(testing, aes(Species, pred_mlr, color = Species)) +
  geom_jitter(width = 0.2, height = 0.1, size=2) +
  labs(title = 'Confusion Matrix - Multinomial Logistic Regression',
       subtitle = paste('Predicted vs. Observed from Iris dataset. Accuracy: '),
       y="Predicted", x="Observed") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# alternative
# 3.2 3lot the Confusion Matrix
testing$pred_mlr <- predictions
cm_lrm <- caret::confusionMatrix(factor(testing$pred_mlr), factor(testing$Species), dnn = c("Predicted", "Observed"))
plt <- as.data.frame(cm_lrm$table)
plt$Predicted <- factor(plt$Predicted, levels=rev(levels(plt$Predicted)))

ggplot(plt, aes(Predicted, Observed, fill= Freq)) +
  geom_tile() + 
  geom_text(aes(label=Freq), size = 5) +
  scale_fill_gradient(low="white", high="darkgreen") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c(levels(factor(testing$Species)))) +
  scale_y_discrete(labels=rev(c(levels(factor(testing$Species))))) +
  labs(title = 'Confusion Matrix - Multinomial Logistic Regression Classifier',
       subtitle = paste('Predicted vs. Observed from Iris testing dataset. Accuracy: ', accuracy_mlr_testing, '%'),
       y="Predicted", x="Observed") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))


#------------------------------------------------------------
# 1.4 Support vector machines (multiple class classification)
#------------------------------------------------------------

library(e1071)

# 1. Build a Support Vector Machines Classifier
set.seed(2023)
svm_model <- svm(Species ~ ., data=training,
                 kernel="radial")                 #linear/polynomial/sigmoid
pred_svm <- predict(svm_model, testing)           # apply svm model on test set
pred_svm_training <- predict(svm_model, training) # apply svm model on train set

# 2. Create accuracy metrics table and coonfusion Matrix
tab_training <- table(pred_svm_training, training$Species)
tab_testing <- table(pred_svm, testing$Species)
result_svm_training <- caret::confusionMatrix(tab_training)
result_svm_testing <- caret::confusionMatrix(tab_testing)
result_svm_testing

# metrics on the training set
sensitivity_svm_training <- round(result_svm_training$byClass[, 1]*100, 2)
specificity_svm_training <- round(result_svm_training$byClass[, 2]*100, 2)
accuracy_svm_training <- round(result_svm_training$overall[1]*100, 2)
precision_svm_training <- round(mean(result_svm_training$byClass[,'Pos Pred Value'] *100), 2)
recall_svm_training <- round(mean(result_svm_training$byClass[, 'Sensitivity']*100), 2)
f1_score_svm_training <- round(2 * ((precision_svm_training * recall_svm_training) / (precision_svm_training + recall_svm_training)),2)

# metrics on the testing set
sensitivity_svm_testing <- round(result_svm_testing$byClass[, 1]*100, 2)
specificity_svm_testing <- round(result_svm_testing$byClass[, 2]*100, 2)
accuracy_svm_testing <- round(result_svm_testing$overall[1]*100, 2)
precision_svm_testing <- round(mean(result_svm_testing$byClass[,'Pos Pred Value'] *100), 2)
recall_svm_testing <-  round(mean(result_svm_testing$byClass[, 'Sensitivity']*100), 2)
f1_score_svm_testing <- round(2 * ((precision_svm_testing * recall_svm_testing) / (precision_svm_testing + recall_svm_testing)),2)


table_metrics <- matrix(c(accuracy_svm_training, accuracy_svm_testing,
                          precision_svm_training, precision_svm_testing,
                          recall_svm_training, recall_svm_testing, 
                          f1_score_svm_training, f1_score_svm_testing), byrow = TRUE, ncol = 2)
colnames(table_metrics) <-  c('Training', 'Testing')
rownames(table_metrics) <- c('accuracy', 'precision', 'recall', 'f1-score')
table_metrics

# export the results in LaTex document
print(xtable(table_metrics, type = "latex", digits=2), file = "tables.tex")

# 3.1 plot the confusion matrix
table(predict(svm_model),training$Species)
pred_svm <- predict(svm_model, testing)
tab <- table(pred_svm, testing$Species)
result_svm <- caret::confusionMatrix(tab)
result_svm
testing$pred_svm <- pred_svm
ggplot(testing, aes(Species, pred_svm, color = Species)) +
  geom_jitter(width = 0.2, height = 0.1, size=2) +
  labs(title = 'Confusion Matrix - Support Vector Machines Classifier',
       subtitle = paste('Predicted vs. Observed from Iris testing dataset. Accuracy: ', accuracy_svm_testing),
       y="Predicted", x="Observed") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# alternative
# 3.2 3lot the Confusion Matrix
cm_svm <- caret::confusionMatrix(factor(pred_svm), factor(testing$Species), dnn = c("Predicted", "Observed"))
plt <- as.data.frame(cm_svm$table)
plt$Predicted <- factor(plt$Predicted, levels=rev(levels(plt$Predicted)))

ggplot(plt, aes(Predicted, Observed, fill= Freq)) +
  geom_tile() + 
  geom_text(aes(label=Freq), size = 5) +
  scale_fill_gradient(low="white", high="darkgreen") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c(levels(factor(testing$Species)))) +
  scale_y_discrete(labels=rev(c(levels(factor(testing$Species))))) +
  labs(title = 'Confusion Matrix - Support Vector Machines Classifier',
       subtitle = paste('Predicted vs. Observed from Iris testing dataset. Accuracy: ', accuracy_svm_testing, '%'),
       y="Predicted", x="Observed") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))


#---------------------------------------------------
# 1.5 Neural Network (multiple class classification)
#---------------------------------------------------

library(neuralnet)

# 1. Build a Neural Network Classifier
set.seed(2023)
iris$setosa <- iris$Species=="setosa"
iris$virginica <- iris$Species == "virginica"
iris$versicolor <- iris$Species == "versicolor"

# spliting into test and training again because we added variables
set.seed(2023)
ind <- sample(2, nrow(iris),replace=TRUE,prob=c(0.7,0.3))
training <- iris[ind==1,]
testing <- iris[ind==2,]

nn_model <- neuralnet(setosa+versicolor+virginica ~ 
                        Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                      data=training, hidden=c(10,10), rep = 5, err.fct = "ce", 
                      linear.output = F, lifesign = "minimal", stepmax = 1000000,
                      threshold = 0.001)

pred_nn_training<- predict(nn_model, training, type = "class")
pred_nn_testing<- predict(nn_model, testing, type = "class")
predictions <- apply(pred_nn_testing, 1, which.max)
predictions_training <- apply(pred_nn_training, 1, which.max)

# 2. Create accuracy metrics table and coonfusion Matrix
predictions[which(predictions=="1")] <- levels(iris$Species)[1]
predictions[which(predictions=="2")] <- levels(iris$Species)[2]
predictions[which(predictions=="3")] <- levels(iris$Species)[3]

predictions_training[which(predictions_training=="1")] <- levels(iris$Species)[1]
predictions_training[which(predictions_training=="2")] <- levels(iris$Species)[2]
predictions_training[which(predictions_training=="3")] <- levels(iris$Species)[3]


tab_training <- table(predictions_training, training$Species)
tab_testing <- table(predictions, testing$Species)
result_nn_training <- caret::confusionMatrix(tab_training)
result_nn_testing <- caret::confusionMatrix(tab_testing)
result_nn_testing

# metrics on the training set
sensitivity_nn_training <- round(result_nn_training$byClass[, 1]*100, 2)
specificity_nn_training <- round(result_nn_training$byClass[, 2]*100, 2)
accuracy_nn_training <- round(result_nn_training$overall[1]*100, 2)
precision_nn_training <- round(mean(result_nn_training$byClass[,'Pos Pred Value'] *100), 2)
recall_nn_training <- round(mean(result_nn_training$byClass[, 'Sensitivity']*100), 2)
f1_score_nn_training <- round(2 * ((precision_nn_training * recall_nn_training) / (precision_nn_training + recall_nn_training)),2)

# metrics on the testing set
sensitivity_nn_testing <- round(result_nn_testing$byClass[, 1]*100, 2)
specificity_nn_testing <- round(result_nn_testing$byClass[, 2]*100, 2)
accuracy_nn_testing <- round(result_nn_testing$overall[1]*100, 2)
precision_nn_testing <- round(mean(result_nn_testing$byClass[,'Pos Pred Value'] *100), 2)
recall_nn_testing <-  round(mean(result_nn_testing$byClass[, 'Sensitivity']*100), 2)
f1_score_nn_testing <- round(2 * ((precision_nn_testing * recall_nn_testing) / (precision_nn_testing + recall_nn_testing)),2)


table_metrics <- matrix(c(accuracy_nn_training, accuracy_nn_testing,
                          precision_nn_training, precision_nn_testing,
                          recall_nn_training, recall_nn_testing, 
                          f1_score_nn_training, f1_score_nn_testing), byrow = TRUE, ncol = 2)
colnames(table_metrics) <-  c('Training', 'Testing')
rownames(table_metrics) <- c('accuracy', 'precision', 'recall', 'f1-score')
table_metrics

# export the results in LaTex document
print(xtable(table_metrics, type = "latex", digits=2), file = "tables.tex")

# 3.1 plot the Confusion Matrix
testing$pred_nn <- predictions
ggplot(testing, aes(Species, pred_nn, color = Species)) +
  geom_jitter(width = 0.2, height = 0.1, size=2) +
  labs(title = 'Confusion Matrix - Neural Networks classifier',
       subtitle = paste('Predicted vs. Observed from Iris dataset. Accuracy: '),
       y="Predicted", x="Observed") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# alternative
# 3.2 3lot the Confusion Matrix
testing$pred_nn <- predictions
cm_lrm <- caret::confusionMatrix(factor(testing$pred_nn), factor(testing$Species), dnn = c("Predicted", "Observed"))
plt <- as.data.frame(cm_lrm$table)
plt$Predicted <- factor(plt$Predicted, levels=rev(levels(plt$Predicted)))

ggplot(plt, aes(Predicted, Observed, fill= Freq)) +
  geom_tile() + 
  geom_text(aes(label=Freq), size = 5) +
  scale_fill_gradient(low="white", high="darkgreen") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c(levels(factor(testing$Species)))) +
  scale_y_discrete(labels=rev(c(levels(factor(testing$Species))))) +
  labs(title = 'Confusion Matrix - Neural Networks Classifier',
       subtitle = paste('Predicted vs. Observed from Iris testing dataset. Accuracy: ', accuracy_nn_testing, '%'),
       y="Predicted", x="Observed") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

#--------------------------------------------------
# 1.6 Decision tree (multiple class classification)
#--------------------------------------------------

library(rpart)

# 1. Build a Decision Tree Classifier
set.seed(2023)
dt_model <- rpart(Species ~.,
                  data = training,
                  method = "class",
                  control = rpart.control(cp = 0),
                  parms = list(split = "information"))

pred_dt_training<- predict(dt_model, training, type = "class")
pred_dt_testing<- predict(dt_model, testing, type = "class")

# 2. Create accuracy metrics table and coonfusion Matrix
tab_training <- table(pred_dt_training, training$Species)
tab_testing <- table(pred_dt_testing, testing$Species)
result_dt_training <- caret::confusionMatrix(tab_training)
result_dt_testing <- caret::confusionMatrix(tab_testing)
result_dt_testing

# metrics on the training set
sensitivity_dt_training <- round(result_dt_training$byClass[, 1]*100, 2)
specificity_dt_training <- round(result_dt_training$byClass[, 2]*100, 2)
accuracy_dt_training <- round(result_dt_training$overall[1]*100, 2)
precision_dt_training <- round(mean(result_dt_training$byClass[,'Pos Pred Value'] *100), 2)
recall_dt_training <- round(mean(result_dt_training$byClass[, 'Sensitivity']*100), 2)
f1_score_dt_training <- round(2 * ((precision_dt_training * recall_dt_training) / (precision_dt_training + recall_dt_training)),2)

# metrics on the testing set
sensitivity_dt_testing <- round(result_dt_testing$byClass[, 1]*100, 2)
specificity_dt_testing <- round(result_dt_testing$byClass[, 2]*100, 2)
accuracy_dt_testing <- round(result_dt_testing$overall[1]*100, 2)
precision_dt_testing <- round(mean(result_dt_testing$byClass[,'Pos Pred Value'] *100), 2)
recall_dt_testing <-  round(mean(result_dt_testing$byClass[, 'Sensitivity']*100), 2)
f1_score_dt_testing <- round(2 * ((precision_dt_testing * recall_dt_testing) / (precision_dt_testing + recall_dt_testing)),2)


table_metrics <- matrix(c(accuracy_dt_training, accuracy_dt_testing,
                          precision_dt_training, precision_dt_testing,
                          recall_dt_training, recall_dt_testing, 
                          f1_score_dt_training, f1_score_dt_testing), byrow = TRUE, ncol = 2)
colnames(table_metrics) <-  c('Training', 'Testing')
rownames(table_metrics) <- c('accuracy', 'precision', 'recall', 'f1-score')
table_metrics

# export the results in LaTex document
print(xtable(table_metrics, type = "latex", digits=2), file = "tables.tex")

# 3.1 plot the confusion matrix
table(predict(dt_model),training$Species)
pred_dt <- predict(dt_model, testing)
tab <- table(pred_dt, testing$Species)
result_dt <- caret::confusionMatrix(tab)
result_dt
testing$pred_dt <- pred_dt_testing
ggplot(testing, aes(Species, pred_dt_testing, color = Species)) +
  geom_jitter(width = 0.2, height = 0.1, size=2) +
  labs(title = 'Confusion Matrix - Decision Tree Classifier',
       subtitle = paste('Predicted vs. Observed from Iris testing dataset. Accuracy: ', accuracy_dt_testing),
       y="Predicted", x="Observed") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# alternative
# 3.2 3lot the Confusion Matrix
cm_dt <- caret::confusionMatrix(factor(pred_dt_testing), factor(testing$Species), dnn = c("Predicted", "Observed"))
plt <- as.data.frame(cm_dt$table)
plt$Predicted <- factor(plt$Predicted, levels=rev(levels(plt$Predicted)))

ggplot(plt, aes(Predicted, Observed, fill= Freq)) +
  geom_tile() + 
  geom_text(aes(label=Freq), size = 5) +
  scale_fill_gradient(low="white", high="darkgreen") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c(levels(factor(testing$Species)))) +
  scale_y_discrete(labels=rev(c(levels(factor(testing$Species))))) +
  labs(title = 'Confusion Matrix - Decision Tree Classifier',
       subtitle = paste('Predicted vs. Observed from Iris testing dataset. Accuracy: ', accuracy_dt_testing, '%'),
       y="Predicted", x="Observed") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

#--------------------------------------------
# 1.7 XGBoost (multiple class classification)
#--------------------------------------------

library(xgboost)

# 0. splitting the dataset into training and test sets
set.seed(2023)
ind <- sample(2, nrow(iris),replace=TRUE,prob=c(0.7,0.3))
training <- iris[ind==1,]
testing <- iris[ind==2,]

xgb_training = xgb.DMatrix(data = as.matrix(training[,-5]), label = training[,5])
xgb_testing = xgb.DMatrix(data = as.matrix(testing[,-5]), label = testing[,5])

# 1. Build a XGBoost Classifier
set.seed(2023)
xgb_model <- xgboost(data=xgb_training, max.depth=3, nrounds=50)

pred_xgb_testing <- predict(xgb_model, xgb_testing)
pred_y_xgb_testing = as.factor((levels(testing[,5]))[round(pred_xgb_testing)])

# 2. Create a Confusion Matrix
tab <- table(pred_species, testing$Species)
result_xgb <- caret::confusionMatrix(tab)

sensitivity_xgb <- round(result_xgb$byClass[, 1], 4)
specificity_xgb <- round(result_xgb$byClass[, 2], 4)
accuracy_xgb <- round(result_xgb$overall[1], 4)

# 3. Plot the Confusion Matrix
testing$pred_xgbspecies <- pred_species
ggplot(testing, aes(Species, pred_xgbspecies, color = Species)) +
  geom_jitter(width = 0.2, height = 0.1, size=2) +
  annotate('text', label = paste("Specificity (Setosa) =", specificity_xgb[1], ", Sensitivity (Setosa) =", sensitivity_xgb[1]
  ), x = 2, y = 0.75, size = 3) + 
  annotate('text', label = paste("Specificity (Versicolor) =", specificity_xgb[2], ", Sensitivity (Versicolor) =", sensitivity_xgb[2]
  ), x = 2, y = 0.65, size = 3) +
  annotate('text', label = paste("Specificity (Virginica) =", specificity_xgb[3], ", Sensitivity (Virginica) =", sensitivity_xgb[3]
  ), x = 2, y = 0.55, size = 3) +
  labs(title = 'Confusion Matrix - XGBoost',
       subtitle = paste('Predicted vs. Observed from Iris dataset. Accuracy: ', accuracy_xgb),
       y="Predicted", x="Observed") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

#---------------------------------
# Machine Learning algorithms in R
# Clustering algorithms
#---------------------------------

#--------------------------------
# 2. Principal Component Analysis
#--------------------------------

library(FactoMineR)
library(factoextra)
library(ggfortify)

df <- iris[1:4] # select only numeric columns

# 1. Scale iris dataset and perform PCA
pca_res <- prcomp(df, scale. = TRUE)

# 2. Visualize PCA results
autoplot(pca_res, data = iris, colour = 'Species', shape = FALSE, label.size = 3) + 
  labs(title = 'Principal Component Analysis',
       subtitle = 'On Iris Dataset')

#----------------------
# 2.1 K-means clustering
#----------------------

library(cluster)

# 1. Perform k-means clustering with 3 groups
kmeans_res <- kmeans(df, centers = 3, iter.max = 10)

# 2. Visualize PCA results with k-means clustering
autoplot(kmeans_res, data = df, label = TRUE, label.size = 3) + 
  labs(title = 'Principal Component Analysis with K-means clustering',
       subtitle = 'On Iris Dataset')

#------------------
# 2.3 PAM clustering
#------------------

# 1. Perform PAM clustering with 3 groups
pam_res <- pam(iris[-5], k = 3)

# 2. Visualize PCA results with Partition Around Medoids (PAM) clustering
autoplot(pam_res, data = df, frame = TRUE, frame.type = 'norm') + 
  labs(title = 'Principal Component Analysis - with PAM clustering',
       subtitle = 'On Iris Dataset')


#---------------------------------
# Machine Learning algorithms in R
# Regression algorithms
#---------------------------------

# 1. splitting the dataset into training and test sets
set.seed(2023)
ind <- sample(2, nrow(iris),replace=TRUE,prob=c(0.7,0.3))
training <- iris[ind==1,]
testing <- iris[ind==2,]

#-----------------------
# 1.1. linear regression
#-----------------------

# 1. Build a linear regression model
lr.model <- lm(Petal.Width ~ Sepal.Length, data = training)
summary(lr.model)

training$lr.pred <- predict(lr.model, type = 'response', newdata = training)
testing$lr.pred <- predict(lr.model, type = 'response', newdata = testing)

intercept <- round(lr.model$coefficients[1],3)
slope <- round(lr.model$coefficients[2],3)
rmse <- round((1/length(training[,1])) * sum((training$Petal.Width - training$lr.pred)^2) ,3)

# 2. Plot the regression line on the testing set

ggplot(testing, aes(x = Sepal.Length, y = Petal.Width, group = Species)) + 
  geom_point(aes(shape = Species, color = Species), size = 1.8) +
  geom_line(color='darkred', size = 1.2, data = testing, aes(x=Sepal.Length, y = lr.pred)) +
  annotate('text', label = paste("Intercept = ", intercept, ", Slope =", slope, ', RMSE = ', rmse
  ), x = 7, y = 0.35, size = 3) + 
  labs(title = 'Scatterplot - Linear Regression model',
       subtitle = 'Sepal.Length x Petal.Width on Iris testing dataset',
       y="Petal.Width", x="Sepal.Length") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

#---------------------------
# 1.2. log-linear regression
#---------------------------

# 1. Build a log-linear regression model
log.lin.model <- lm(log(Petal.Width) ~ Sepal.Length, data = training)
summary(log.lin.model)

training$log.lin.model <- predict(log.lin.model, type = 'response', newdata = training)
testing$log.lin.model <- predict(log.lin.model, type = 'response', newdata = testing)

intercept <- round(log.lin.model$coefficients[1],3)
slope <- round(log.lin.model$coefficients[2],3)
rmse <- round((1/length(training[,1])) * sum((training$Petal.Width - training$log.lin.model)^2) ,3)

# 2. Plot the regression line on the testing set

ggplot(testing, aes(x = Sepal.Length, y = log(Petal.Width), group = Species)) + 
  geom_point(aes(shape = Species, color = Species), size = 1.8) +
  geom_line(color='darkred', size = 1.2, data = testing, aes(x=Sepal.Length, y = log.lin.model)) +
  annotate('text', label = paste("Intercept = ", intercept, ", Slope =", slope, ', RMSE = ', rmse
  ), x = 7, y = -1.75, size = 3) + 
  labs(title = 'Scatterplot - Log-Linear Regression model',
       subtitle = 'Sepal.Length x Petal.Width on Iris testing dataset',
       y="log(Petal.Width)", x="Sepal.Length") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

#------------------------
# 1.3. Poisson regression
#------------------------

# 1. Build a Poisson regression model
pois.model <- glm(round(Petal.Width) ~ Sepal.Length, data = training, 
                  family=poisson(link="log"))
summary(pois.model)

training$pois.model <- predict(pois.model, type = 'response', newdata = training)
testing$pois.model <- predict(pois.model, type = 'response', newdata = testing)

intercept <- round(pois.model$coefficients[1],3)
slope <- round(pois.model$coefficients[2],3)
rmse <- round((1/length(training[,1])) * sum((training$Petal.Width - training$pois.model)^2) ,3)

# 2. Plot the regression line on the testing set

ggplot(testing, aes(x = Sepal.Length, y = Petal.Width, group = Species)) + 
  geom_point(aes(shape = Species, color = Species), size = 1.8) +
  geom_line(color='darkred', size = 1.2, data = testing, aes(x=Sepal.Length[order(testing$Sepal.Length)], y = pois.model[order(testing$Sepal.Length)])) +
  annotate('text', label = paste("Intercept = ", intercept, ", Slope =", slope, ', RMSE = ', rmse
  ), x = 7, y = 0.35, size = 3) + 
  labs(title = 'Scatterplot - Poisson Regression model',
       subtitle = 'Sepal.Length x Petal.Width on Iris testing dataset',
       y="Petal.Width", x="Sepal.Length") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

#----------------------
# 1.4. Gamma regression
#----------------------

# 1. Build a log-linear regression model
gam.model <- glm(Petal.Width ~ Sepal.Length, data = training, 
                 family=Gamma(link = "log"))
summary$gam.model

training$gam.model <- predict(gam.model, type = 'response', newdata = training)
testing$gam.model <- predict(gam.model, type = 'response', newdata = testing)

intercept <- round(gam.model$coefficients[1],3)
slope <- round(gam.model$coefficients[2],3)
rmse <- round((1/length(training[,1])) * sum((training$Petal.Width - training$gam.model)^2) ,3)

# 2. Plot the regression line on the testing set

ggplot(testing, aes(x = Sepal.Length, y = Petal.Width, group = Species)) + 
  geom_point(aes(shape = Species, color = Species), size = 1.8) +
  geom_line(color='darkred', size = 1.2, data = testing, aes(x=Sepal.Length[order(testing$Sepal.Length)], 
                                                             y = gam.model[order(testing$Sepal.Length)])) +
  annotate('text', label = paste("Intercept = ", intercept, ", Slope =", slope, ', RMSE = ', rmse
  ), x = 7, y = 0.35, size = 3) + 
  labs(title = 'Scatterplot - Gamma Regression model',
       subtitle = 'Sepal.Length x Petal.Width on Iris testing dataset',
       y="Petal.Width", x="Sepal.Length") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))


#------------------------------------------------------------------------------
# 3. Nonparametric regressions
#------------------------------------------------------------------------------

# 3.1 Kernel regression

Kreg = ksmooth(x = iris$Sepal.Length, y = iris$Sepal.Width,
               kernel = "normal", bandwidth = 1)

# 3.2 Ploting the regression
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, group = Species)) + 
  geom_point(aes(shape = Species, color = Species), size = 1.8) +
  geom_line(color='darkred', size = 1.2, data = iris, aes(x = Kreg$x,
                                                          y = Kreg$y)) +
  labs(title = 'Scatterplot - Nonparametric Kernel regression model',
       subtitle = 'Sepal.Length x Sepal.Width on Iris testing dataset',
       y="Sepal.Width", x="Sepal.Length") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))


# 3.2 smoothing splines

# 3.2 Ploting the regression
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point(aes(shape = Species, color = Species), size = 1.8) +
  geom_smooth(method = 'loess' , color='darkred', size = 1.2, se = FALSE) + 
  labs(title = 'Scatterplot - Nonparametric Smoothing Splines regression model',
       subtitle = 'Sepal.Length x Sepal.Width on Iris testing dataset',
       y="Sepal.Width", x="Sepal.Length") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

#----
# end
#----
