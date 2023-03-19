#---------------------------------
# Machine Learning algorithms in R
# Classification algorithms
#---------------------------------

library(GGally)

data(iris)

#-----------------
# 0. Visualization
#-----------------

# 1. multiple plots 
ggpairs(iris, ggplot2::aes(colour = Species, alpha = 0.4))

library(ggplot2)
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

# 2. Create a coonfusion Matrix
tab <- table(pred_nb$class, testing$Species)
result_nb <- caret::confusionMatrix(tab)
result_nb

sensitivity_nb <- round(result_rf$byClass[, 1], 4)
specificity_nb <- round(result_rf$byClass[, 2], 4)
accuracy_nb <- round(result_nb$overall[1], 4)

# 3. Plot the Confusion Matrix

library(ggplot2)

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
accuracy_rf <- round(result_rf$overall[1], 4)

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
       subtitle = paste('Predicted vs. Observed from Iris dataset. Accuracy: ', accuracy_rf),
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
summary(mlr_model)
pred_mlr<- predict(mlr_model, testing, type = "response")
predictions <- apply(pred_mlr, 1, which.max)

# 2. Create a Confusion Matrix
predictions[which(predictions=="1")] <- levels(iris$Species)[1]
predictions[which(predictions=="2")] <- levels(iris$Species)[2]
predictions[which(predictions=="3")] <- levels(iris$Species)[3]
tab <- table(predictions, testing$Species)

result_mlr <- caret::confusionMatrix(tab)
result_mlr

# 3. Plot the Confusion Matrix
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

result_svm <- caret::confusionMatrix(pred_svm, testing$Species)
sensitivity_svm <- round(result_svm$byClass[, 1], 4)
specificity_svm <- round(result_svm$byClass[, 2], 4)
accuracy_svm <- round(result_svm$overall[1], 4)

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
       subtitle = paste('Predicted vs. Observed from Iris dataset. Accuracy: ', accuracy_svm),
       y="Predicted", x="Observed") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

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
       subtitle = paste('Predicted vs. Observed from Iris dataset. Accuracy: '),
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

pred_dt <- predict(dt_model, testing, type = 'class')

# 2. Create a Confusion Matrix
tab <- table(pred_dt, testing$Species)
result_dt <- caret::confusionMatrix(tab)

result_dt <- caret::confusionMatrix(pred_dt, testing$Species)
sensitivity_dt <- round(result_dt$byClass[, 1], 4)
specificity_dt <- round(result_dt$byClass[, 2], 4)
accuracy_dt <- round(result_dt$overall[1], 4)

# 3. Plot the Confusion Matrix
testing$pred_dt <- pred_dt
ggplot(testing, aes(Species, pred_dt, color = Species)) +
  geom_jitter(width = 0.2, height = 0.1, size=2) +
  annotate('text', label = paste("Specificity (Setosa) =", specificity_dt[1], ", Sensitivity (Setosa) =", sensitivity_dt[1]
  ), x = 2, y = 0.75, size = 3) + 
  annotate('text', label = paste("Specificity (Versicolor) =", specificity_dt[2], ", Sensitivity (Versicolor) =", sensitivity_dt[2]
  ), x = 2, y = 0.65, size = 3) +
  annotate('text', label = paste("Specificity (Virginica) =", specificity_dt[3], ", Sensitivity (Virginica) =", sensitivity_dt[3]
  ), x = 2, y = 0.55, size = 3) +
  labs(title = 'Confusion Matrix - Decision Tree',
       subtitle = paste('Predicted vs. Observed from Iris dataset. Accuracy: ', accuracy_dt),
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

# 1. splitting the dataset into training and test sets
set.seed(2023)
ind <- sample(2, nrow(iris),replace=TRUE,prob=c(0.7,0.3))
training <- iris[ind==1,]
testing <- iris[ind==2,]

xgb_training = xgb.DMatrix(data = as.matrix(training[,-5]), label = training[,5])
xgb_testing = xgb.DMatrix(data = as.matrix(testing[,-5]), label = testing[,5])

# 1. Build a XGBoost Classifier
set.seed(2023)
xgb_model <- xgboost(data=xgb_training, max.depth=3, nrounds=50)

pred_xgb <- predict(xgb_model, xgb_testing)
pred_species = as.factor((levels(testing[,5]))[round(pred_xgb)])

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
