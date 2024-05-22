
# Load required libraries
library(dplyr)
library(ggplot2)
library(reshape2)
library(summarytools)
library(caret)
library(factoextra)

# Load the dataset
setwd("C:/Users/HP/Desktop")
proj_data <- read.csv("project data.csv", header = T) 

# Preliminary analysis
proj_data <- proj_data[proj_data$Group %in% c("Nondemented", "Demented"), ]
proj_data$M.F <- ifelse(proj_data$M.F == "M", 1, 0)
proj_data <- proj_data[proj_data$Group != "Converted", ]
proj_data <- na.omit(proj_data)
attach(proj_data)

# descriptive statistics table
table(M.F)
table(Group)
summary(proj_data)


# Histograms of all variables
par(mfrow = c(3, 3))
for (i in 3:10) {
  hist(proj_data[, i], main = "", col = "red", xlab = names(proj_data)[i])
}

# Scaling of all variables
Group <- ifelse(Group == "Nondemented", 0, 1)
proj_scale <- scale(proj_data[,2:10], center = TRUE, scale = FALSE)
# k means clustering 
set.seed(123)
kmeans_clusters <- kmeans(proj_scale, centers=2, nstart = 1)
kmeans_clusters
fviz_cluster(kmeans_clusters, data = proj_scale,stand = FALSE)

# feature selection
Group <- factor(Group)
# backward selection
model_1 <- glm(Group ~ ., data = proj_data, family = binomial, maxit = 100)
step_1 <- step(model_1, method = "backward")
summary(step_1)

# forward selection
model_2 <- glm(Group ~ 1, data = proj_data, family = binomial)
step_2<-step(model_2,scope = ~ Group + M.F + Age + EDUC +	SES +	MMSE + CDR +	eTIV	+ nWBV +	ASF,
            method='forward')
summary(step_2)

#split data into training and test data
set.seed(123)
trainingRowIndex <- sample(1:nrow(proj_data), 0.8*nrow(proj_data))  # row indices for 80% training data
trainingData <- proj_data[trainingRowIndex, ]  # model training data
testData  <- proj_data[-trainingRowIndex, ]

y_test<- testData$Group
testData<- testData[,-1]

# Fit logistic regression model
model <- glm(Group ~ EDUC +	MMSE + CDR  +	ASF, data = trainingData, family = binomial)
summary(model)

#Making predictions
y_test<- ifelse(y_test== 1, "Demented", "Nondemented")
glm.probs <- predict(model,newdata = testData,type="response") #Pr(Y=1|X)
confusion_matrix<- table(y_test, glm.probs>0.5)
confusion_matrix


