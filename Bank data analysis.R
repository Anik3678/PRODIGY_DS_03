#Prodigy Infotech DS Task-3

#Build a decision tree classifier to predict whether a customer will purchase a product or service based on their demographic & behavioral data.
#Use a dataset such as the bank marketing dataset from the UCI machine learning repository.

#Load the data
bank<-read.csv("C:\\Users\\User\\Documents\\Data science\\Internship\\Prodigy Infotech Internship\\Task-3\\bank.csv",sep = ";")

#View the data
View(bank)

#Observing no. of rows & columns of the dataset
dim(bank)

#Checking for missing value
any(is.na(bank))

#Checking for duplicated values
any(duplicated(bank))

# Install packages 
install.packages("rpart")
install.packages("rpart.plot")

# Load the libraries
library(rpart)
library(rpart.plot)


# Convert the target variable to a factor
y<-as.factor(bank$y)
y

# Set seed for reproducibility
set.seed(123)

# Split the data into training (70%) and testing (30%) sets
sample_index <- sample(1:nrow(bank), 0.7 * nrow(bank))
train_data <- bank[sample_index, ]
test_data <- bank[-sample_index, ]


#Build the decision tree model
decision_tree_model <- rpart(y ~ ., data = train_data)

# View the summary of the model
summary(decision_tree_model)


# Plot the decision tree
rpart.plot(decision_tree_model,extra=108, main = "Decision Tree for Customer Purchase Prediction")


# Predict on the test set
predictions <- predict(decision_tree_model, test_data, type = "class")

# Create a confusion matrix to evaluate the model
confusion_matrix <- table(test_data$y, predictions)
print(confusion_matrix)

# Calculate the accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy

#Accuracy of the model
accuracy<-mean(predictions==test_data$y)
accuracy
