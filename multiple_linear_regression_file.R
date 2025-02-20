# Multiple Linear Regression

# Importing the Dataset
dataset = read.csv('50_Startups.csv')

# Encoding categorical data
dataset$State = factor(dataset$State,
                         levels = c('New York', 'California', 'Florida'),
                         labels = c(1, 2, 3))

# Splitting the Dataset into Training and Test Set
install.packages('caTools')

# Select the caTools
library(caTools)
set.seed(123)

# Splitting the Data into Train and Test Split
split = sample.split(dataset$Profit, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Multiple Linear Regression to the Training Set
regressor = lm(formula = Profit ~ .,
               data = training_set)

# Predicting the Test Set Results
y_pred = predict(regressor, newdata = test_set)

# Building the Optimal Model using Backward Elimination
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = dataset)
summary(regressor)

# Removing the State Independent Variable
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
               data = dataset)
summary(regressor)

# Removing the Administration Independent Variable
regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
               data = dataset)
summary(regressor)

# Removing the Marketing.Spend Independent Variable
regressor = lm(formula = Profit ~ R.D.Spend,
               data = dataset)
summary(regressor)