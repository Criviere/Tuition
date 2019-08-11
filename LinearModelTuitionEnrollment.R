# Importing the dataset
dataset <- read.csv('IncomingStudentsVsTuitionPrices.csv')

# Create subsets for both training & test set.
library(caTools)
set.seed(123)
split <- sample.split(dataset$Tuition, SplitRatio = .80)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)
dataset$Tuition <- as.numeric(gsub(",", "", dataset$Tuition))

# Test Correlation
cor(x = dataset$Tuition, y = dataset$Incoming)

# Write training_set to file
write.csv(training_set, file = "training_set.csv", row.names = FALSE)

# Write test_set to file
write.csv(test_set, file = "test_set.csv", row.names = FALSE)

# Fitting Simple Linear Regression to the Training set.
regressor <- lm(formula =  Incoming ~ Tuition, 
                data = training_set)

# Visualize Training Set Results
library(ggplot2)
ggplot() + 
  geom_point(data=training_set, mapping=aes(x = training_set$Tuition, y = training_set$Incoming, group = 1),
                      size = 3, shape = 22, colour = 'black') + 
  geom_text(data=training_set, mapping=aes(x=training_set$Tuition, y=training_set$Incoming, label=training_set$Year), size=2, vjust = -1.3, hjust = 0.3) +
  geom_line(aes(x = training_set$Tuition, y = predict(regressor, newdata = training_set), group = 1), 
            colour = 'blue') + 
  ggtitle('Incoming Students vs Tuition Prices (Training set)') + 
  xlab('Tuition Prices') + 
  ylab('Incoming Students')

# Visualize Test Set Results
library(ggplot2)
ggplot() + 
  geom_point(mapping=aes(x = test_set$Tuition, y = test_set$Incoming, group = 1), 
             size = 3, shape = 22, colour = 'red') + 
  geom_text(data=test_set, mapping=aes(x=test_set$Tuition, y=test_set$Incoming, label=test_set$Year), size=2, vjust = -1.3, hjust = 0.3) +
  geom_line(aes(x = training_set$Tuition, y = predict(regressor, newdata = training_set), group = 1), 
            colour = 'blue') + 
  ggtitle('Incoming Students vs Tuition Prices (Test set)') + 
  xlab('Tuition Prices') + 
  ylab('Incoming Students')