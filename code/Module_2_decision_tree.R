cutomer_details <- read.csv("../data/customer_id.csv")
attach(cutomer_details)

library(dplyr)
# Drop variables
cutomer_details <- subset(cutomer_details, select=-c(customer_id,amount))

cutomer_details$gender <- factor(cutomer_details$gender)
cutomer_details$pay_freq <- factor(cutomer_details$pay_freq)


cutomer_details$annual_salary <- cut(
  cutomer_details$annual_salary,
  breaks = c(0, 60000.00, 135000.00),
  labels = c("Less Than 60K", "More Than 60K"),
  right  = FALSE
)

cutomer_details$age <- cut(
  cutomer_details$age,
  breaks = c(18, 30, 80),
  labels = c("Age under 30", "Age 30 and above"),
  right  = FALSE
)

glimpse(cutomer_details)




library(caret)
Train = createDataPartition(cutomer_details$annual_salary, p=0.75, list=FALSE)
#split data in 75%-25% ratio


training = cutomer_details[ Train, ] #75% training
testing = cutomer_details[ -Train, ]

library(rpart)
library(rpart.plot)
fit <- rpart(annual_salary~., data = training, method = 'class')
rpart.plot(fit)


predict_annual_salary <-predict(fit, testing, type = 'class')

table_mat <- table(testing$annual_salary, predict_annual_salary)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))
