cutomer_details <- read.csv("../data/customer_id.csv")

cutomer_details <- cutomer_details[, c(1, 2, 3, 5, 6, 4)]

attach(cutomer_details)

gender <- factor(gender)
pay_freq <- factor(pay_freq)

cutomer_details$gender_num = as.numeric(gender)
cutomer_details$pay_freq_num = as.numeric(pay_freq)

# Data Explanatory Analysis
par(mfrow=c(1,2))
hist(annual_salary, xlab="Annual Salary", main="Histogram of Annual Salary")
hist(log(annual_salary), xlab="log(Annual Salary)", main="Histogram of log(Annual Salary)")


transform_y <- cutomer_details[,5:8]
transform_y$annual_salary <- log(transform_y$annual_salary)

names(transform_y)[1] <- "log_annual_salary"
names(transform_y)[3] <- "gender"
names(transform_y)[4] <- "pay_freq"


pairs(transform_y)


lm <- lm(transform_y$log_annual_salary~transform_y$pay_freq)
summary(lm)
