
library(car)  # For VIF to check multicollinearity
library(MASS) # For stepwise regression (stepAIC)

data1 <- read.csv("C:\\Users\\Razaqa\\Downloads\\GP complete.csv")


data1[data1 == 0] <- NA  
data1 <- na.omit(data1)  


Regression <- lm(Loss.for.the.year ~ Year + Finance.costs + Market.Price + 
                   No..Of.employees + Income.tax.expense + Revenue + 
                   Cost.of.sales + Operating.profit..loss. + Gross.Profit + 
                   Payables, data = data1)


summary(Regression)


vif_values <- vif(Regression)
print(vif_values)

#Multiple linear regression model
Regression <- lm(Loss.for.the.year ~ Finance.costs + Revenue + 
                   Cost.of.sales + Operating.profit..loss. + Gross.Profit, 
                 data = data1)


summary(Regression)


Regression <- stepAIC(Regression, direction = "both")  # Both forward and backward selection
summary(Regression)

#  Diagnostics 
alias_check <- alias(Regression)
print(alias_check)  # Lists variables with aliasing issues, if any

# Optional: Check variance of predictors to ensure no near-zero variance
variances <- apply(data1[, -1], 2, var)  # Exclude the dependent variable
print(variances)

# Predictions
data1$Predicted <- predict(Regression, newdata = data1)

scaled_actual <- data1$Loss.for.the.year / 1e6
scaled_predicted <- data1$Predicted / 1e6

#  Plot Actual vs Predicted Values
plot(scaled_actual, scaled_predicted, 
     main = "Predicted vs Actual Loss for the Year",
     xlab = "Actual Loss for the Year (in Millions)", 
     ylab = "Predicted Loss for the Year (in Millions)", 
     pch = 16, col = "blue")
abline(0, 1, col = "red", lwd = 2)

summary(Regression)

plot(Regression)

data1$Predicted_Loss <- predict(Regression, newdata = data1)

head(data1)  

# Random forest model
library(randomForest)
rf_model <- randomForest(Loss.for.the.year ~ Year + Finance.costs + Market.Price + 
                           No..Of.employees + Income.tax.expense + Revenue + 
                           Cost.of.sales + Operating.profit..loss. + Gross.Profit + 
                           Payables, data = data1, importance = TRUE)

print(rf_model)

plot(rf_model)

data1$rfPredicted <- predict(rf_model, newdata = data1)

# Get variable importance
importance(rf_model)

# Visualize variable importance
varImpPlot(rf_model)
