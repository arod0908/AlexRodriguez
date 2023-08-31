rm(list = ls(all = TRUE));
graphics.off()

library(readxl)
library(xlsx)
library(corrplot)
library(MASS)
library(caTools)
library(ggplot2)
library(car)
library(tidyverse)
library(broom)
library(glmnet)


NBA_data <- read.xlsx("/Users/alexrodriguez/Desktop/Graduate Classes/Data Mining I/Final/Data mining I NBA data.xlsx", 1)
NBA_Current <- read.xlsx("/Users/alexrodriguez/Desktop/Graduate Classes/Data Mining I/Final/Current Season.xlsx", 1)


names(NBA_data)[6] ='Win %'
names(NBA_data)[10] ='FG %'
names(NBA_data)[11] ='3PM'
names(NBA_data)[12] ='3PA'
names(NBA_data)[13] ='3P %'
names(NBA_data)[16] ='FT %'
names(NBA_data)[27] ='Plus/Minus'
names(NBA_data)[31] ='Ast %'
names(NBA_data)[32] ='Ast/TO'
names(NBA_data)[33] ='Ast Ratio'
names(NBA_data)[34] ='OREB %'
names(NBA_data)[35] ='DREB %'
names(NBA_data)[36] ='REB %'
names(NBA_data)[37] ='TOV %'
names(NBA_data)[38] ='EFG %'
names(NBA_data)[39] ='TS %'

names(NBA_Current)[5] ='Win %'
names(NBA_Current)[9] ='FG %'
names(NBA_Current)[10] ='3PM'
names(NBA_Current)[11] ='3PA'
names(NBA_Current)[12] ='3P %'
names(NBA_Current)[15] ='FT %'
names(NBA_Current)[26] ='Plus/Minus'
names(NBA_Current)[30] ='Ast %'
names(NBA_Current)[31] ='Ast/TO'
names(NBA_Current)[32] ='Ast Ratio'
names(NBA_Current)[33] ='OREB %'
names(NBA_Current)[34] ='DREB %'
names(NBA_Current)[35] ='REB %'
names(NBA_Current)[36] ='TOV %'
names(NBA_Current)[37] ='EFG %'
names(NBA_Current)[38] ='TS %'

NBA_data_df <- NBA_data[,-c(1,2,3,4,5)]
#Descriptive Statistics of our data set
summary(NBA_data_df)

#Correlation Matrix plot
CorMatrix <- cor(NBA_data_df)
par(mfrow=c(1,1))
corrplot(CorMatrix, method="color")

#Correlation Matrix
Correlation <- cor(NBA_data_df)
Correlation

#Plot of all the Variables Boxplots
boxplot(NBA_data_df)

hist(NBA_data_df$`Win %`)

NBA_data_train  <- NBA_data_df[1:270, ]
NBA_data_test   <- NBA_data_df[271:300, ]


#Model 1 is the full model with all the predictor variables. 
Full_Model <- lm(`Win %` ~ ., data = NBA_data_train)
summary(Full_Model)
AIC(Full_Model)
plot(Full_Model)
vif(Full_Model)

#Reduce the model by removing the variables with high Collinearity first then run 
#Backward regression on the model to obtain the 'best model'. 

#Removing the variables with high Collinearity
Reduced_Model <- lm(formula = `Win %` ~ `FG %` + `3PA` + `3P %` + FTA + `FT %` + OREB + PF + TOV + STL 
                    + BLK + BLK + PF + PFD + DEFRTG + `Ast %` 
                    + `DREB %` + PACE, data = NBA_data_train)
summary(Reduced_Model)
AIC(Reduced_Model)
vif(Reduced_Model)

#Running Backward regression on the new model to obtain the 'best model'
Best_model <- stepAIC(Reduced_Model, direction = "backward", trace = FALSE)
summary(Best_model)
AIC(Best_model)
par(mfrow=c(2,2))
plot(Best_model)
vif(Best_model)

Best_model$anova
mean(Best_model$residuals^2) #MSE Train data

#Step 1 - create the evaluation metrics function

eval_metrics <- function(model, df, predictions, target){
  resids <- df[,target] - predictions
  resids2 <- resids**2
  N <- length(predictions)
  r2 <- as.character(round(summary(model)$r.squared, 2))
  adj_r2 <- summary(model)$adj.r.squared
  RMSE <- sqrt(sum(resids2)/N)
  #print(adj_r2) #Adjusted R-squared
  #print(as.character(round(sqrt(sum(resids2)/N), 2))) #RMSE
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = adj_r2
  )
  
}

# Step 2 - predicting and evaluating the model on train data
predictions_train <- predict(Best_model, newdata = NBA_data_train)
eval_metrics(Best_model, NBA_data_train, predictions_train, target = 'Win %')

# Step 3 - predicting and evaluating the model on test data
predictions_test <- predict(Best_model, newdata = NBA_data_test)
eval_metrics(Best_model, NBA_data_test, predictions_test, target = 'Win %') 

mean((NBA_data_test$`Win %` - predictions_test)^2) #MSE Test data

plot(predictions_test, NBA_data_test$`Win %`,
     xlab = "Predicted Values",
     ylab = "Observed Values")
abline(a = 0, b = 1, lwd=2,
       col = "red")

predictions_current <- predict(Best_model, newdata = NBA_Current)
predictions_current

predictions_current <- as.data.frame(predictions_current)
Blub <- cbind(NBA_Current$Rank, NBA_Current$TEAM, NBA_Current$`Win %`, predictions_current)
#blub2 <- Blub[order(predictions_current,)]
Blub$Rank<-rank(-Blub$predictions_current, )
#order(Blub$Rank, decreasing = F)
write.csv(Blub,"/Users/alexrodriguez/Desktop/Graduate Classes/Data Mining I/Final/Blub.csv", row.names = FALSE)



NBA_data_train
x_train <- as.matrix(NBA_data_train[,-c(1)])
y_train <- as.matrix(NBA_data_train[,c(1)])

NBA_data_test
x_test <- as.matrix(NBA_data_test[,-c(1)])
y_test <- as.matrix(NBA_data_test[,c(1)])

# The Ridge Regression
library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.fit <- cv.glmnet(x_train,y_train,type.measure="mse", alpha=0, 
                       family="gaussian", lambda = grid) # alpha=0 Ridge
ridge.fit$lambda.min
ridge.fit$lambda.1se

ridge.fit.predicted <- predict(ridge.fit, s=ridge.fit$lambda.min, newx=x_test)

mean((ridge.fit.predicted-y_test)^2) 

plot(ridge.fit)
print(coef(ridge.fit))

plot(ridge.fit.predicted, y_test,
     xlab = "Predicted Values",
     ylab = "Observed Values")
abline(a = 0, b = 1, lwd=2,
       col = "red")

#ridge path
plot(ridge.fit$glmnet.fit,"lambda", label=FALSE)

eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE <- sqrt(SSE/nrow(df))
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
}

predictions_train <- predict(ridge.fit$glmnet.fit, s = ridge.fit$lambda.min, newx = x_train)
eval_results(y_train, predictions_train, NBA_data_train)
mean((predictions_train-y_train)^2) 

predictions_test <- predict(ridge.fit$glmnet.fit, s = ridge.fit$lambda.min, newx = x_test)
eval_results(y_test, predictions_test, NBA_data_test)
mean((predictions_test-y_test)^2) 










