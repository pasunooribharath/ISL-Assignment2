#Chapter 6 Applied Problem 9

#a)
install.packages("ISLR")
library(ISLR)
attach(College)
# Randomly sample the 70% of the observations for train:
set.seed(3)
train_index <- sample(1:nrow(College), round(nrow(College) * 0.7))
train <- College[train_index, ]
nrow(train) / nrow(College)

#The remaining observations are allocated to the test dataset:
test <- College[-train_index, ]
nrow(test) / nrow(College)

#b)
model_linear <- lm(Apps ~ ., data = train)
summary(model_linear)


#Using MSE as the test error metric:
ols_pred <- predict(model_linear, test)
(ols_mse <- mean((ols_pred - test$Apps)^2))



#c)
install.packages("dplyr")
install.packages("caret") 
library(dplyr)
library(caret)

train_mat <- dummyVars(Apps ~ ., data = train, fullRank = F) %>%
  predict(newdata = train) %>%
  as.matrix()

test_mat <- dummyVars(Apps ~ ., data = test, fullRank = F) %>%
  predict(newdata = test) %>%
  as.matrix()

# I test varying values of λ (from 0.01 to 100) using 5-fold cross-validation:

install.packages("glmnet")
library(glmnet)
set.seed(3)

model_ridge <- cv.glmnet(y = train$Apps, 
                         x = train_mat, 
                         alpha = 0, 
                         lambda = 10^seq(2,-2, length = 100), 
                         standardize = TRUE, 
                         nfolds = 5)

data.frame(lambda = model_ridge$lambda, 
           cv_mse = model_ridge$cvm) %>%
  ggplot(aes(x = lambda, y = cv_mse)) + 
  geom_point() + 
  geom_line() + 
  geom_vline(xintercept = model_ridge$lambda.min, col = "deepskyblue3") +
  geom_hline(yintercept = min(model_ridge$cvm), col = "deepskyblue3") +
  scale_x_continuous(trans = 'log10', breaks = c(0.01, 0.1, 1, 10, 100), labels = c(0.01, 0.1, 1, 10, 100)) + 
  scale_y_continuous(labels = scales::comma_format()) + 
  theme(legend.position = "bottom") + 
  labs(x = "Lambda", 
       y = "Cross-Validation MSE", 
       col = "Non-Zero Coefficients:", 
       title = "Ridge Regression - Lambda Selection (Using 5-Fold Cross-Validation)")

model_ridge_best <- glmnet(y = train$Apps,
                           x = train_mat,
                           alpha = 0, 
                           lambda = 10^seq(2,-2, length = 100))

ridge_pred <- predict(model_ridge_best, s = model_ridge$lambda.min, newx = test_mat)
(ridge_mse <- mean((ridge_pred - test$Apps)^2))


#d)
set.seed(4)

model_lasso <- cv.glmnet(y = train$Apps, 
                         x = train_mat, 
                         alpha = 1, 
                         lambda = 10^seq(2, -2, length = 100), 
                         standardize = TRUE, 
                         nfolds = 5, 
                         thresh = 1e-12)

data.frame(lambda = model_lasso$lambda, 
           cv_mse = model_lasso$cvm, 
           nonzero_coeff = model_lasso$nzero) %>%
  ggplot(aes(x = lambda, y = cv_mse, col = nonzero_coeff)) + 
  geom_point() + 
  geom_line() + 
  geom_vline(xintercept = model_lasso$lambda.min, col = "deepskyblue3") +
  geom_hline(yintercept = min(model_lasso$cvm), col = "deepskyblue3") +
  scale_x_continuous(trans = 'log10', breaks = c(0.01, 0.1, 1, 10, 100), labels = c(0.01, 0.1, 1, 10, 100)) + 
  scale_y_continuous(labels = scales::comma_format()) + 
  theme(legend.position = "bottom") + 
  scale_color_gradient(low = "red", high = "green") +
  labs(x = "Lambda", 
       y = "Cross-Validation MSE", 
       col = "Non-Zero Coefficients:", 
       title = "Lasso - Lambda Selection (Using 5-Fold Cross-Validation)")
# Fitting the full model, and evaluating the test MSE:
model_lasso_best <- glmnet(y = train$Apps,
                           x = train_mat,
                           alpha = 1, 
                           lambda = 10^seq(2,-5, length = 100))

lasso_pred <- predict(model_lasso_best, s = model_lasso$lambda.min, newx = test_mat)
(lasso_mse <- mean((lasso_pred - test$Apps)^2))

#The coefficients are:
lasso_coef <- predict(model_lasso_best, type = "coefficients", s = model_lasso$lambda.min)

round(lasso_coef, 3)
