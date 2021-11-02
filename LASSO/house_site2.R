#=========================================================================#
# Financial Econometrics & Derivatives, ML/DL using R, Python, Tensorflow 
# by Sang-Heon Lee
#
# https://kiandlee.blogspot.com
#-------------------------#
# Lasso, Ridge
#=========================================================================#

library(glmnet)

graphics.off()  # clear all graphs
rm(list = ls()) # remove all files from your workspace

data <- read.csv("~/Insper/Data/Projeto Macro/Book1.csv")
data <- na.omit(data)
data <- scale(data)

X <- data[, 1:13]
y <- data[, 14]
#---------------
# Model
#---------------
lambda <- 0.01

# standard linear regression without intercept(-1)
li.eq <- lm(y ~ X-1) 

# lasso
la.eq <- glmnet(X, y, lambda=lambda, 
                family="gaussian", 
                intercept = F, alpha=1) 

# Ridge
ri.eq <- glmnet(X, y, lambda=lambda, 
                family="gaussian", 
                intercept = F, alpha=0) 

#---------------
# Results (lambda=0.01)
#---------------
df.comp <- data.frame(
  Linear  = li.eq$coefficients,
  Lasso   = la.eq$beta[,1],
  Ridge   = ri.eq$beta[,1]
)
df.comp

#---------------
# Results (lambda=0.1)
#---------------
lambda <- 0.1

# lasso
la.eq <- glmnet(X, y, lambda=lambda,
                family="gaussian",
                intercept = F, alpha=1) 
# Ridge
ri.eq <- glmnet(X, y, lambda=lambda,
                family="gaussian",
                intercept = F, alpha=0) 


df.comp <- data.frame(
  Linear  = li.eq$coefficients,
  Lasso   = la.eq$beta[,1],
  Ridge   = ri.eq$beta[,1]
)
df.comp

#----------------
# Shrinkage of coefficients 
# (rangle lambda input or without lambda input)
#----------------

# lasso
la.eq <- glmnet(X, y, family="gaussian", 
                intercept = F, alpha=1) 
# Ridge
ri.eq <- glmnet(X, y, family="gaussian", 
                intercept = F, alpha=0) 
# plot
x11(); par(mfrow=c(2,1)) 
x11(); matplot(log(la.eq$lambda), t(la.eq$beta),
               type="l", main="Lasso", lwd=2)
x11(); matplot(log(ri.eq$lambda), t(ri.eq$beta),
               type="l", main="Ridge", lwd=2)

#----------------    
# Run cross-validation & select lambda
#----------------
mod_cv <- cv.glmnet(x=X, y=y, family="gaussian",
                    intercept = F, alpha=1)

# plot(log(mod_cv$lambda), mod_cv$cvm)
# cvm : The mean cross-validated error 
#     - a vector of length length(lambda)

# lambda.min : the ?? at which 
# the minimal MSE is achieved.

# lambda.1se : the largest ?? at which 
# the MSE is within one standard error 
# of the minimal MSE.

x11(); plot(mod_cv) 
coef(mod_cv, c(mod_cv$lambda.min,
               mod_cv$lambda.1se))
print(paste(mod_cv$lambda.min,
            log(mod_cv$lambda.min)))
print(paste(mod_cv$lambda.1se,
            log(mod_cv$lambda.1se)))
