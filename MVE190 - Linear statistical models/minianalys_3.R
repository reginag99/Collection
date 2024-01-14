library(ggplot2)
library(dplyr)
library(leaps)
library(caret)
library(olsrr)
library(corrplot)


nydata <-read.csv("AirBnB_NYCity_2019.csv")
nydata <- data.frame(nydata)

#plotmap <- ggplot(nydata, aes(x = longitude, y = latitude)) +
#  geom_point()
#plotmap

nydata$room_type <- factor(nydata$room_type)
nydata$neighbourhood_group <- factor(nydata$neighbourhood_group)

nydata <- na.omit(nydata)

#Removing ads which have price 0, unrealistic and for log-transform
nydata <- nydata[nydata$price != 0, ] 
nydata$price_log <- log(nydata$price)

#Overview of data comment out since it takes time to run
#pairs(nydata[, c("price_log","minimum_nights","reviews_per_month", "number_of_reviews","room_type","neighbourhood_group")])

#Definig formulas for models
formula_full <- "price ~ minimum_nights + reviews_per_month + number_of_reviews + room_type + neighbourhood_group"
formula_full_log <- "price_log ~ minimum_nights + reviews_per_month + number_of_reviews + room_type + neighbourhood_group"

#Model using training/Test split
model_lm<-lm(formula_full_log,data = nydata,  x=TRUE)

X <- model_lm$x # the design matrix
X <- X[,-c(1)] # remove the intercept
y <- nydata$price_log

#Hur fungerar det med training data
frac <- 0.8 # training is 80% the whole dataset
trainindeces <- sample(seq(1,dim(X)[1]),round(dim(X)[1]*frac))
xx <- as.matrix(X[trainindeces,])   #Train data
yy <- y[trainindeces]               #Train data
xxt <- as.matrix(X[-trainindeces,]) #Test data
yyt <- y[-trainindeces]             #Test data

cor_matrix <- cor(xx)
corrplot(cor_matrix) 
highly_correlated <- findCorrelation(cor_matrix, cutoff = 0.9)

# If highly correlated variables found, consider removing some predictors
if (length(highly_correlated) > 0) {
  message("Highly correlated variables found. Consider removing some predictors.")
}

model_lm_2 <- lm(formula_full_log, data = nydata,  x=TRUE)
out<-ols_step_all_possible(model_lm_2) 
plot(out)
out

#Forcing all categorical variables
m<-regsubsets(xx,yy,force.in=c(4,5,6,7,8,9), int=T,nbest=1000,nvmax=dim(X)[2],method = c("ex"),really.big = T) 

cleaps<-summary(m,matrix=T)
cleaps$which
cleaps

tt<-apply(cleaps$which,1,sum) ## size of each model in the matrix
mses<-cleaps$rss/length(yy) ## corresponding MSEs

complexity <- c(rep(1,3),rep(2,3),3) #FORCING BOTH

plot(complexity, mses)

pmses<-rep(0,dim(cleaps$which)[1])
for (ta in (1:dim(cleaps$which)[1])) {
  mmr<-lm(yy~xx[,cleaps$which[ta,-1]==T]) 
  Xmat<-cbind(rep(1,dim(xxt)[1]),xxt[,cleaps$which[ta,-1]==T])
  PEcp<-sum((yyt-Xmat%*%mmr$coef)^2)/length(yyt) 
  pmses[ta]<-PEcp }

plot(complexity, pmses)


Models_1 <- cleaps$which
Models_1[1,-1]

selected_variables_1 <- names(Models_1[1, -1][Models_1[1, -1]])
selected_variables_1

chosenmod_1 <- lm(yy~xx[,selected_variables_1])  # fit the training data with model 1
betachosen_1 <- chosenmod_1$coefficients 
betachosen_1
designchosen_1 <- xxt[,selected_variables_1]
designchosen_1 <- cbind(rep(1,dim(designchosen_1)[1]),designchosen_1)
ypred_1 <- designchosen_1%*%betachosen_1  # obtain predictions using the covariates from the test set
plot(yyt,ypred_1, main = "Model 1 Test/Train split, reviews_per_month, number_of_reviews excluded",
     xlab = "Y test responses", ylab = "Y predictions from model")  # compare predictions with test responses
abline(0,1)

selected_variables_4 <- names(Models_1[4, -1][Models_1[4, -1]])
selected_variables_4

Models_1[4,-1]
chosenmod_4 <- lm(yy~xx[,selected_variables_4])  # fit the training data with model #1
betachosen_4 <- chosenmod_4$coefficients 
designchosen_4 <- xxt[,selected_variables_4]
designchosen_4 <- cbind(rep(1,dim(designchosen_4)[1]),designchosen_4)
ypred_4 <- designchosen_4%*%betachosen_4  # obtain predictions using the covariates from the test set
plot(yyt,ypred_4, main = "Model 2 Test/Train split, reviews_per_month excluded",
     xlab = "Y test responses", ylab = "Y predictions from model")  # compare predictions with test responses
abline(0,1)  

Models_1[7,-1]
selected_variables_7 <- names(Models_1[7,-1][Models_1[7,-1]])
selected_variables_7
chosenmod_7 <- lm(yy~xx[,selected_variables_7])  # fit the training data with model #1
betachosen_7 <- chosenmod_7$coefficients 
designchosen_7 <- xxt[,selected_variables_7]
designchosen_7 <- cbind(rep(1,dim(designchosen_7)[1]),designchosen_7)
ypred_7 <- designchosen_7%*%betachosen_7  # obtain predictions using the covariates from the test set


plot(yyt,ypred_7, main = "Model 3 Test/Train split",xlab = "Y test responses", 
     ylab = "Y predictions from model")

# compare predictions with test responses
abline(0,1)  

betachosen_1
betachosen_4
betachosen_7


##### K-Fold #####

model_k <-lm(formula_full_log,data = nydata,  x=TRUE)

X_k <- model_k$x # the design matrix
X_k <- X_k[,-c(1)] # remove the intercept
y_k <- nydata$price_log

cor_matrix <- cor(X_k)
highly_correlated <- findCorrelation(cor_matrix, cutoff = 0.9)

# If highly correlated variables found, consider removing some predictors
if (length(highly_correlated) > 0) {
  message("Highly correlated variables found. Consider removing some predictors.")
}

#Forcing all categorical variables
m_k<-regsubsets(X_k,y_k,force.in=c(4,5,6,7,8,9), int=T,nbest=1000,nvmax=dim(X_k)[2],method = c("ex"),really.big = T) #force.in=c(1:33)


cleaps_k<-summary(m_k,matrix=T)
cleaps_k$which
cleaps_k

Models <- cleaps_k$which
set.seed(123)
Models

K <- 10
n <- length(y_k)
ii <- sample(seq(1,n), n)  # vector of length n
foldsize <- floor(n/K)  
sizefold <- rep(foldsize,K)  # here each fold has size n/K
Prederrors <- matrix(0,dim(Models)[1],K)
Prederrors
iused <- 0  # initialize number of indeces used for testing

for (k in (1:K)) {
  itest <- ii[(iused + 1):(iused + sizefold[k])]
  itrain <- ii[-c((iused + 1):(iused + sizefold[k]))]
  iused <- iused + length(itest) 
  Prederrors[1,k] <- sum((y_k[itest] - mean(y_k[itrain]))^2) # intercept only 
  for (mm in (1:dim(Models)[1])) {  # loops through all model subsets 
    xtrain <- X_k[itrain,Models[mm,2:dim(Models)[2]]]  # extract training design matrix for model mm 
    xtrain <- cbind(rep(1, dim(xtrain)[1]), xtrain)  # add in the intercept column of ones since we now need to estimate also the intercept via least squares
    betahat <- solve(t(xtrain)%*%xtrain)%*%t(xtrain)%*%y_k[itrain]  # least squares formula for parameter estimates
    xtest <- X_k[itest,Models[mm,2:dim(Models)[2]]]  # extract test design matrix for model mm 
    xtest <- cbind(rep(1, dim(xtest)[1]), xtest)  # add in the intercept column of ones since we now need to make predictions
    ypred<-xtest%*%betahat
    Prederrors[mm,k]<-sum((y_k[itest]-ypred)^2) #Calculate prediction errors
  }
}

Prederrors

PE <- apply(Prederrors,1,sum)/n  # final prediction errors.

Models

rep

complexity <- c(rep(1,3),rep(2,3),3) #FORCING BOTH

length(complexity)
length(PE)

PE

plot(complexity,PE, main = "Predicion error (pMSE) and model complexity", ylab = "prediction error (pMSE)")

Models[1,-1]
Models[5,-1]
Models[7,-1]

#Models without intercept!!
selected_variables_1 <- names(Models_1[1, -1][Models_1[1, -1]])
selected_variables_1

chosenmod <- lm(y_k[itrain]~X_k[itrain, selected_variables_1])  # fit the training data with model #1
betachosen <- chosenmod$coefficients  # parameter estimates from training data
betachosen
# select "active variables" in TEST data, corresponding to model #1
designchosen <- X_k[itest,selected_variables_1]
#add a column of ones for the intercept
designchosen <- cbind(rep(1,dim(designchosen)[1]),designchosen)
ypred <- designchosen%*%betachosen  # obtain predictions using the covariates from the test set
plot(y_k[itest],ypred, main = "Model 1 (variables 3)", 
     xlab = "Y test responses", ylab = "Y predictions from model")  # compare predictions with test responses
abline(0,1)  

selected_variables_5 <- names(Models_1[5, -1][Models_1[5, -1]])
selected_variables_5

chosenmod <- lm(y_k[itrain]~X_k[itrain,selected_variables_5])  # fit the training data with model #1
betachosen <- chosenmod$coefficients  # parameter estimates from training data
betachosen
# select "active variables" in TEST data, corresponding to model #1
designchosen <- X_k[itest,selected_variables_5]
#add a column of ones for the intercept
designchosen <- cbind(rep(1,dim(designchosen)[1]),designchosen)
ypred <- designchosen%*%betachosen  # obtain predictions using the covariates from the test set
plot(y_k[itest],ypred,main = "Model 5 (variables 4)",
     xlab = "Y test responses", ylab = "Y predictions from model")  # compare predictions with test responses
abline(0,1)

selected_variables_7 <- names(Models_1[7, -1][Models_1[7, -1]])
selected_variables_7

chosenmod <- lm(y_k[itrain]~X_k[itrain,selected_variables_7])  # fit the training data with model #1
betachosen <- chosenmod$coefficients  # parameter estimates from training data
betachosen
# select "active variables" in TEST data, corresponding to model #1
designchosen <- X_k[itest,selected_variables_7]
#add a column of ones for the intercept
designchosen <- cbind(rep(1,dim(designchosen)[1]),designchosen)
ypred <- designchosen%*%betachosen  # obtain predictions using the covariates from the test set
plot(y_k[itest],ypred,main = "Model 7 (variables 5)", 
     xlab = "Y test responses", ylab = "Y predictions from model")  # compare predictions with test responses
abline(0,1)   


##Partial F-test
chosenmod1 <- lm(y_k[itrain]~X_k[itrain,Models[1,-1]==T]) #alla utom numbers per mounth och number of reviews
chosenmod2 <- lm(y_k[itrain]~X_k[itrain,Models[5,-1]==T]) #alla utom numbr of reviws
chosenmod3 <- lm(y_k[itrain]~X_k[itrain,Models[7,-1]==T]) #alla

partial_f_test_1vs2 <- anova(chosenmod1,chosenmod2)
#Analysis of Variance Table

#Model 1: y_k[itrain] ~ X_k[itrain, Models[1, -1] == T]
#Model 2: y_k[itrain] ~ X_k[itrain, Models[5, -1] == T]
#Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
#1  34942 7960.6                                  
#2  34941 7946.0  1    14.612 64.253 1.128e-15 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#shows that number of rviws have a significant impact
partial_f_test_1vs3 <- anova(chosenmod1,chosenmod3)
#Analysis of Variance Table

#Model 1: y_k[itrain] ~ X_k[itrain, Models[1, -1] == T]
#Model 2: y_k[itrain] ~ X_k[itrain, Models[7, -1] == T]
#Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
#1  34942 7960.6                                  
#2  34940 7946.0  2    14.613 32.128 1.148e-14 ***
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#shows that the missing parameters have a significant impact

partial_f_test_2vs3 <- anova(chosenmod2,chosenmod3)
#Analysis of Variance Table

#Model 1: y_k[itrain] ~ X_k[itrain, Models[5, -1] == T]
#Model 2: y_k[itrain] ~ X_k[itrain, Models[7, -1] == T]
#Res.Df  RSS Df Sum of Sq      F Pr(>F)
#1  34941 7946                           
#2  34940 7946  1 0.0010281 0.0045 0.9464

#shows that numbers of reviews per mounth is not significant compared to a model already including number of reviews per mounth

partial_f_test_1vs2
partial_f_test_1vs3
partial_f_test_2vs3

