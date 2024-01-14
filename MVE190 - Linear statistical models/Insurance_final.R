library(ggplot2)
library(dplyr)
library(caret)
library(leaps)
library(olsrr)
library(gridExtra)
library(MASS)
library(corrplot)
library(car)

df <-read.csv("medinsur.csv")
str(df) 

#Factor for categorical covariates
df$sex <- factor(df$sex)
df$smoker <- factor(df$smoker)
df$region <- factor(df$region)
df$children_factor <- factor(df$children)

#:::::: Overview of data and transformations ::::::::::::::::: ----

par(mfrow=c(2,3))
plot(df$age, df$charges, main="Charges vs Age")
plot(df$sex, df$charges, main="Charges vs Sex")
plot(df$bmi, df$charges, main="Charges vs Bmi") #Kolla på färg man kvinna
boxplot(df$charges ~ df$children, main="Charges vs Children")
plot(df$smoker, df$charges, main="Charges vs Smoker")
plot(df$region, df$charges, main="Charges vs Region")
par(mfrow = c(1, 1))

bc <- boxcox(charges ~ age + sex + bmi + children + smoker + region, data = df)
id_max <- which(bc$y==max(bc$y))
lambda <- bc$x[id_max] 

df$charges_log <- log(df$charges)
df$charges_sqrt <- sqrt(df$charges)
df$charges_box <- (df$charges^lambda - 1)/lambda

par(mfrow=c(2,3))
plot(df$age, df$charges_sqrt, main="Charges (sqrt) vs Age")
plot(df$sex, df$charges_sqrt, main="Charges (sqrt) vs Sex")
plot(df$bmi, df$charges_sqrt, main="Charges (sqrt) vs Bmi")
boxplot(df$charges_sqrt ~ df$children, main="Charges(sqrt)  vs Children")
plot(df$smoker, df$charges_sqrt, main="Charges (sqrt) vs Smoker")
plot(df$region, df$charges_sqrt, main="Charges (sqrt) vs Region")

plot(df$age, df$charges_log, main="Charges log vs Age")
plot(df$sex, df$charges_log, main="Charges log vs Sex")
plot(df$bmi, df$charges_log, main="Charges log vs Bmi")
plot(df$charges_log ~ df$children, main="Charges vs Children")
plot(df$smoker, df$charges_log, main="Charges log vs Smoker")
plot(df$region, df$charges_log, main="Charges log vs Region")

plot(df$age, df$charges_box, main="Charges (box) vs Age")
plot(df$sex, df$charges_box, main="Charges (box) vs Sex")
plot(df$bmi, df$charges_box, main="Charges(box) vs Bmi")
boxplot(df$charges_log ~ df$children, main="Charges (box) vs Children")
plot(df$smoker, df$charges_box, main="Charges (box) vs Smoker")
plot(df$region, df$charges_box, main="Charges (box) vs Region")

par(mfrow = c(1, 1))

#plot(df$age, df$children, main="Charges (sqrt) vs Age")

#Plotting the distribution of charges
ggplot(df, aes(x = charges_box)) +
  geom_histogram()

ggplot(df, aes(x = charges_log)) +
  geom_histogram()

ggplot(df, aes(x = charges_sqrt)) +
  geom_histogram()

#Box-cox and log look like the best transformations
# WARN: It looks like there are groups of charges for covariate ages

#:::::: Preparation to divide data into test and training set ::::::::::::::::: ----
set.seed(123)
K <- 10
n <- length(train_indices)
ii <- sample(seq(1,n), n)  # vector of length n
foldsize <- floor(n/K)  
sizefold <- rep(foldsize,K) 

#:::::: Finding model using 10-fold Square root transformation ::::::::::::::::: ----
formula_full_sqrt = "charges_sqrt ~ age + sex + bmi + children + smoker + region"

mlr_sqrt <-lm(formula_full_sqrt, data = df,  x=TRUE)

X_sqrt <- mlr_sqrt$x # the design matrix
X_sqrt <- X_sqrt[,-c(1)] # remove the intercept

cor_matrix <- cor(X_sqrt) 
corrplot(cor_matrix) 

y_sqrt <- df$charges_sqrt

m_sqrt<-regsubsets(X_sqrt,y_sqrt,force.in=c(2,5,6,7,8), int=T,nbest=1000,nvmax=dim(X_sqrt)[2],method = c("ex"),really.big = T) 

cleaps<-summary(m_sqrt,matrix=T)

models_sqrt <- cleaps$which
models_sqrt

PE_sqrt <- matrix(0,dim(models_sqrt)[1],K)
iused <- 0  

for (k in (1:K)) {
  itest <- ii[(iused + 1):(iused + sizefold[k])]
  itrain <- ii[-c((iused + 1):(iused + sizefold[k]))]
  iused <- iused + length(itest) 
  PE_sqrt[1,k] <- sum((y_sqrt[itest] - mean(y_sqrt[itrain]))^2) # intercept only 
  for (mm in (1:dim(models_sqrt)[1])) {  # loops through all model subsets 
    xtrain <- X_sqrt[itrain,models_sqrt[mm,2:dim(models_sqrt)[2]]]  # extract training design matrix for model mm 
    xtrain <- cbind(rep(1, dim(xtrain)[1]), xtrain)  # add in the intercept column of ones since we now need to estimate also the intercept via least squares
    betahat <- solve(t(xtrain)%*%xtrain)%*%t(xtrain)%*%y_sqrt[itrain]  # least squares formula for parameter estimates
    xtest <- X_sqrt[itest,models_sqrt[mm,2:dim(models_sqrt)[2]]]  # extract test design matrix for model mm 
    xtest <- cbind(rep(1, dim(xtest)[1]), xtest)  # add in the intercept column of ones since we now need to make predictions
    ypred<-xtest%*%betahat
    PE_sqrt[mm,k]<-sum((y_sqrt[itest]-ypred)^2) #Calculate prediction errors
  }
}

PE_sqrt

pMSE_sqrt <- apply(PE_sqrt,1,sum)/n  # final prediction errors.

complexity <- c(rep(6,3),rep(7,3),8) #FORCING BOTH

length(complexity)
length(pMSE_sqrt)

pMSE_sqrt

plot(complexity,pMSE_sqrt, main = "Predicion error (pMSE) and model complexity", ylab = "prediction error (pMSE)")

#Full model has lowest prediction error looking at residuals
summary(mlr_sqrt) #Gender not signigicant!

par(mfrow=c(2,2))
plot(mlr_sqrt)
par(mfrow = c(1, 1))

#Residuals look really poor and very high prediction errors for test set -> moving on to next transformation

#:::::: Finding model using 10-fold log transformation ::::::::::::::::: ----
formula_full_log = "charges_log ~ age + sex + bmi + children + smoker + region"

mlr_log <-lm(formula_full_log, data = df,  x=TRUE)

X_log <- mlr_log$x # the design matrix
X_log <- X_log[,-c(1)] # remove the intercept

cor_matrix <- cor(X_log) 
corrplot(cor_matrix) 

y_log <- df$charges_log

m_log<-regsubsets(X_log,y_log,force.in=c(2,5,6,7,8), int=T,nbest=1000,nvmax=dim(X_log)[2],method = c("ex"),really.big = T) 

cleaps<-summary(m_log,matrix=T)

models_log <- cleaps$which
models_log

PE_log <- matrix(0,dim(models_log)[1],K)
iused <- 0  

for (k in (1:K)) {
  itest <- ii[(iused + 1):(iused + sizefold[k])]
  itrain <- ii[-c((iused + 1):(iused + sizefold[k]))]
  iused <- iused + length(itest) 
  PE_log[1,k] <- sum((y_log[itest] - mean(y_log[itrain]))^2) # intercept only 
  for (mm in (1:dim(models_log)[1])) {  # loops through all model subsets 
    xtrain <- X_log[itrain,models_log[mm,2:dim(models_log)[2]]]  # extract training design matrix for model mm 
    xtrain <- cbind(rep(1, dim(xtrain)[1]), xtrain)  # add in the intercept column of ones since we now need to estimate also the intercept via least squares
    betahat <- solve(t(xtrain)%*%xtrain)%*%t(xtrain)%*%y_log[itrain]  # least squares formula for parameter estimates
    xtest <- X_log[itest,models_log[mm,2:dim(models_log)[2]]]  # extract test design matrix for model mm 
    xtest <- cbind(rep(1, dim(xtest)[1]), xtest)  # add in the intercept column of ones since we now need to make predictions
    ypred<-xtest%*%betahat
    PE_log[mm,k]<-sum((y_log[itest]-ypred)^2) #Calculate prediction errors
  }
}

PE_log

pMSE_log <- apply(PE_log,1,sum)/n  # final prediction errors.

complexity <- c(rep(6,3),rep(7,3),8) #FORCING BOTH

length(complexity)
length(pMSE_log)

pMSE_log

plot(complexity,pMSE_log, main = "Predicion error (pMSE) and model complexity", ylab = "prediction error (pMSE)")

#Full model has lowest prediction error looking at residuals
summary(mlr_log) #Gender not as signigicant

par(mfrow=c(2,2))
plot(mlr_log)
par(mfrow = c(1, 1))

#Residuals look really poor 

#:::::: Finding model using 10-fold BOX_COX transformation ::::::::::::::::: ----
formula_full_box = "charges_box ~ age + sex + bmi + children + smoker + region"

mlr_box <-lm(formula_full_box, data = df,  x=TRUE)

X_box <- mlr_box$x # the design matrix
X_box <- X_box[,-c(1)] # remove the intercept

cor_matrix <- cor(X_box) 
corrplot(cor_matrix) 

y_box <- df$charges_box

m_box<-regsubsets(X_box,y_box,force.in=c(2,5,6,7,8), int=T,nbest=1000,nvmax=dim(X_box)[2],method = c("ex"),really.big = T) 

cleaps<-summary(m_box,matrix=T)

models_box <- cleaps$which
models_box

PE_box <- matrix(0,dim(models_box)[1],K)
iused <- 0  

for (k in (1:K)) {
  itest <- ii[(iused + 1):(iused + sizefold[k])]
  itrain <- ii[-c((iused + 1):(iused + sizefold[k]))]
  iused <- iused + length(itest) 
  PE_box[1,k] <- sum((y_box[itest] - mean(y_box[itrain]))^2) # intercept only 
  for (mm in (1:dim(models_box)[1])) {  # loops through all model subsets 
    xtrain <- X_box[itrain,models_box[mm,2:dim(models_box)[2]]]  # extract training design matrix for model mm 
    xtrain <- cbind(rep(1, dim(xtrain)[1]), xtrain)  # add in the intercept column of ones since we now need to estimate also the intercept via least squares
    betahat <- solve(t(xtrain)%*%xtrain)%*%t(xtrain)%*%y_box[itrain]  # least squares formula for parameter estimates
    xtest <- X_box[itest,models_box[mm,2:dim(models_box)[2]]]  # extract test design matrix for model mm 
    xtest <- cbind(rep(1, dim(xtest)[1]), xtest)  # add in the intercept column of ones since we now need to make predictions
    ypred<-xtest%*%betahat
    PE_box[mm,k]<-sum((y_box[itest]-ypred)^2) #Calculate prediction errors
  }
}

PE_box

pMSE_box <- apply(PE_box,1,sum)/n  # final prediction errors.

complexity <- c(rep(6,3),rep(7,3),8) #FORCING BOTH

length(complexity)
length(pMSE_box)

pMSE_box

plot(complexity,pMSE_box, main = "Predicion error (pMSE) and model complexity", ylab = "prediction error (pMSE)")

#Full model has lowest prediction error looking at residuals
summary(mlr_box) #Gender not as signigicant

par(mfrow=c(2,2))
plot(mlr_box)
par(mfrow = c(1, 1))

#Residuals look really poor 



#:::::: Going back and looking at data ::::::::::::::::: ----

par(mfrow=c(2,3))
plot(df$age, df$charges_sqrt, main="Charges (sqrt) vs Age", col = factor(df$smoker))
plot(df$sex, df$charges_sqrt, main="Charges (sqrt) vs Sex")
plot(df$bmi, df$charges_sqrt, main="Charges (sqrt) vs Bmi", col = factor(df$smoker))
boxplot(df$charges_sqrt ~ df$children, main="Charges(sqrt)  vs Children")
plot(df$smoker, df$charges_sqrt, main="Charges (sqrt) vs Smoker")
plot(df$region, df$charges_sqrt, main="Charges (sqrt) vs Region")

plot(df$age, df$charges_log, main="Charges log vs Age", col = factor(df$smoker))
plot(df$sex, df$charges_log, main="Charges log vs Sex")
plot(df$bmi, df$charges_log, main="Charges log vs Bmi", col = factor(df$smoker))
plot(df$charges_log ~ df$children, main="Charges vs Children", col = factor(df$smoker))
plot(df$smoker, df$charges_log, main="Charges log vs Smoker")
plot(df$region, df$charges_log, main="Charges log vs Region")

plot(df$age, df$charges_box, main="Charges (box) vs Age", col = factor(df$smoker))
plot(df$sex, df$charges_box, main="Charges (box) vs Sex")
plot(df$bmi, df$charges_box, main="Charges(box) vs Bmi", col = factor(df$smoker))
plot(df$charges_log ~ df$children, main="Charges (box) vs Children", col = factor(df$smoker))
plot(df$smoker, df$charges_box, main="Charges (box) vs Smoker")
plot(df$region, df$charges_box, main="Charges (box) vs Region")

par(mfrow = c(1, 1))


#:::::: Testing if there is an interaction between age and smoker for log charges using full model :::::::::::::::: ----

formula_full_log = "charges_log ~ age + sex + bmi + children + smoker + region"

mlr_log <-lm(formula_full_log, data = df,  x=TRUE)

par(mfrow=c(2,2))
plot(mlr_log)
par(mfrow = c(1, 1))

summary(mlr_log)

p1 <- ggplot(df, aes(x = age, y = charges_log, color = factor(smoker), shape = factor(smoker))) +
  geom_point()

p1 <- p1 + geom_abline(intercept = mlr_log$coef[1], slope = mlr_log$coef[2], linetype = "solid", color = "#F8766D")

p1 <- p1 +  geom_abline(intercept = mlr_log$coef[1] + mlr_log$coef[6], slope = mlr_log$coef[2], linetype = "solid", color = "#00BFC4")

p1


#I think that there is an interaction here!!
smoker_true_data <- df[df$smoker == 'yes', ]
smoker_false_data <- df[df$smoker == 'no', ]

##Looking at residuals:
y_nonsmoker_pred <- mlr_log$coef[1] + mlr_log$coef[2] * smoker_false_data$age
y_smoker_pred <- mlr_log$coef[1] +mlr_log$coef[6]+mlr_log$coef[2] * smoker_true_data$age

residual_smoker <- smoker_true_data$charges_log - y_smoker_pred
residual_non_smoker <- smoker_false_data$charges_log - y_nonsmoker_pred

plot(smoker_false_data$age, residual_non_smoker, col = '#F8766D', pch = 16)
points(smoker_true_data$age, residual_smoker, col = '#00BFC4', pch = 17, main = 'Residuals vs. Charges Log', xlab = 'Charges Log', ylab = 'Residuals')
abline(h = 0, col = 'black')
legend('topright', legend = c('Smoker', 'Non-Smoker'), col = c('#00BFC4', '#F8766D'), pch = c(16, 17))

#Different slopes needed depending on smoker and age!
#Creating interaction:
# Dummy encoding smoker
df$smoker_yes <- ifelse(df$smoker == 'yes', 1, 0) #Känns olämpligt att använda 0?? vill man använda 1 och 2

df$int_smoker <- df$smoker_yes * df$age

mlr_log_int = "charges_log ~ age + sex + bmi + children + smoker + region + int_smoker"

mlr_log_int <-lm(mlr_log_int,data = df,  x=TRUE)

X <- mlr_log_int$x # the design matrix
X <- X[,-c(1)] # remove the intercept
y <- df$charges_log

cor_matrix <- cor(X) 
corrplot(cor_matrix) 
#Now they are highly correlated, age and int_agesmoker which we need to fix somehow!

par(mfrow=c(2,2))
plot(mlr_log_int)
par(mfrow = c(1, 1))

#Residuals look better

summary(mlr_log_int)


p2 <- ggplot(df, aes(x = age, y = charges_log, color = factor(smoker), shape = factor(smoker))) +
  geom_point()

p2 <- p2 + geom_abline(intercept = mlr_log$coef[1], slope = mlr_log$coef[2], linetype = "solid", color = "#F8766D")

p2 <- p2 +  geom_abline(intercept = mlr_log$coef[1] + mlr_log_int$coef[6], slope = mlr_log$coef[2] + mlr_log_int$coef[10], linetype = "solid", color = "#00BFC4")

p2


plot(df$age,df$charges_log,pch = as.numeric(df$smoker),col=df$smoker)
abline(h=0,lty=2)
legend(91,20,c("Smoker","Non-Smoker"),pch=c(1,2),col=c(1,2))
lines(c(20,60),mlr_log_int$coef[1]+mlr_log_int$coef[2]*c(20,60), col = 1)
lines(c(20,60),mlr_log_int$coef[1]+mlr_log_int$coef[6]+mlr_log_int$coef[2]*c(20,60)+mlr_log_int$coef[10]*c(20,60), col = 2)

##Looking at residuals:
y_nonsmoker_pred <- mlr_log_int$coef[1]+mlr_log_int$coef[2]*smoker_false_data$age
y_smoker_pred <- mlr_log_int$coef[1]+mlr_log_int$coef[6]+mlr_log_int$coef[2]*smoker_true_data$age+mlr_log_int$coef[10]*smoker_true_data$age

residual_smoker <- smoker_true_data$charges_log - y_smoker_pred
residual_non_smoker <- smoker_false_data$charges_log - y_nonsmoker_pred

plot(smoker_false_data$age, residual_non_smoker, col = '#F8766D', pch = 16)
points(smoker_true_data$age, residual_smoker, col = '#00BFC4', pch = 17, main = 'Residuals vs. Charges Log', xlab = 'Charges Log', ylab = 'Residuals')
abline(h = 0, col = 'black')
legend('topright', legend = c('Smoker', 'Non-Smoker'), col = c('#00BFC4', '#F8766D'), pch = c(16, 17))


#### :::::: Adding interaction to full log model and 10-fold cross validation :::::: ####

formula_full_log = "charges_log ~ age + sex + bmi + children + smoker + region + smoker:age"

mlr_log <-lm(formula_full_log, data = df,  x=TRUE)

X_log <- mlr_log$x # the design matrix
X_log <- X_log[,-c(1)] # remove the intercept

cor_matrix <- cor(X_log) 
corrplot(cor_matrix) 
#Correlation not unexpected but not ideal

y_log <- df$charges_log

#All cathegorial are forced
m_log<-regsubsets(X_log,y_log,force.in=c(2,5,6,7,8), int=T,nbest=1000,nvmax=dim(X_log)[2],method = c("ex"),really.big = T) 

#All cathegorial but sex are forced
m_log<-regsubsets(X_log,y_log,force.in=c(5,6,7,8), int=T,nbest=1000,nvmax=dim(X_log)[2],method = c("ex"),really.big = T) 

#All cathegorial but region are forced
m_log<-regsubsets(X_log,y_log,force.in=c(2,5), int=T,nbest=1000,nvmax=dim(X_log)[2],method = c("ex"),really.big = T) 

cleaps<-summary(m_log,matrix=T)

models_log <- cleaps$which
models_log


PE_log <- matrix(0,dim(models_log)[1],K)
iused <- 0  

for (k in (1:K)) {
  itest <- ii[(iused + 1):(iused + sizefold[k])]
  itrain <- ii[-c((iused + 1):(iused + sizefold[k]))]
  iused <- iused + length(itest) 
  PE_log[1,k] <- sum((y_log[itest] - mean(y_log[itrain]))^2) # intercept only 
  for (mm in (1:dim(models_log)[1])) {  # loops through all model subsets 
    xtrain <- X_log[itrain,models_log[mm,2:dim(models_log)[2]]]  # extract training design matrix for model mm 
    xtrain <- cbind(rep(1, dim(xtrain)[1]), xtrain)  # add in the intercept column of ones since we now need to estimate also the intercept via least squares
    betahat <- solve(t(xtrain)%*%xtrain)%*%t(xtrain)%*%y_log[itrain]  # least squares formula for parameter estimates
    xtest <- X_log[itest,models_log[mm,2:dim(models_log)[2]]]  # extract test design matrix for model mm 
    xtest <- cbind(rep(1, dim(xtest)[1]), xtest)  # add in the intercept column of ones since we now need to make predictions
    ypred<-xtest%*%betahat
    PE_log[mm,k]<-sum((y_log[itest]-ypred)^2) #Calculate prediction errors
  }
}

PE_log

pMSE_log <- apply(PE_log,1,sum)/n  # final prediction errors.

#All forced
complexity <- c(rep(6,4),rep(7,6),rep(8,4),9) #FORCING BOTH

#All but sex forced
complexity <- c(rep(5,5),rep(6,10),rep(7,10),rep(8,5),10) #FORCING BOTH


#All but region forced
complexity <- c(rep(3,7),rep(4,21),rep(5,35),rep(6,35),rep(7,21),rep(8,7),9)

length(complexity)
length(pMSE_log)


##To get the right lowest index
index <- which(complexity == 9)
index
indx_lowest <- index[which.min(pMSE_log[index])]
indx_lowest
pMSE_log[index_y]

#line for full model
index_x <- c(6,7,8,9)
index_y <- c(4,9,14,15)

#line for a model with sex not forced
index_x <- c(5,6,7,8,10)
index_y <- c(1,6,16,26,31)

#line for a model with region not forced
index_x <- c(3,4,5,6,7,8,9)
index_y <- c(2,15,45,64,99,120,127)
pMSE_log[index_y]

plot(complexity, pMSE_log, main = "Prediction Error (pMSE) and Model Complexity", 
     ylab = "Prediction Error (pMSE)", pch = 16, cex = 1)
lines(index_x, pMSE_log[index_y], col = "red", lwd = 2)
#Full model has best results 

#Looking at full model
summary(mlr_log) #All covariates significant for full model, gender least significant! 
summary()

top_model_indices <- order(pMSE_log)[1:8]

# Extract the best model from the models_log matrix
best_model_variables <- models_log[top_model_indices, 2:dim(models_log)[2]]

best_model_variables

#What now do we fit these on entire data and look at residuals 
# + significance to see if maybe variable should be forced out?

#Look at children bmi? 


###
m_log<-regsubsets(X_log,y_log,force.in=c(2,5,6,7,8), int=T,nbest=1000,nvmax=dim(X_log)[2],method = c("ex"),really.big = T) 

cleaps<-summary(m_log,matrix=T)

models_log <- cleaps$which
models_log

PE_log <- matrix(0,dim(models_log)[1],K)
iused <- 0  

for (k in (1:K)) {
  itest <- ii[(iused + 1):(iused + sizefold[k])]
  itrain <- ii[-c((iused + 1):(iused + sizefold[k]))]
  iused <- iused + length(itest) 
  PE_log[1,k] <- sum((y_log[itest] - mean(y_log[itrain]))^2) # intercept only 
  for (mm in (1:dim(models_log)[1])) {  # loops through all model subsets 
    xtrain <- X_log[itrain,models_log[mm,2:dim(models_log)[2]]]  # extract training design matrix for model mm 
    xtrain <- cbind(rep(1, dim(xtrain)[1]), xtrain)  # add in the intercept column of ones since we now need to estimate also the intercept via least squares
    betahat <- solve(t(xtrain)%*%xtrain)%*%t(xtrain)%*%y_log[itrain]  # least squares formula for parameter estimates
    xtest <- X_log[itest,models_log[mm,2:dim(models_log)[2]]]  # extract test design matrix for model mm 
    xtest <- cbind(rep(1, dim(xtest)[1]), xtest)  # add in the intercept column of ones since we now need to make predictions
    ypred<-xtest%*%betahat
    PE_log[mm,k]<-sum((y_log[itest]-ypred)^2) #Calculate prediction errors
  }
}

PE_log

pMSE_log <- apply(PE_log,1,sum)/n  # final prediction errors.

complexity <- c(rep(6,4),rep(7,6),rep(8,4),9) #FORCING BOTH

length(complexity)
length(pMSE_log)

pMSE_log


########Partial F-test of the best ones ###########
formula_index6 = "charges_log ~ smoker + region + age + smoker:age"

formula_index6_sqrt = "charges_log ~ smoker + region + age + I(age^3) + smoker:age"
formula_index16 = "charges_log ~ smoker + region + age + children + smoker:age"
formula_index26 = "charges_log ~ smoker + region + age + bmi + children + smoker:age"

model_6 <- lm(formula_index6, data = df, x=TRUE)
model_6_sqrt <- lm(formula_index6_sqrt, data = df, x=TRUE)
model_16 <- lm(formula_index16, data = df, x=TRUE)
model_26 <- lm(formula_index26, data = df, x=TRUE)


Partial_model_6vs16 <- anova(model_6,model_16)
Partial_model_6vs26 <- anova(model_6,model_26)
Partial_model_16vs26 <- anova(model_16,model_26)

Partial_model_16vs26

summary(model_6)

########Outliers#########
pred_log_6 <- predict(model_26)
residual_6 <- df$charges_log - pred_log_6
pMSE_6 <- mean((df$charges_log - pred_log_6)^2)
index_lowest_PE <- order(pMSE_6)[1:5]
plot(complexity, pMSE_log, main = "Prediction Error (pMSE) and Model Complexity", 
     ylab = "Prediction Error (pMSE)", pch = 16, cex = 1)
lines(complexity[index_lowest_PE], pMSE_log[index_lowest_PE], col = "red", lwd = 2)

leverage <- hatvalues(model_6)
leverage_order <- order(leverage, decreasing = TRUE)
leverage_max_index <- leverage_order[1:5]
max_leverage <-leverage[leverage_max_index ]
max_leverage

lev_res <- leverage*residual_6
lev_res_order <- order(abs(lev_res), decreasing = TRUE)
lev_res_max_index <- lev_res_order[1:5]
lev_res_max_index

age_20_index <- which(df$age < 20)

plot(pred_log_6, residual_6, xlab = 'Predicted values', ylab = 'Residuals')
points(pred_log_6[leverage_max_index], residual_6[leverage_max_index],col = "red", pch = 16)
points(pred_log_6[lev_res_max_index], residual_6[lev_res_max_index],col = "green", pch = 16)
points(pred_log_6[age_20_index], residual_6[age_20_index],col = "orange", pch = 16)

title('Residuals vs Fitted')
abline(h = 0, col = "blue", lty = 2, pch = 16)

######Averages
average_age <- sum(df$age)/length(df$age)
average_sex <- names(table(df$sex))[which.max(table(df$sex))]
average_BMI <- sum(df$bmi)/length(df$bmi)
average_children <- sum(df$children)/length(df$children)
average_smoker <- names(table(df$smoker))[which.max(table(df$smoker))]
average_Region <- names(table(df$region))[which.max(table(df$region))]
average_charges_log <- sum(df$charges_log)/length(df$charges_log)

Averages <- c(average_charges_log, average_age,average_sex,average_BMI,average_children,average_smoker,average_Region)
Averages

#### :::::: Adding interaction to sqrt model and 10-fold cross validation :::::: ####

formula_full_sqrt = "charges_sqrt ~ age + sex + bmi + children + smoker + region + int_smoker"

mlr_sqrt <-lm(formula_full_sqrt, data = df,  x=TRUE)

X_sqrt <- mlr_sqrt$x # the design matrix
X_sqrt <- X_sqrt[,-c(1)] # remove the intercept

cor_matrix <- cor(X_sqrt) 
corrplot(cor_matrix) 

y_sqrt <- df$charges_sqrt

m_sqrt<-regsubsets(X_sqrt,y_sqrt,force.in=c(2,5,6,7,8), int=T,nbest=1000,nvmax=dim(X_sqrt)[2],method = c("ex"),really.big = T) 

cleaps<-summary(m_sqrt,matrix=T)

models_sqrt <- cleaps$which
models_sqrt

PE_sqrt <- matrix(0,dim(models_sqrt)[1],K)
iused <- 0  

for (k in (1:K)) {
  itest <- ii[(iused + 1):(iused + sizefold[k])]
  itrain <- ii[-c((iused + 1):(iused + sizefold[k]))]
  iused <- iused + length(itest) 
  PE_sqrt[1,k] <- sum((y_sqrt[itest] - mean(y_sqrt[itrain]))^2) # intercept only 
  for (mm in (1:dim(models_sqrt)[1])) {  # loops through all model subsets 
    xtrain <- X_sqrt[itrain,models_sqrt[mm,2:dim(models_sqrt)[2]]]  # extract training design matrix for model mm 
    xtrain <- cbind(rep(1, dim(xtrain)[1]), xtrain)  # add in the intercept column of ones since we now need to estimate also the intercept via least squares
    betahat <- solve(t(xtrain)%*%xtrain)%*%t(xtrain)%*%y_sqrt[itrain]  # least squares formula for parameter estimates
    xtest <- X_sqrt[itest,models_sqrt[mm,2:dim(models_sqrt)[2]]]  # extract test design matrix for model mm 
    xtest <- cbind(rep(1, dim(xtest)[1]), xtest)  # add in the intercept column of ones since we now need to make predictions
    ypred<-xtest%*%betahat
    PE_sqrt[mm,k]<-sum((y_sqrt[itest]-ypred)^2) #Calculate prediction errors
  }
}

PE_sqrt

pMSE_sqrt <- apply(PE_sqrt,1,sum)/n  # final prediction errors.

complexity <- c(rep(6,4),rep(7,6),rep(8,4),9) #FORCING BOTH

length(complexity)
length(pMSE_sqrt)

pMSE_sqrt

plot(complexity,pMSE_sqrt, main = "Predicion error (pMSE) and model complexity", ylab = "prediction error (pMSE)")

#Full model has lowest prediction error looking at residuals
summary(mlr_sqrt) #Gender is still not signigicant! Should try and remove it

par(mfrow=c(2,2))
plot(mlr_sqrt)
par(mfrow = c(1, 1))

top_model_indices <- order(pMSE_sqrt)[1:8]

# Extract the best model from the models_log matrix
best_model_variables <- models_sqrt[top_model_indices, 2:dim(models_sqrt)[2]]

best_model_variables

