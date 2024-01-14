library(ggplot2)
library(foreign)
library(dplyr)
library(leaps)
library(caret)
library(olsrr)
library(MASS)
library(AER)
library(plotly)
library(car)

########## READ, TRANSFORM AND SELCT DATA ###################
data<-read.table("countydemographics.txt")
data <-as.data.frame(data)

data <- na.omit(data)
# add variable names
colnames(data, do.NULL = FALSE)
colnames(data)<-c("id","county","state","area","popul","pop1834","pop65plus","phys","beds",
                  "crimes","higrads","bachelors","poors","unemployed","percapitaincome","totalincome","region")

data$crime_rate <- data$crimes/data$popul
data$county_num <- as.numeric(as.factor(data$county))
data$state_num <- as.numeric(as.factor(data$state))
data$state <- factor(data$state)
data$county<-factor(data$county)
data$region<-factor(data$region)


#if wanting to try logaritmic
#data$crime_rate <- -log(data$crime_rate)


######################Another version of step intervall##########################################
min <- min(data$crime_rate)
max <- max(data$crime_rate)

i<-100
step <- (max-min)/i

Intervals <- list()

# Create intervals
for (s in 1:i) {
  interval_start <- min + (s - 1) * step
  interval_end <- min + s * step
  Intervals[[s]] <- c(interval_start, interval_end)
}

print(Intervals[[1]])
data$num_ratio <- 0

for (j in 1:i) {
  data$num_ratio <- ifelse(data$crime_rate >= Intervals[[j]][1] & data$crime_rate <= Intervals[[j]][2], j, data$num_ratio)
}
##################################################
# Transforming to int
data$num_ratio<- round(data$crime_rate, digits = 3)*1000

#remove outliers
data <- data[data$id != 173, ]
data <- data[data$id != 229, ]
data <- data[data$id != 36,]
data <- data[data$id !=34, ]
data <- data[data$id !=6, ]
data <- data[data$id !=123, ]
data <- data[data$id !=245,]
data <- data[data$id !=132,] #

#histogram over the data. Remove ID 6 forst to get a good plot
hist(data$num_ratio, main = "Histogram of Crime Rates", xlab = "Crime Rates")

#Explore the data a bit
plot_box_rate_county <- ggplot(data, aes(x = as.factor(num_ratio), y = county_num)) +
  geom_boxplot(fill = "blue", color = "black") 
plot_box_rate_county

plot_ratio_county <- ggplot(data, aes(x = county_num, y = crime_rate)) +  geom_point()
plot_ratio_county

plot_ratio_income <- ggplot(data, aes(x = percapitaincome, y = crime_rate)) +  geom_point()
plot_ratio_income

plot_ratio_state <- ggplot(data, aes(x = state_num, y = crime_rate)) +  geom_point()
plot_ratio_state

plot_ratio_popu65 <- ggplot(data, aes(x = pop65plus, y = crime_rate)) +  geom_point()
plot_ratio_popu65

plot_ratio_popu1834 <- ggplot(data, aes(x = pop1834, y = crime_rate)) +  geom_point()
plot_ratio_popu1834

############CREAT MODEL########################
y <- "num_ratio"

#x <- " county_num + state_num + popul + pop1834 + pop65plus  + higrads + bachelors + poors + unemployed + percapitaincome"
#dviances
#m1 - 40594.18
#m2 - 422.2649
#m1_bw -40700.07
#m2_bw -40700.07
#county_num+state_num+pop1834+pop65plus+crimes+higrads+bachelors+poors+unemploye+percapitaincome","totalincome","region"

#x <- " county_num + state_num + popul + pop1834 + pop65plus  + higrads + bachelors + unemployed + percapitaincome "
#dviances
#m1 - 47690.4
#m2 - 426.4053
#m1_bw - 48232.99
#m2_bw - 48232.99


#x <- "pop1834 + pop65plus + bachelors + unemployed + percapitaincome "
#dviances
#m1 - 56966.64
#m2 - 429.893
#m1_bw - 57530.73
#m2_bw - 57530.73

#x <- " county_num + state_num + pop1834 + pop65plus  + higrads + bachelors + unemployed"
#dviances
#m1 - 52582.24
#m2 - 428.0984
#m1_bw - 52966.17
#m2_bw - 52966.17

x <- " county_num + state_num + pop65plus + unemployed"
#dviances
#m1 - 58114.5
#m2 - 429.4068
#m1_bw -
#m2_bw -


formula <- as.formula(paste(y, "~", x))
#############CREATE MODELS###########################################


m1 <- glm(formula, data = data)
sum_m1<-summary(m1)
theta<- 0.5

#Checjing so no nan is produced
colSums(is.na(data))
any(is.na(data))
data <- na.omit(data)

m2 <- glm.nb(formula, data = data,control = glm.control(maxit = 1000000)) 

sum_m2<- summary(m2)
sum_m2

#Calculate leverage values
leverage_values_m1 <- hatvalues(m1)
leverage_values_m2 <- hatvalues(m2)
leverage_matrix <- cbind(leverage_values_m1, leverage_values_m2)

#extraxt id witg biggest leverage
max_lev_ind <- which(leverage_matrix == max(leverage_matrix), arr.ind = TRUE)
max_lev <- data$id[max_lev_ind]
max_lev
######################PRED VS REAL####################### 
m1_pred <- predict(m1, type = "response")
m2_pred <- predict(m2, type = "response")
plot_pred <- data.frame(m1_pred = m1_pred)
plot_pred$m2_pred <-m2_pred
plot_pred$real_num <- data$num_ratio
plot_pred$real <- data$crime_rate


plot_m1_real <- ggplot(plot_pred,aes(x = real_num, y = m1_pred)) + geom_point()+ ggtitle('Poisson vs real')
plot_m1_real
deviances <- data.frame(m1 = deviance(m1)) #lower deviance -> better fit to data
plot_pred$m1_res <- resid(m1)
plot_m1_res <- ggplot(plot_pred,aes(x = m1_pred, y = m1_res)) + geom_point()+ ggtitle('Poisson, residual plot')
plot_m1_res

plot_m2_real <- ggplot(plot_pred,aes(x = real_num, y = m2_pred)) + geom_point()+ ggtitle('Negative binomial vs real')
plot_m2_real
plot_m2_ly <- plot_ly(plot_pred,x = ~real_num, y = ~m2_pred, type = 'scatter',mode = 'markers')
plot_m2_ly <- plot_m2_ly %>% highlight()
plot_m2_ly

deviances$m2 <- deviance(m2)
plot_pred$m2_res <- resid(m2)
plot_pred$res_lv <- plot_pred$m2_res*leverage_values_m2
plot_m2_res <- ggplot(plot_pred,aes(x = real_num, y = m2_res)) + geom_point()+ ggtitle('Negative binomial, residual plot')
plot_m2_res
sum_m2
sum_m1
deviances$disp_m1 <-sum_m1$dispersion
deviances$disp_m2 <-sum_m2$theta

plot_out <-  plot_ly(plot_pred, x = ~m2_pred, y = ~m2_res, type = 'scatter',mode = 'markers', title='Negative binomial, residual plot')
plot_out <- plot_out %>% highlight()

plot_out

max_lev_res_ind <- which.max(plot_pred$res_lv)
max_lev_res <- data$id[max_lev_res_ind]
max_lev_res
leverage_values_m2[max_lev_res_ind]
plot_pred$res_lv[max_lev_res_ind]
sum_m2$theta

####################BACKWARD SELECTION##################
backward_m1 <- step(m1, direction = "backward")
backward_m1
deviances$m1_bq<-deviance(backward_m1)

backward_m2 <- step(m1, direction = "backward")
backward_m2
deviances$m2_bw<-deviance(backward_m2)


predicted_values_backward_m1 <- predict(backward_m1, type = "response")
predicted_values_backward_m2 <- predict(backward_m2, type = "response")
plot_pred_backwards <- data.frame(m1_pred = predicted_values_backward_m1)
plot_pred_backwards$m2_pred <- predicted_values_backward_m1
plot_pred_backwards$real_num <- data$num_ratio
plot_pred_backwards$real <- data$crime_rate

plot_out_backwards_m1 <- plot_ly(plot_pred_backwards, x = ~real, y = ~m1_pred, type = 'scatter',mode = 'markers')
plot_out_backwards_m1 <- plot_out %>% highlight()
plot_out_backwards_m1

plot_m1_real_bw <- ggplot(plot_pred_backwards,aes(x = real_num, y = m1_pred)) + geom_point() + ggtitle('backward,m1')
plot_m1_real_bw
plot_pred_backwards$m1_res <- resid(backward_m1)
plot_m1_res_bw <- ggplot(plot_pred_backwards,aes(x = m1_pred, y = m1_res)) + geom_point()+ ggtitle('backward residuals, m1')
plot_m1_res_bw

plot_out_backwards_m2 <- plot_ly(plot_pred_backwards, x = ~real, y = ~m1_pred, type = 'scatter',mode = 'markers')
plot_out_backwards_m2 <- plot_out %>% highlight()
plot_out_backwards_m2
plot_pred_backwards$m2_res <- resid(backward_m2)
plot_m2_res_bw <- ggplot(plot_pred_backwards,aes(x = m2_pred, y = m2_res)) + geom_point()+ ggtitle('backward residuals, m2')
plot_m2_res_bw

plot_m2_real_bw <- ggplot(plot_pred_backwards,aes(x = real_num, y = m2_pred)) + geom_point()+ ggtitle('backward, m2')
plot_m2_real_bw




