library(ggplot2)
library(dplyr)
library(arm)

df<-read.csv("earnings.csv")

df_other <- df %>%
  mutate(ethnicity = ifelse(ethnicity != 'White', 'Non-White', ethnicity)) 

df_other <- df_other %>%
  mutate(male = ifelse(male == 1, "Male", "Female"))

df_other <- df_other %>%
  mutate(category = paste(ethnicity, male, sep = " "))

df_other <- df_other[df_other$earn != 0, ] 

df_other$category <- factor(df_other$category)


#Reference White Male used 
df_other$category <- relevel(df_other$category,ref="White Male")

plot <- ggplot(df_other, aes(x = category, y = earn, fill = category)) +
  geom_boxplot() +
  ylab("Earning (log-transformed)") +
  scale_y_continuous(labels = scales::number_format()) +
  xlab("") +
  scale_fill_manual(values = c("White Male" = "lightblue", "White Female" = "lightgreen", "Non-White Male" = "blue", "Non-White Female" = "green"))

plot

df_other$earn_log <- log(df_other$earn)

plot_log<- ggplot(df_other, aes(x = category, y = earn_log, fill = category)) +
  geom_boxplot() +
  ylab("Earning (log-transformed)") +
  xlab("") +
  scale_fill_manual(values = c("White Male" = "lightblue", "White Female" = "lightgreen", "Non-White Male" = "blue", "Non-White Female" = "green"))

plot_log


#Categorical regression non-log
mod_nonlog <-lm(earn ~ category,data=df_other,x=TRUE) #category-1
summary(mod_nonlog)

X=mod_nonlog$x

summary(mod_nonlog)$cov.unscaled %*% t(X) %*% df_other$earn

labels_plot <- c("White Female","Non-White Male","Non-White Female")
par(mar = c(7, 7, 2, 2))
coefplot(mod_nonlog)#, varnames = labels_plot)


#Model log transformed:
mod_log<-lm(earn_log ~ category,data=df_other,x=TRUE) #category-1
summary(mod_log)

X=mod_log$x

summary(mod_log)$cov.unscaled %*% t(X) %*% df_other$earn_log

coefplot(mod_log)

#Changing reference to White female
df_other$category <- relevel(df_other$category,ref="White Female")

#Categorical regression non-log (reference white female)
mod_nonlog_2 <-lm(earn ~ category,data=df_other,x=TRUE) #category-1
summary(mod_nonlog_2)

X=mod_nonlog_2$x

summary(mod_nonlog_2)$cov.unscaled %*% t(X) %*% df_other$earn

labels_plot <- c("White Female","Non-White Male","Non-White Female")
par(mar = c(7, 7, 2, 2))
coefplot(mod_nonlog_2)#, varnames = labels_plot)


#Categorical regression log (reference white female)
mod_log_2 <-lm(earn_log ~ category,data=df_other,x=TRUE) #category-1
summary(mod_log_2)

X=mod_log_2$x

summary(mod_log_2)$cov.unscaled %*% t(X) %*% df_other$earn_log

coefplot(mod_log_2)


### Reginas kod:  ###
