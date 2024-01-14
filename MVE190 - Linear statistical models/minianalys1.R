library(ggplot2)
library(dplyr)

df<-read.csv("earnings.csv")

df_males_earn <- df[df$male != 0, c('male', 'earn', 'ethnicity')] 
df_males_earn <- df_males_earn[df_males_earn$earn != 400000, ] #Removing outlier
df_males_earn <- df_males_earn[order(df_males_earn$ethnicity), ]
cat("Number of males:", nrow(df_males_earn), "\n")
cat("Number of White males:", sum(df_males_earn$ethnicity == 'White'), "\n")

df_females_earn <- df[df$male != 1, c('male', 'earn', 'ethnicity')]
df_females_earn <- df_females_earn[order(df_females_earn$ethnicity), ]
cat("Number of females:", nrow(df_females_earn), "\n")
cat("Number of White Females:", sum(df_females_earn$ethnicity == 'White'), "\n")


ggplot(df_males_earn, aes(y = earn)) +
  geom_histogram(fill = "cyan", color = 'black')+
  scale_y_continuous(labels = scales::number_format(scale = 1e-3, accuracy = 1)) +
  ylim(0, 200000) + xlim(0,250) + 
  labs(title = "Yearly earnings of males in 1990") + 
  xlab("Number of males") +
  ylab("Yearly earnings (US dollar)") 


ggplot(df_females_earn, aes(y = earn)) +
  geom_histogram(fill = "green", color = 'black') +
  scale_y_continuous(labels = scales::number_format(scale = 1e-3, accuracy = 1)) +
  ylim(0, 200000) + xlim(0,250) + 
  labs(title = "Yearly earnings of females in 1990 ") + 
  xlab("Number of females") +
  ylab("Yearly earnings (US dollar)") 


df_ethnicity <- df %>%
  mutate(ethnicity = ifelse(ethnicity != 'White', 'Non-White', ethnicity)) 

df_ethnicity <- df_ethnicity  %>%
  mutate(male = ifelse(male == 1, "Male", "Female"))

df_ethnicity <- df_ethnicity %>%
  mutate(category = paste(ethnicity, male, sep = " "))

df_ethnicity <- df_ethnicity[, c('category','earn')]

#Remove people who do not earn any money
df_ethnicity <- df_ethnicity[df_ethnicity$earn != 0, ] 

df_ethnicity$earn_log <- log(df_ethnicity$earn)

#Plotting boxplots
plot <- ggplot(df_ethnicity, aes(x = category, y = earn, fill = category)) +
  geom_boxplot() +
  ylab("Yearly earnings") +
  scale_y_continuous(labels = scales::number_format()) +
  xlab("") +
  scale_fill_manual(values = c("White Male" = "lightblue", "White Female" = "lightgreen", "Non-White Male" = "dodgerblue3", "Non-White Female" = "forestgreen")) +
  ylim(0,210000) + 
  labs(title = "Yearly earnings for different societal groups")

plot


#Plotting boxplots
plot <- ggplot(df_ethnicity, aes(x = category, y = earn_log, fill = category)) +
  geom_boxplot() +
  ylab("Yearly earnings (log-transformed)") +
  scale_y_continuous(labels = scales::number_format()) +
  xlab("") +
  scale_fill_manual(values = c("White Male" = "lightblue", "White Female" = "lightgreen", "Non-White Male" = "dodgerblue3", "Non-White Female" = "forestgreen"))

plot

#Data frame for looking at education vs earnings
df_edu <- df[df$earn != 0, ] #Removing those that do not earn anything
df_edu$earn_log <- log(df_edu$earn)

#Selecting columns 
df_earnvsedu <- df_edu [, c('male','earn','education','earn_log')]

df_earnvsedu <- df_earnvsedu[complete.cases(df_earnvsedu), ]

p1 <- ggplot(df_earnvsedu, aes(x = education, y = earn)) +
  geom_point() +
  scale_x_continuous(labels = scales::number_format(scale = 1e0, accuracy = 1)) +
  xlab("Number of years of education") +
  ylab("Yearly earnings") + 
  labs(title = "Number of years of education and earnings")

p1

m_lr1 <- lm(earn ~ education, data = df_earnvsedu)

summary(m_lr1)

p1_residuals <- ggplot(df_earnvsedu, aes(x = education, y = residuals(m_lr1))) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  xlab("Number of years of education") +
  ylab("Residuals") +
  labs(title = "Residuals from Linear Regression Model")

p1_residuals

# Scatter plot of education vs. log-transformed earnings
p2 <- ggplot(df_earnvsedu, aes(x = education, y = earn_log)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  scale_x_continuous(labels = scales::number_format(scale = 1e0, accuracy = 1)) +
  xlab("Number of years of education") +
  ylab("Log-transformed yearly earnings") + 
  labs(title = "Number of years of education and log-transformed earnings")

p2

m_lr2 <- lm(earn_log ~ education, data = df_earnvsedu)
summary(m_lr2)

p2_residuals <- ggplot(df_earnvsedu, aes(x = education, y = residuals(m_lr2))) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  xlab("Number of years of education") +
  ylab("Residuals") +
  labs(title = "Residuals from Linear Regression Model")

p2_residuals



# Linear regression of mean
result <- tapply(df$earn, df$education, mean)
years_edu <- as.numeric(levels(factor(df$education)))

# Create a data frame for plotting
plot_data <- data.frame(education = years_edu, mean_earnings = result)

# Create a scatter plot
plot1 <- ggplot(data = plot_data, aes(x = education, y = mean_earnings)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) + 
  xlab("Number of years of education") +
  ylab("Average earning") + 
  labs(title = "Linear regression of mean earnings")
plot1

ml_1 <- lm(result ~ years_edu, data = plot_data)
summary(ml_1)$r.squared
summary(ml_1)
plot_data$residuals <- residuals(ml_1)

plot1_res <- ggplot(plot_data, aes(x = years_edu, y = residuals)) +
  geom_point() + geom_line(y=0) +
  xlab("Number of years of education ") +
  ylab("Residual") + 
  labs(title = "Residuals for linear regression")

plot1_res


#Linear regression removing outlier
df <- df[df$earn != 400000, ]

result <- tapply(df$earn, df$education, mean)
years_edu <- as.numeric(levels(factor(df$education)))

# Create a data frame for plotting
plot_data2 <- data.frame(education = years_edu, mean_earnings = result)

# Create a scatter plot
plot2 <- ggplot(data = plot_data2, aes(x = education, y = mean_earnings)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) + 
  xlab("Number of years of education") +
  ylab("Average earning") + 
  labs(title = "Linear regression of mean earnings")
plot2

ml_2 <- lm(result ~ years_edu, data = plot_data2)
summary(ml_2)$r.squared

plot_data2$residuals <- residuals(ml_2)

plot2_res <- ggplot(plot_data2, aes(x = years_edu, y = residuals)) +
  geom_point() + geom_line(y=0) +
  xlab("Number of years of education ") +
  ylab("Residual") + 
  labs(title = "Residuals for linear regression")

plot2_res


## Linear regression using mean log transformation
result <- tapply(df_earnvsedu$earn_log, df_earnvsedu$education, mean)
years_edu <- as.numeric(levels(factor(df_earnvsedu$education)))


# Create a data frame for plotting
plot_data_log <- data.frame(education = years_edu, mean_earnings = result)

# Create a scatter plot
plot_log <- ggplot(data = plot_data_log, aes(x = education, y = mean_earnings)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) + 
  xlab("Number of years of education") +
  ylab("Average earning") + 
  labs(title = "Linear regression of mean earnings")
plot_log

ml_log <- lm(result ~ years_edu, data = plot_data_log)
summary(ml_log)$r.squared
summary(ml_log)

plot_data_log$residuals <- residuals(ml_log)

plot_log_res <- ggplot(plot_data_log, aes(x = years_edu, y = residuals)) +
  geom_point() + geom_line(y=0) +
  xlab("Number of years of education ") +
  ylab("Residual") + 
  labs(title = "Residuals for linear regression")

plot_log_res


## Removing different groups of earnings
df_edu_3 <- df_edu[df_edu$education != 3, ] 

result <- tapply(df_edu_3$earn_log, df_edu_3$education, mean)
years_edu <- as.numeric(levels(factor(df_edu_3$education)))


# Create a data frame for plotting
plot_data_3 <- data.frame(education = years_edu, mean_earnings = result)

# Create a scatter plot
plot_3 <- ggplot(data = plot_data_3, aes(x = education, y = mean_earnings)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) + 
  xlab("Number of years of education") +
  ylab("Average earning") + 
  labs(title = "Linear regression of mean earnings")

plot_3

ml_3 <- lm(result ~ years_edu, data = plot_data_3)
summary(ml_3)$r.squared

plot_data_3$residuals <- residuals(ml_3)

plot_3_res <- ggplot(plot_data_3, aes(x = years_edu, y = residuals)) +
  geom_point() + geom_line(y=0) +
  xlab("Number of years of education ") +
  ylab("Residual") + 
  labs(title = "Residuals for linear regression")

plot_3_res
