data = read.csv("data.csv", header=TRUE)
attach(data)

library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggthemes)
library(psych)

# Hien thi
plot(age ~ chol, pch=16)

# Hệ số trương quan
cor(data , method = "kendall")

# Kiểm giả đinh
cor.test(age, chol)

# Hoi quy
fit = lm(age ~ chol, data=data)
summary(fit)

# Ve 
par(mfrow=c(2,2))
plot(fit)



