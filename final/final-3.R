data = read.csv("choldata.csv", header=TRUE)
attach(data)

library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggthemes)
library(psych)

# # Hien thi
# plot(age ~ chol, pch=16)


# Hệ số trương quan
cor(data , method = "kendall")

# Kiểm giả đinh
cor.test(age, chol)

# Hoi quy
fit = lm(age ~ chol, data=data)
summary(fit)

# Call:
# lm(formula = age ~ chol, data = data)

# Residuals:
#    Min     1Q Median     3Q    Max 
# -9.919 -3.724  1.173  3.154  6.526

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -11.780      4.868   -2.42   0.0278 *  
# chol          15.184      1.419   10.70 1.06e-08 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 4.906 on 16 degrees of freedom
# Multiple R-squared:  0.8775,    Adjusted R-squared:  0.8698 
# F-statistic: 114.6 on 1 and 16 DF,  p-value: 1.058e-08

# Ve 
par(mfrow=c(2,2))
plot(fit)


## Hoi quy da bien

fit = lm (chol ~ age + bmi, data=data)
summary(fit)

# Call:
# lm(formula = chol ~ age + bmi, data = data)

# Residuals:
#     Min      1Q  Median      3Q     Max
# -0.3762 -0.2259 -0.0534  0.1698  0.5679 

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept) 0.455458   0.918230   0.496    0.627
# age         0.054052   0.007591   7.120  3.5e-06 ***
# bmi         0.033364   0.046866   0.712    0.487
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 0.3074 on 15 degrees of freedom
# Multiple R-squared:  0.8815,    Adjusted R-squared:  0.8657
# F-statistic: 55.77 on 2 and 15 DF,  p-value: 1.132e-07

# chol = 0.455 + 0.054 * (age) + 0.033 * (bmi)
# R^2 = 88,15%

# Phương trình cho biết khi độ tuổi tăng 1 năm thì cholesterol tăng 0.054 mg/L (ước số này
# không khác mấy so với 0.0578 trong phương trình chỉ có độ tuổi), và mỗi 1 kg/m2 tăng
# BMI thì cholesterol tăng 0.0333 mg/L. Hai yếu tố này “giải thích” khoảng 88.2% (R2 =
# 0.8815) độ dao động của cholesterol giữa các cá nhân









