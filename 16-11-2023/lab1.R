pisa = read.csv("./PISA VN 2015.csv")
pisa$REGION = factor(pisa$REGION, levels=c( "NORTH",
"CENTRAL", "SOUTH"))
pisa$AREA = factor(pisa$AREA, levels=c("URBAN", "RURAL",
"REMOTE"))
attach(pisa)

library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggthemes)
library(psych)



pisa = read.csv("./PISA VN 2015.csv")
attach(pisa)
plot(PV1SCIE ~ WEALTH, pch=16)

# Hệ số trương quan
cor.test(PV1SCIE, WEALTH)


# Mô hình và R code

fit = lm(PV1SCIE ~ WEALTH, data=pisa)
summary(fit)

# Kiểm giả đinh

fit = lm(PV1SCIE ~ WEALTH, data=pisa)
par(mfrow=c(2,2))
plot(fit)

# Mô hình về mối quan hệ giữa giới tính  và điểm

fit = lm(PV1READ ~ Gender , data=pisa)
summary(fit)


## Đến phiên bạn
pisa = read.csv("./PISA VN 2015.csv")
pisa$REGION = factor(pisa$REGION, levels = c("NORTH", "CENTRAL", "SOUTH"))
pisa$AREA = factor(pisa$AREA, levels = c("URBAN", "RURAL", "REMOTE"))
attach(pisa)
#
n1 = lm(PV1SCIE ~ REGION, data = pisa)
summary(n1)

n2 = lm(PV1SCIE ~ AREA , data = pisa)
summary(n2)

n3 = lm(PV1SCIE ~ REGION, data = pisa)
summary(n3)

n4 = lm(PV1SCIE ~ WEALTH, data = pisa)
summary(n4)
#

m0 = lm(PV1SCIE ~ 1)
anova(m0)

m1 = lm(PV1SCIE ~ WEALTH, data = pisa)
anova(m1)


# Kiểm tra gỉa định
m4 = lm(PV1SCIE ~ WEALTH, data = pisa)
summary(m4)
par(mfrow=c(2,2))
plot(m4)


