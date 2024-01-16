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


# # Mối quan hệ giữa giới tính và điểm
# fit = lm(PV1READ ~ Gender, data=pisa)
# summary(fit)
#
#
# # Phân tích vài mô hình
#
# n1 = lm(PV1SCIE ~ AGE, data = pisa)
# summary(n1)
#
# n2 = lm(PV1SCIE ~ AGE + Gender, data = pisa)
# summary(n2)
#
# n3 = lm(PV1SCIE ~ AGE + Gender + PARED, data = pisa)
# summary(n3)
#
# n4 = lm(PV1SCIE ~ AGE + Gender + PARED + WEALTH, data = pisa)
# summary(n4)
#
# n5 = lm(PV1SCIE ~ AGE + Gender + PARED + WEALTH + SCHLTYPE, data = pisa)
# summary(n5)
#
# #
# par(mfrow=c(2,2))
# plot(n5)


# ## Đến phiên bạn
pisa = read.csv("./PISA VN 2015.csv")
pisa$REGION = factor(pisa$REGION, levels = c("NORTH", "CENTRAL", "SOUTH"))
pisa$AREA = factor(pisa$AREA, levels = c("URBAN", "RURAL", "REMOTE"))
attach(pisa)


# Tương quan giữa các điểm
pisa = read.csv("./PISA VN 2015.csv")
vars=cbind(PV1SCIE,PV1MATH, PV1READ, WEALTH, PARED)
pairs.panels(vars)

# Khác biệt giữa các vùng

n1 = lm(PV1SCIE ~ REGION, data = pisa)
summary(n1)
n2 = lm(PV1SCIE ~ AREA , data = pisa)
summary(n2)
n3 = lm(PV1SCIE ~ REGION + AREA , data = pisa)
summary(n3)
n4 = lm(PV1SCIE ~ REGION + AREA + WEALTH, data = pisa)
summary(n4)
n5 = lm(PV1SCIE ~ REGION + AREA + WEALTH + PARED, data = pisa)
summary(n5)


#
#



