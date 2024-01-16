library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggthemes)
library(psych)



pisa = read.csv("./PISA VN 2015.csv")
attach(pisa)
plot(PV1SCIE ~ WEALTH, pch=16)

# Hệ số tương quan
cor.test(PV1SCIE, WEALTH)



