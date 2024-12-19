pisa = read.csv("./PISA VN 2015.csv")
attach(pisa)
names(pisa)

# Bai 3
data_filtered <- subset(pisa$Gender, pisa$PV1READ >= 500)
table(data_filtered)


cor.test(pisa$PV1SCIE, pisa$WEALTH)

# Bai 4
# Ve mo hinh hoi quy

fit = lm(pisa$PV1SCIE ~ pisa$WEALTH)
par(mfrow=c(2,2))
plot(fit)

# Thong tin mo hinh
summary(fit)