pisa = read.csv("./PISA VN 2015.csv")

pisa$REGION = factor(pisa$REGION, levels=c( "NORTH","CENTRAL", "SOUTH"))

pisa$AREA = factor(pisa$AREA, levels=c("URBAN", "RURAL",
"REMOTE"))


attach(pisa)

# install.packages(c("gridExtra", "ggthemes", "ggplot2", "dplyr"))

library(ggplot2)
library(dplyr)
library(gridExtra)
library (ggthemes)


# Tần số gãy xương
par(mfrow=c(1,1))
freq = table(REGION)
barplot(freq, main="Hoang Kim Viet Tan", xlab="Reglion", ylab="NO.Students")

# Số học sinh theo vùng
p = ggplot(data=pisa , aes(x= REGION))
p = p + geom_bar(aes(y =(..count..)/sum(..count..)))
p = p + labs(title = "Hoang Kim Viet Tan")
p


# Số học sinh theo vùng và giới tính
p = ggplot(data=pisa ,aes(x=REGION ,fill=Gender))
p = p + geom_bar(aes(y =(..count..)/sum(..count..)))
p = p + labs(title = "Hoang Kim Viet Tan")
p



# Dùng ggplot và phân trăm

p = ggplot(pisa %>% count(REGION, Gender) %>% mutate(pct=n/sum(n)), aes(REGION, n, fill=Gender ) )

p = p + geom_bar(stat="identity")

p = p + geom_text(aes(label=paste(sprintf("%1.1f", pct*100),"%" )), position=position_stack(vjust=0.5))
p = p + labs(title = "Hoang Kim Viet Tan")
p + xlab("Region") + ylab("percent")


# Điểm trung bình môn khoa học theo vùng

summary = pisa %>% group_by(REGION, AREA) %>%
summarise(mean = mean(PV1SCIE, na.rm=T)) 

p = ggplot(data=summary, aes(x=REGION, fill=AREA,
y=mean) )
p = p + labs(title = "Hoang Kim Viet Tan")
p = p + geom_bar(stat = "identity", position="dodge")
p


# Điểm trung bình môn khoa học theo HISCED

pisa$Hisced = as.factor(pisa$HISCED)
attach(pisa)
summary = pisa %>%
group_by(REGION, Hisced) %>%
summarise(mean = mean(PV1SCIE, na.rm=T))

p = ggplot(summary, aes(x=reorder(Hisced, -mean), y=mean, fill=Hisced))

p = p + labs(title = "Hoang Kim Viet Tan")

p = p + geom_bar(stat="identity" , width=1, position= "dodge") + coord_flip()
p = p + facet_grid(. ~ REGION) + theme(legend.position="none")
p = p + geom_text(aes(y=mean, ymax=mean, label=round(mean, 0)),

position= position_dodge(width=1), size=3, vjust=0.9, hjust=1,
slze=3, color= "white")
p

# Điểm trung bình môn khoa học theo School

pisa$School = as.factor(pisa$CNTSCHID)
summary = pisa %>% group_by(REGION, School) %>% summarise(mean = mean(PV1SCIE, na.rm=T))
p = ggplot(summary, aes(x=reorder(School, -mean), y=mean, fill=School) )
p = p + geom_bar(stat="identity", width=1, position="dodge") + coord_flip()
p = p + facet_grid(. ~ REGION) + theme(legend.position="none")
p = p + labs(title = "Hoang Kim Viet Tan")
p

# Dùng barplot để vẽ số trung bình và sai số chuẩn

summary = pisa %>% group_by(REGION) %>% summarise(m = mean(PV1MATH), s = sd(PV1MATH), nn = n(), se = sd(PV1MATH)/sqrt(n()))

p = ggplot(data=summary, aes(x=REGION, y=m, fill=REGION))

p = p + geom_bar(stat="identity")
p = p + labs(title = "Hoang Kim Viet Tan")

p = p + geom_errorbar(aes(ymin=m-s, ymax=m+s),
width=0.2)
p

# Dùng barplot để vẽ số trung bình và sai số chuẩn

summary = pisa %>% group_by(REGION, Gender) %>% summarise(m = mean(PV1MATH), s = sd(PV1MATH), nn = n(), se = sd(PV1MATH)/sqrt(n()))
p = ggplot(data=summary, aes(x=REGION, y=m, fill=Gender) )
p = p + geom_bar(stat="identity", position="dodge")
p = p + labs(title = "Hoang Kim Viet Tan")
p = p + geom_errorbar(aes(ymin=m-se, ymax=m+se), position="dodge", width=0.2)
p


# phân bố số học sinh theo vùng và miền

p = ggplot(pisa %>% count(REGION, AREA) %>%
mutate(pct=n/sum(n)), aes(REGION, n, fill=AREA) )

p = p + geom_bar(stat="identity")

p = p + geom_text(aes(label=paste0(sprintf("%1.1f" ,
pct*100), "%")), position=position_stack(vjust=0.5))

p = p + labs(title = "Hoang Kim Viet Tan")
p + xlab("Region") + ylab("percent")


# phân bố HISCED theo giới tính

# tạo biên factor

pisa$HISCED = as.factor(pisa$HISCED)
pisa$PARED = as.factor(pisa$PARED)

# vẽ biêu đô

p = ggplot(data=pisa, aes(x=HISCED, fill=Gender) )

p = p + geom_bar(aes(y = (..count..)/sum(..count..)))

p = p + labs(title = "Hoang Kim Viet Tan")
p + xlab("parental Highest Education")



# Điểm trung bình môn khoa học theo vùng

p = ggplot(pisa, aes(x=REGION, y=PV1SCIE,
fill=REGION) )

p = p + stat_summary (fun.y="mean", geom="bar")

p = p + labs(title = "Hoang Kim Viet Tan")
p = p + coord_flip() + theme(legend.position="none")
p



# Điểm trung bình cho mỗi trường miền Trung

central = subset(pisa, REGION=="CENTRAL")

means = aggregate(central$PV1MATH, by=list(central$School),
FUN=mean)

colnames(means) = c( "School", "Mean")

mean = transform(means, School=reorder(School, order(Mean,
decreasing=T)))

p = ggplot(mean, aes(x=reorder(School, Mean), y=Mean, fill=School))

p = p + geom_bar(stat="identity", width=1, color="white",
position=position_dodge())

p = p + labs(title = "Hoang Kim Viet Tan")

p = p + coord_flip() + theme(legend.position= "none")

p + geom_text(aes(y=Mean, ymax=Mean, label=round(Mean, 0)),
position= position_dodge(width=1), size=2, vjust=0.9, hjust=1,
size=2, Color="white")

# phân bố điểm môn khoa học

par(mfrow=c(1,2))
hist(PV1SCIE)
hist(PV1SCIE, col= "blue", border= 'white', xlab="Hoang Kim Viet Tan"
, main="Hoang Kim Viet Tan")

# phân bố điểm môn khoa học: thêm "density"'

par(mfrow=c(1,2))

hist(PV1SCIE, col="blue", border= "white", xlab="Điểm
môn khoa học", main="Hoang Kim Viet Tan")

# thêm đensity line

hist(PV1SCIE, prob=T, col="blue", border="white",
xlab="Điểm môn khoa học", main="Hoang Kim Viet Tan")

lines(density(PV1SCIE, na.rm=T), col="red", lwd=2)


# Biểu đồ phân bố dùng ggplot2: 

library(gridExtra);


p = ggplot(pisa,aes(x=PV1SCIE))
p= p + geom_histogram(color="white", fill="blue") 
p = p + labs(title = "Hoang Kim Viet Tan")

p


# Biểu đồ phân bố dùng : đơn giản + density

p = ggplot(pisa, aes(x=PV1SCIE))

p = p + geom_histogram(aes(y=..density..), color= "white", fill="blue")

p = p + geom_density(col= "red")
p = p + labs(title = "Hoang Kim Viet Tan")
p


# Dùng ggplot2: đơn giản + density + theo nhom

p = ggplot(pisa, aes(x=PV1SCIE, fill=AREA))

p1 = p + geom_histogram(position="dodge") + labs(title = "Hoang Kim Viet Tan")

p2 = ggplot(pisa, aes(x=PV1SCIE, fill=AREA,
color=AREA)) +  geom_density(alpha = 0.1) + labs(title = "Hoang Kim Viet Tan") 

grid.arrange(p1, p2, nrow=2)


# Đến phiên bạn...

p = ggplot(pisa, aes(x=PV1READ, fill=REGION))

p1 = p + geom_histogram(position="dodge")+ labs(title = "Hoang Kim Viet Tan")

p2 = ggplot(pisa, aes(x=PV1READ, fill=REGION,
color=REGION)) + geom_density(alpha = 0.1)+ labs(title = "Hoang Kim Viet Tan")

grid.arrange(p1, p2, nrow=2)

# Đến phiên bạn...

p = ggplot(pisa, aes(x=PV1READ, fill=AREA) )

p1 = p + geom_histogram(position="dodge") + facet_grid(REGION ~ .)+ labs(title = "Hoang Kim Viet Tan")

p2 = p + geom_density(aes(x=PV1READ, color=Gender,
alpha=0.5)) + facet_wrap(~Gender)+ labs(title = "Hoang Kim Viet Tan")

grid.arrange(p1, p2, nrow=2)


# Điểm môn đọc & hiểu

par(mfrow=c(1,2)) 

boxplot (PV1READ) 

boxplot(PV1READ ~ Gender, main="Hoang Kim Viet Tan", xlab="Điem mon doc & hieu", ylab= "Score",
col="blue", border="purple")


# Biểu đô hộp dùng ggplot2: don gian

# tạo biến factor ,
Hisced = as.factor(HISCED)

# vẽ biểu đồ theo Hisced 
p = ggplot(pisa, aes(x=Hisced, 
y=PV1READ, color=Hisced) ) 
p = p + geom_boxplot() 
p = p + labs(title = "Hoang Kim Viet Tan")
p


#Biểu đô hộp dùng ggplot2:

# vẽ biểu đồ theo Hisced :

p = ggplot(pisa, aes(x=REGION, 
y=PV1READ, fill=REGION))
p = p + labs(title = "Hoang Kim Viet Tan")
p = p + geom_boxplot()
p


# Đến phiên bạn...
library (ggthemes)
fill = "#4271AE"
line = "#1F3552"

p = ggplot(pisa, aes(x=Hisced, y=PV1READ, color=Hisced))
p = p + labs(title = "Hoang Kim Viet Tan")
p = p + geom_boxplot(fill=fill, colour=line)
p + theme_economist()


# Đến phiên bạn... (điểm theo giới tính và Hisced)

p = ggplot(pisa, aes(x=Hisced, y=PV1READ, fill=Hisced))
p = p + geom_boxplot(aes(fill=Hisced), alpha=1)

p = p + geom_jitter(aes(color=Hisced, outlier.colour="red"),
size=1.5, alpha=.2)

p = p + theme(axis.title.x = element_text(color= "blue",
size=10, face= "bold"), axis.text.x = element_text(angle=45,
vjust=0.5, size=10)) + xlab("HISCED") + ylab("Điem Reading")

p = p + labs(title = "Hoang Kim Viet Tan")

p + theme_economist()



# Đến phiên bạn... (điểm theo giới tính và Hisced)

p = ggplot(pisa, aes(x=Hisced, y=PV1READ, fill=Hisced))

p = p + geom_boxplot(aes(fill=Hisced), alpha=1)

p = p + geom_jitter(aes(color=Hisced, outlier.colour="red"),
size=1.5, alpha=.2)

p = p + theme(axis.title.x = element_text(color= "blue",
size=10, face= "bold"), axis.text.x = element_text(angle=45,
vjust=0.5, size=10)) + xlab("HISCED") + ylab("Điem Reading")
p = p + labs(title = "Hoang Kim Viet Tan")

p + facet_wrap(~REGION) + theme_economist()


# Liên quan giữa PV1SCIE và WEALTH

par(mfrow=c(2,2))

plot(WEALTH, PV1SCIE, main = "Hoang Kim Viet Tan") 

plot(PV1SCIE ~ WEALTH, pch=16) 

plot(PV1SCIE ~ WEALTH,
xlab="Wealth", ylab="Reading")

plot(PV1SCIE ~ WEALTH,
x1ab="Wealth", ylab="Reading",
col="blue")

abline(lm(PV1SCIE ~ WEALTH),
col="red")



# Biểu đồ tương quan với ggplot2

p = ggplot(pisa, aes(x=WEALTH, y=PV1SCIE))
p = p + labs(title = "Hoang Kim Viet Tan")
p = p + geom_point()
p


# Biểu đồ tương quan với ggplot2: tô màu theo Gender

p = ggplot(pisa, aes(x=WEALTH, y=PV1SCIE, color=Gender) )
p = p + labs(title = "Hoang Kim Viet Tan")
p1 =  p + geom_point()
p2 = p + geom_point() +
geom_smooth(method="lm")
grid.arrange(p1, p2, nrow=2)


#Đến phiên bạn ... PV1READ theo giới tính và tuổi

p = ggplot(pisa, aes(x=AGE, y=PV1READ, color=Gender))
p = p + labs(title = "Hoang Kim Viet Tan")
p1 = p + geom_point()
p2 = p + geom_point() + geom_smooth(method="lm")
grid.arrange(p1, p2, nrow=2)


# Dữ liệu PISA: Misced theo vùng


# tạo biên factor
pisa$Misced = as.factor(pisa$MISCED)

pisa$Pared = as.factor(pisa$PARED)

# vẽ biêu đô

p = ggplot(data=pisa, aes(x=Misced, fill=REGION))
p = p + labs(title = "Hoang Kim Viet Tan")
p = p + geom_bar(aes(y = (..count..)/sum(..count..)))
p + xlab("MISCED")


# Dữ liệu PISA: Misced theo vùng

# vẽ biểu đồ theo tuổi và giới

# tính stackbar

p = ggplot(data=pisa, aes(x=PARED,
y=AREA, fill=Gender) )

p = p + labs(title = "Hoang Kim Viet Tan")
p = p + geom_bar(aes(y =
(..count..)/sum(..count..)))
p + xlab("PARED")

# vẽ biểu đồ theo tuổi và giới
# tính với dodge

p = ggplot(data=pisa,
aes(x=PARED, fill=AREA) )

p = p + labs(title = "Hoang Kim Viet Tan")
p = p+
geom_bar(position="dodge", aes(y
= (..count..)/sum(..count..)))

p + xlab("PARED")

# Biểu đồ hộp: điểm toán theo PARED

# install.packages ( "ggthemes")

fill = "#4271AE"
line = "#1F3552"

p = ggplot(pisa, aes(x=as.factor(PARED), y=PV1MATH) )
p = p + labs(title = "Hoang Kim Viet Tan")
p = p + geom_boxplot(fill=fill, colour=line)

p + theme_economist()

# Biểu đồ hộp: điểm toán theo PARED và giới tính

p = ggplot(pisa, aes(x=REGION, y=PV1SCIE, fill=Gender) )
p = p + labs(title = "Hoang Kim Viet Tan")
p = p + geom_boxplot(aes(fill=Gender), alpha=1)

p = p + geom_jitter(aes(color=Gender, outlier.colour="red"), size=1.5,
alpha=.2)

p = p + theme(axis.title.x = element_text(color="blue", size=10,
face="bold"), axis.text.x = element_text(angle=45, vjust=0.5,
size=10))

p + theme_economist()

# Dont know

p = ggplot(pisa, aes(x=AREA, y=PV1READ, f1ll=Gender) )
p = p + labs(title = "Hoang Kim Viet Tan")

p = p + geom_boxplot(aes(fill=Gender), alpha=1)

p = p + geom_jitter(aes(color=Gender, outlier.colour="red"), size=1.5,
alpha=.2)

p = p + theme(axis.title.x = element_text(color="blue", size=10,
face="bold"), axis.text.x = element_text(angle=45, vjust=0.5,
size=10))

p + facet_wrap(~Gender) + theme_economist()


# Điểm môn PV1SCIE theo WEALTH và giới tính


p = ggplot(pisa, aes(x=WEALTH, y=PV1SCIE, color=Gender) )

p = p + labs(title = "Hoang Kim Viet Tan")

p1 = p + geom_point()

p2 = p + geom_point() + geom_smooth(method="lm")

grid.arrange(p1, p2, nrow=2)


