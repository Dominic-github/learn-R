pisa = read.csv("./PISA VN 2015.csv")

pisa$REGION = factor(pisa$REGION, levels=c( "NORTH",
"CENTRAL", "SOUTH"))

pisa$AREA = factor(pisa$AREA, levels=c("URBAN", "RURAL",
"REMOTE"))

attach(pisa)

# Tần số gãy xương
par(mfrow=c(1,2))

freq = table(REGION)
barplot(freq)

barplot(freq, xlab="Reglion", ylab="NO.Students")

# Số học sinh theo vùng

library(ggplot2)

p = ggplot(data=pisa, aes(x= REGION))

p = p + geom_bar(aes(y =(..count..)/sum(..count..)))

# Số học sinh theo vùng và giới tính

p = ggplot(data=pisa ,aes(x=REGION ,fill=Gender))
p = p + geom_bar(aes(y =(..count..)/sum(..count..)))

# Dùng ggplot và phân trăm

library(dplyr)

p = ggplot(pisa %>% count(REGION, Gender) %>%
mutate(pct=n/sum(n)), aes(REGION, n, fill=Gender ) )

p = p + geom_bar(stat="identity ")

p = p + geom_text(aes(label=paste0(sprintf("%1.1f",
ppct*100),"%" )), pOosition=posltion stack(vj]ust=0.5))

p + xlab("Region") + ylab("percent")

# Điểm trung bình môn khoa học theo vùng

summary = pisa %>%
grOUp_Dby(REGION, AREA) %>%

summarlse(mean =

mean(PV1SCIE, na.rm=T)) 400-

p = ggplot(data=summary ,
aes(x=REGI1ON, f111=AREA,
y=mean) )

mean

p = p + geom_bar(stat = "1dentlty", posltion=" dodge")

# Điểm trung bình môn khoa học theo HISCED

pisasSH1sced = as.factLor(pisaSHISCED)
attach(pisa)
summary = pisa %>%
group_byÿy(REGION, Hisced) %>%
sumnarlse(mean = mean(PV1SCIE, na.rm=T))

p = ggplot(sumnary, aes(x=reorder(Hlsced, -mean), ÿy=mean,
f11ll=Hisced))

p = p + geom_bar(stat="identity" , width=1l, posltion= "dodge") +
coord flip()

p = p + facet _grid(. ~ REGION) + theme(legend.position="none")
p = p + geom_text(aes(y=mean, ymax=mean, label=round(mean, 0)),

position= posltion dodge(width=l), size=3, vjJust=0.9, hjust=l,
slze=3, COolor= "white")
reorder(Hisced, -mean)

# Điểm trung bình môn khoa học theo School

pisasSSchool = as.factor(pisa$SCNTSCHID)
attach(pisa)

sumnary = pisa %>% group_bÿy(REGION, School) %3>%
sumnarlse(mean = mean(PV1SCIE, na.rm=T))

p = ggplot(sumnmary, aes(x=reorder(School, -mean), ÿy=mean, flll=
School) )

p = p + geom_bar(stat="identity", width=l, posltion="dodge") +
coord £flip()

p = p + facet _grid(. ~ REGION) + theme(legend.position="none")



# Dùng barplot để vẽ số trung bình và sai số chuẩn

1ibrary(dplyr)

sumnary = pisa %>3 group _by(REGION) %>%
sumnarlse(m = mean(PV1MATH), S = sd(PV1MATH),
nn = n(), se = sd(PV1MATH)/sqrt(n()))

p = ggplot(data=sumnary, aes(x=REGION, y=m,
f111=REGION) )

p = p + geom_bar(stat="identity")

p = Dp + geom_errorbar(aes(ymin=m-s, ymax=mts),
wildth=0.2)

# Dùng barplot để vẽ số trung bình và sai số chuẩn

summary = pisa $>% group_by(REGION, Gender) $%>%
sumnarlse(m = mean(PV1MATH), S = sd(PV1MATH), nn =
n(), se = sd(PV1MATH) /sgqrt(n()))

p = ggplot(data=sumnary, aes(x=REGION, y=m,
f1ll=Gender) )

p = Dp + geom_bar(stat="ldentlty", posltion="dodge")

p = p + geom_errorbar(aes(ymin=m-se, ymax=m+t+se)
pOS1t1on= dodge”, width=0.2)

# phân bố số học sinh theo vùng và miền

1ibrary(dplyr)

p = ggplot(pisa %>% count(REGION, AREA) %>%
mutate(pct=n/sum(n)), aes(REGION, n, f111=AREA,) )

p = p + geom_bar(stat="identity")

p = D + geom_text(aes(label=paste0(sprintf("s%1l.1f,
pct*100), $")), pOoS1t1on=posltion stack(vJust=0.5))

p + xlab( "Region") + ylab( "percent")


# phân bố HISCED theo giới tính

# tạo biên factor

pisaSHISCED = as.factor(pisaSHISCED)
ppisaspARED = as.factor(pisasSpARED)

attach(pisa)

# vẽ biêu đô
ggplot(data=pisa, aes(x=HISCED, f1ll=Gender) )
p + geom_bar(aes(y = (..count..)/sum(..count..)))

xlab("parental Highest Education")



# phân bố điểm môn khoa học

par(mfrow=c(1l,2))
hist(PV1SCIE)

hist(PV1SCIE, col= blue”, border= 'whlte', xlab="Điểm
môn khoa học", main="phân bố điểm môn khoa học")


# phân bố điểm môn khoa học: thêm "density"'

par(mfrow=c(1l,2))

hist(PV1SCIE, col="blue", border= "white", xlab="Điểm
môn khoa học", main="phân bố điểm môn khoa học")

# thêm đensity line

hist(PV1SCIE, Drob=T, col=" blue”, border="whlite”,
x1lab="Điểm môn khoa học", main="phân bố điểm môn
khoa học")

lines(denslty(PV1SCIE, na.rm=T), col="red", 1wd=2)


# Biểu đồ phân bố dùng ggplot2: 

library(ggplot2);
library(grldExtra)
p = ggplot(pisa,aes(x=PV1SCIE))

p-= p + geom_h1stogram(color="white", fill="blue") 


# Biểu đồ phân bố dùng : đơn giản + density

p = ggplot(pisa, aes(x=PV18C1E))

p = p + geom_histogram(aes(y=..density..), COlor= "whilte",fill="blue")

p = p + geom_dđensity(col= "red")


# Dùng ggplot2: đơn giản + density + theo nhom

p = ggplot(pisa, aes(x=PV1SCIE, fi11=AREA))



p1 = p + geom_histogram(position="dodge")

p2 = ggplot(pisa, PV1SCGIE, aes(x=PV18CIE, f111=ARFEA,
CO1OTF=AREA)) +  geom_density(alpha = 0.1) 

grid.arrange(pl, p2, nrow=2)


# Đến phiên bạn...

library(ggplot2); 
library(grldExtra)
p = ggplot(pisa, aes(x=PV1READ, f111=REGION))

p1 = p + geom_histogram(position="dodge")

p2 = ggplot(pisa, aes(x=PV1READ, f£111=REGION,
CO1OT=REGION)) + geom_density(alpha = 0.1)

grid.arrange(pl, p2, nrow=2)
# Đến phiên bạn...

p = ggplot(pisa, aes(x=PV1READ, £ill=ARFA) )

ppl = p †+ geom_histogram(position="dodge") + facet grld(REGION ~ .)

p2 = p + geom_denslty(aes(x=PV1READ, color=Gender,
alpha=0.5)) + facet _wrap(~Gender)

grid.arrange(pl, p2, nrow=2)


# Điểm môn đọc & hiểu

par(mfrow=c(1,2)) 

boxplot (PV1READ) 

boxplot(PV1READ ~ Gender,
main="phân bố điểm môn đọc & hiểu", xlab=" Điểm môn đọc & hiểu " ylab= "Score",
col="blue"”, border="purple”")




# Biểu đô hộp dùng ggplot2: don gian

# tạo biến factor ,
Hisced = as.factor (H1SCED)

# vẽ biểu đồ theo Hisced 
p = ggplot(pisa, aes(x=H1sced, 
Y=PV1READ, color=Hisced) ) 
p = p + geom_boxplot() 




#Biểu đô hộp dùng ggplot2:

# vẽ biểu đồ theo Hisced :

p = ggplot(pisa, aes(x=REGION, 
V=PV1READ, f111=REGION))

p = p + geom_boxplot()


# Đến phiên bạn...

install.packages(" ggthemes")
library (ggthemes)
fill = "#4271AR"
line = "Z1F3552"

p = ggplot(pisa, aes(x=Hisced, y=zPV1READ, color=Hisced))
p + geom_boxplot(fill=fill, colour=line)
p + theme _economist()


# Đến phiên bạn... (điểm theo giới tính và Hisced)

p = ggplot(pisa, aes(x=Hisced, y=PV1READ, f111=Hisced))
p = Dp + geom_boxplot(aes(flll=Hisced), alpha=l)

p = p + geom_jitter(aes(color=Hisced, outlier.colour="red"),
slze=l.5, alpha=.2)

p = p + theme(axls.tltle.x = element text(color= "blue",
Slze=10, face= "bold"), axis.text.x = element text(angle=45,

VjJust=0.5, size=10)) + xlab("HISCED") + ylab("Điểm Reading")

p + theme _economist.()



# Đến phiên bạn... (điểm theo giới tính và Hisced)

p = ggplot(pisa, aes(x=H1sced, y=PV1READ, f11ll=H1sced))
p = Dp + geom_boxplot(aes(flll=Hisced), alpha=l)

p = p + geom_jitter(aes(color=Hisced, outlier.colour="red"),
slze=l.5, alpha=.2)

p = p + theme(axls.tltle.x = element text(color= blue",
Slze=10, face= bold"), axis.text.x = element text(angle=45,

VjJust=0.5, size=10)) + xlab("HISCED") + ylab("Điểm Reading")

p + facet wrap(~REGION) + theme economist()





# Liên quan giữa PV1SCIE và WEALTH

par(mfrow=c(2,2))

plot(WEALTH, PV1SCIE) 

pPlot(PV1SCIE ~ WEALTH, pch=l6) 

plot(PV1SCIE ~ WEALTH,
x1lab="Wealth", ylab="Reading")

plot(PV1SCIE ~ WEALTH, + ¬
x1ab="Wealth", ylab="Reading",
col="blue")

abline(lm(PV1SCIE ~ WEALTH)
col="red”")



# Biểu đồ tương quan với ggplot2

library(ggplot2);
library(grldExtra)

P = ggplot(pisa, aes(x=WEALTH,
y=PV1SCIE))

P = P + geom_point()


# Biểu đồ tương quan với ggplot2: tô màu theo Gender

P = ggplot(pisa, aes(x=WEALTH,
y=PV1SCIE, color=Gender) )
p1 p + geom_point()



P2 = Dp + geom_point() +
geom_smooth(method="lm")

grid.arrange(pl, p2, nrow=2)


#Đến phiên bạn ... PV1READ theo giới tính và tuổi

pP = ggplot(pisa, aes(x=AGE, y=PV1READ, color=Gender))
pl = p + geom_point.()

p2 = p + geom_point() + geom_smooth(method="lm")

grid.arrange(pl, p2, nrow=2)


# Tom tat

# Dữ liệu PISA: Misced theo vùng

pisa = read.csv("./PISA VN 2015.csv")
attach(pilsa)

# tạo biên factor
pisasSM1sced = as.factor(pisaSMISCED)

pisasPared = as.factor(pisasPARED)

# vẽ biêu đô

P = ggplot(data=pisa, aes(x=Misced, f111=REGION))
P = P + geom_bar(aes(y = (..count..)/sum(..count..)))
p + xlab("MISCED")


# Dữ liệu PISA: Misced theo vùng

# vẽ biểu đồ theo tuổi và giới

# tính stackbar

P = ggplot(data=pisa, aes(x=PARED,
Y=ZAREA, fill=Gender) )

P = P †+ geom_bar(aes(y =
(..count..)/sum(..count..)))
p + xlab("PARED")

# vẽ biểu đồ theo tuổi và giới
# tính với dodge

P = ggplot(data=pisa,
aes(x=PARED, fill=AREA) )

P-= P+
geom_bar(position="dodge", aes(y
= (..count..)/sum(..count..)))

p + x1lab("PARED")

# Biểu đồ hộp: điểm toán theo PARED

# install.packages ( "ggthemes")

library (ggthemes)
fill = "#4271AR"
line = "Z1F3552"

P = ggplot(pisa, aes(x=as.factor(PARED), ÿy=PV1MATH) )
P = P + geom_boxplot(fill1=fill, colour=line)

p + theme economist()

# Biểu đồ hộp: điểm toán theo PARED và giới tính

P = ggplot(pisa, aes(x=REGION, y=PV1SCIE, fill=Œender) )
P = p + geom_boxplot(aes(fill=Gender), alphaz=l)

P = p + geom_jitter(aes(color=Gender, outlier.colour="red"”), size=l.5,
alpha=.2)

pP = p + theme(axis.title.x = element text(color="blue"”, size=10,
tface="bold"), axis.text.x element text(angle=45, vJust=0.5,
size=10))

p + theme economist()


# dont know
P = ggplot(pisa, aes(x=AREA, y=PV1READ, f1ll=Gender) )
P = p + geom_boxplot(aes(fill=Gender), alphaz=l)

P = D + geom_31tter(aes(color=Gender, outlier.colour="red"), size=l.5,
alpha=.2)

P = p + theme(axis.title.x = element text(color="blue"”, size=10,
face="bold"), axis.text.x element text(angle=45, vj]ust=0.5,
size=10))

p + facet .wrap(~Gender) + theme _economist ()


# Điểm môn PV1SCIE theo WEALTH và giới tính

library(gridExtra)

P = ggplot(pisa, aes(x=WEALTH, y=PV1SCIE, color=Gender) )

Pl = p + geom_point()

p2 = p + geom_point() + geom_smooth(method="1lm")

grld.arrange(pl, p2, nrow=2)


# Đến phiên bạn ... tỉ trọng mỡ theo bmi và giới tính

# ob = read.csv("~/Dropbox/_Conferences and Workshops/Da Nang 2017/ Datasets/obesity data.csv")

Ob = read.csv(flle.choose())

P = ggplot(ob, aes(x=bml, y=zpcfat, fill=gender, col=gender))
P = D + geom_point() + geom_smooth(method="lm"”, formula=y~x+I(x^2))

p + xlab("Body mass index”") + ylab("Percent body fat")

# Đến phiên bạn ... phân tích phiếu tín nhiệm

# tn = read.csv("~/Dropbox/_Conferences and Workshops/Da Nang 2017/Datasets/Lay phieu tin nhiem.csv")

tn = read.csv(flle.choose())

= ggplot(tn, aes(x=Year2013, ÿy=Year2014, col=Group) )
= p + geom_polnt(shape=l6, size=2)
P + geom_text(aes(label=Name), size=5)

= p + geom_abline(slope=l, size=0.5, lty=2)
+ theme_bvw()

