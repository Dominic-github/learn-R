getwd()
x1 = c(1,2,3,4)
x2 = c(4,6,8,10)

# data = data.frame(x1,x2)
# data$sum = data$x1 + data$x2
# data$mean = data$sum / 2




# gender = c("male","female","male","female","female")
# id = c(1,2,3,4,5)
# data =data.frame(id,gender)
# data$sex[gender == "male"] = 1
# data$sex[gender == "female"] = 2
# data$group[id >=1 & id <= 3] = "A"
# data$group[id > 3 & id <= 5] = "B"
# mean(id)
# mean(group)




id = c(1:10)
name = c("A","B","C","D","E","F","G","H","I","J")
x = c(12,45,67,32,26,86,11,16,25,37)

data = data.frame(id,name,x)

# data[,1]
#
# data[1:5,2:3]


new.data = data[order(x),]

# new.data

attach(chol)

chol.new= na.omit(chol)

name.data = subset(chol,sex == "Nam)"

# Tao data moi chi lay cot 1 3 7
data2 = chol[,c(1,3,7)]

# Lay du lien tu 1 den 25
data2 = chol[1:25,c(1,3,7)]




d1
d2


d3 = merge(d1,d2,by="id",all=true)
d3 = merge(d1,d2,by="id")

diagnosis = bmd
diagnosis = replace(diagnosis, bmd <=-2.5, 1)
diagnosis = replace(diagnosis, bmd >-2.5 & bmd <= 1.0, 2)
diagnosis = replace(diagnosis, bmd > 1, 3)
data = data.frame(bmd, diagnosis)


library(Hmisc)
bmd = c(-0.92, 0.21, 0.17, -3.21, -1.80, -2.60, -2.00, 1.71, 2.12, -2.11)
group = cut2(bmd, g=2)
table(group)

group = cut2(bmd,g=4)


d = merge(d1,d2,by="id", all=TRUE)







