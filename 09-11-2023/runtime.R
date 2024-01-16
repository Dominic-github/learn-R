pisa = read.csv("./PISA VN 2015.csv")
attach(pisa)

library(DescTools)
library(rms)
library(ggplot2)
library(gridExtra)

pisa = read.csv("PISA VN 2015.csv", header=TRUE)
attach(pisa)

#PV1READ (numeric)
Desc(PV1READ) 

#REGION (character)
Desc(REGION) 

#Mô tả điểm môn khoa học theo vùng
Desc(PV1SCIE~REGION)

#Mô tả theo vùng và khu 
Desc(REGION~AREA)

#MQH giữa điểm đọc và điểm khoa học
#mỗi hs được biểu diễn bằng 1 dot
Desc(PV1READ~PV1SCIE)

#Mối tương quan giữa nhiều biến
b = pisa[, c(19,20,22,25,33,34,35)]
m = cor(b)
PlotCorr(m, border="grey")
PlotWeb(m, col=c(hred, hblue), las=1)

#Số học sinh phân bố theo vùng
Desc(REGION~AREA)