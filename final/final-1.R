# Đọc file text igf.txt
igfdata = read.table("igf.txt", header = TRUE, na.string=".")
attach(igfdata)

# Kiểm tra xem có phải là một data.frame
is.data.frame(igfdata)

dim(igfdata)
names(igfdata)

# Đọc vào 1 dataframe
data = data.frame(id,age,weight,height)

#Giá trị trung bình
mean(age)

# Phương sai
var(age)

# Độ lệch chuẩn
sd(age)

# Giá trị nhỏ nhất
min(age)

# Giá trị cao nhất
max(age)

# Miền giá trị 
range(age)

# Hiển thị toàn bộ thông tin chung về biến age trong dữ liệu
summary(age)

# Tóm tắt dữ liệu igfdata theo sex
by(igfdata, sex, summary)

# vẽ đồ thị cho 6 biến số: igfi, igfbp3, als, pinp, ictp, p3np
op = par(mfrow=c(2,3))
hist(igfi)
hist(igfbp3)
hist(als)
hist(pinp)
hist(ictp)
hist(p3np)

# Kiểm định t một mẫu
mean(age)
t.test(age, mu=30)

# Kiểm định t hai mẫu
mean(igfi)

data2 = data.frame(igfi,sex)
print(data2)
giaTriTB = tapply(data2$igfi, data2$sex, mean)
print(giaTriTB)

by(igfi, sex, mean)
t.test(igfi~ sex)

# Kiểm tra biến igfi có phân phối chuẩn không?
shapiro.test(igfi)
hist(igfi)

# Kiểm định t cho các biến số theo cặp paired t-test
# nhập dữ kiện
before = c(180, 140, 160, 160, 220, 185, 145, 160, 160, 170)
after = c(170, 145, 145, 125, 205, 185, 150, 150, 145, 155)
bp = data.frame(before, after)

# kiểm định t
t.test(before, after, paired=TRUE)

# Kiểm định Wilcoxon cho các biến số theo cặp (wilcox.test)
wilcox.test(before, after, paired=TRUE)

# Viết hàm tính sai số chuẩn
desc = function(x)
{
    av = mean(x)
    sd = sd(x)
    se = sd/sqrt(length(x))
    c(MEAN=av, SD=sd, SE=se)
}
desc(als)
