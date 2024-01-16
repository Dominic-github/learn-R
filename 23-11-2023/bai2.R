# Đọc file text igf.txt
igfdata = read.table("igf.txt", header = TRUE, na.string=".")
attach(igfdata)

# Kiểm tra xem có phải là một data.frame
is.data.frame(igfdata)
dim(igfdata)
names(igfdata)

# Hiển thị tần số của biến số phân loại
table(sex)
table(ethnicity)

# Hiển thị bảng thống kê 2 chiều cho 2 biến: sex, ethnicity.
table(sex, ethnicity)

# Hiển thị số lượng và tỉ lệ người là nam (Male) trong dataframe đang xét
quantity_male = sum(sex == "Male")

# 
prop.test(69, 100, 0.50)
