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

# Trong nghiên cứu trên, chúng ta thấy có 69 nữ và 31 nam. Như vậy tỉ lệ nữ là 0.69 (hay 69%). Để
# kiểm định xem tỉ lệ này có thật sự khác với tỉ lệ 0.5 hay không? 
prop.test(69, 100, 0.50)
